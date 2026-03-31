# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_etl.R
# OBJETIVO: Extracción y transformación (ETL) genérica de insumos operativos
# AUTOR:    Rafael López / Equipo de Análisis
# FECHA:    2026-03-26
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno / Core
# -------------------------------------------------------------------------
# NOTAS DE SEGURIDAD:
# - Este script es AGNÓSTICO a las reglas de negocio de los estados.
# - Las reglas geográficas o temporales específicas deben inyectarse
#   mediante el parámetro `postprocess_insumos` (Callback Hook).
# -------------------------------------------------------------------------

# ---- 1) Helpers: Catálogos Estándar (No exportados) ---------------------

cargar_usuarios_cat <- function(pool, id_proyecto) {
  dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(
      IdProyecto == !!id_proyecto,
      Capacitacion == TRUE | Cargo == "Coordinador de Brigada"
    ) |>
    dplyr::select(
      Id,
      Num,
      Cargo,
      Status,
      Municipio,
      Nombre,
      APaterno,
      AMaterno,
      IdBrigada
    ) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(
      nombre:a_materno,
      ~ tidyr::replace_na(.x, "")
    )) |>
    dplyr::transmute(
      id_usuario = id,
      num = as.character(num),
      cargo = cargo,
      status = status,
      municipio_usuario = municipio,
      nombre_completo = toupper(stringr::str_squish(paste(
        nombre,
        a_paterno,
        a_materno
      ))),
      id_brigada
    ) |>
    dplyr::distinct(id_usuario, .keep_all = TRUE)
}

cargar_brigadas_cat <- function(pool, id_proyecto) {
  dplyr::tbl(pool, "Brigadas") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::select(Id, NombreBrigada, Activo, IdZonaDeTrabajo, IdUsuario) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::transmute(
      id_brigada = id,
      nombre_brigada = gsub("_", " ", nombre_brigada),
      activo_brigada = activo,
      id_zona_trabajo_brigada = id_zona_de_trabajo,
      id_usuario_brigada = id_usuario
    ) |>
    dplyr::distinct(id_brigada, .keep_all = TRUE)
}

cargar_municipios_cat <- function(pool) {
  dplyr::tbl(pool, "Municipios") |>
    dplyr::select(Id, Municipio) |>
    dplyr::collect() |>
    dplyr::transmute(
      id_municipio = Id,
      municipio_log = Municipio
    )
}

cargar_usuario_log <- function(pool, id_proyecto) {
  tbl(pool, "UsuarioLog") |>
    filter(IdProyecto == !!id_proyecto) |>
    select(
      IdHistorico,
      IdUsuario,
      IdCargo,
      IdEstado,
      IdMunicipio,
      IdZonaDeTabajo,
      IdSupervisor,
      IdBrigada,
      FechaInsert
    ) |>
    collect() |>
    mutate(
      ts_evento = lubridate::as_datetime(lubridate::with_tz(
        FechaInsert,
        tzone = "America/Mexico_City"
      )),
      fecha_evento = lubridate::as_date(ts_evento)
    )
}

# ---- 2) Loaders y Constructores Base (No exportados) --------------------

cargar_actividad <- function(
  pool,
  fuentes,
  fecha_min = NULL,
  fecha_max = NULL,
  filtro_minimo = NULL,
  normalizador = NULL
) {
  purrr::map_dfr(fuentes, function(f) {
    q <- dplyr::tbl(pool, f$tabla)

    if (!is.null(filtro_minimo)) {
      q <- filtro_minimo(q, f)
    }
    if (!is.null(f$select_cols)) {
      q <- q |> dplyr::select(dplyr::any_of(f$select_cols))
    }
    if (!is.null(fecha_min)) {
      q <- q |> dplyr::filter(fecha >= !!as.Date(fecha_min))
    }
    if (!is.null(fecha_max)) {
      q <- q |> dplyr::filter(fecha <= !!as.Date(fecha_max))
    }

    df <- q |>
      dplyr::collect() |>
      dplyr::mutate(
        fecha = as.Date(fecha),
        usuario_num = as.character(usuario_num),
        seccion = sprintf("%04s", seccion),
        origen_datos = dplyr::coalesce(f$origen, f$tabla)
      )

    if (!is.null(normalizador)) {
      df <- normalizador(df, f)
    }
    df
  })
}

resolver_estructura_corte <- function(usuario_log, corte) {
  usuario_log |>
    filter(fecha_evento <= as.Date(corte)) |>
    arrange(IdUsuario, IdHistorico) |>
    group_by(IdUsuario) |>
    slice_tail(n = 1) |>
    ungroup() |>
    transmute(
      id_usuario = IdUsuario,
      id_cargo = IdCargo,
      id_estado = IdEstado,
      id_municipio = IdMunicipio,
      id_zona = IdZonaDeTabajo,
      id_supervisor = IdSupervisor,
      id_brigada = IdBrigada,
      ts_evento,
      fecha_evento
    )
}

construir_bd_aux <- function(
  estructura_corte,
  usuarios_cat,
  brigadas_cat,
  municipios_cat
) {
  estructura_corte |>
    dplyr::left_join(municipios_cat, by = "id_municipio") |>
    dplyr::left_join(brigadas_cat, by = "id_brigada") |>
    dplyr::left_join(
      usuarios_cat |>
        dplyr::transmute(
          id_usuario,
          vocero = num,
          nombre_vocero = nombre_completo,
          status_vocero = status
        ),
      by = "id_usuario"
    ) |>
    dplyr::left_join(
      usuarios_cat |>
        dplyr::transmute(
          id_supervisor = id_usuario,
          supervisor = num,
          nombre_coordinador = nombre_completo,
          status_coord = status
        ),
      by = "id_supervisor"
    ) |>
    dplyr::transmute(
      distrito = NA_character_,
      municipio = municipio_log,
      nombre_brigada,
      nombre_coordinador,
      supervisor,
      status_coord,
      nombre_vocero,
      vocero,
      status_vocero
    )
}

cargar_pases_lista <- function(pool, ids_cuestionario, procesador_pl) {
  purrr::map(
    ids_cuestionario,
    ~ procesador_pl(pool = pool, id_cuestionario = .x)
  ) |>
    rlang::set_names(paste0("pl_", ids_cuestionario))
}

# ---- 3) Orquestador Principal (Exportado) -------------------------------

#' Cargar Insumos Operativos (ETL Central)
#'
#' @description
#' Orquesta la descarga y consolidación de datos desde la base de datos SQL.
#' Reconstruye la jerarquía operativa al corte (bd_aux), descarga las sábanas
#' de actividad y procesa múltiples cuestionarios de pase de lista.
#'
#' @param pool Conexión activa a la base de datos (DBI).
#' @param id_proyecto Numeric. ID del proyecto a extraer.
#' @param corte Date. Fecha límite de análisis.
#' @param fuentes_actividad List. Lista de tablas origen y sus metadatos.
#' @param ids_pase_lista Numeric vector. IDs de los cuestionarios de pase de lista.
#' @param procesador_pl Function. Función para parsear el JSON del pase de lista.
#' @param fecha_min_actividad Date. Límite inferior de la extracción de actividad.
#' @param filtro_minimo_actividad Function. Filtros preliminares en SQL.
#' @param normalizador_actividad Function. Ajustes de tipos posteriores a la descarga.
#' @param postprocess_insumos Function. Hook (Callback) opcional para inyectar reglas de negocio específicas del proyecto al objeto final.
#'
#' @return Una lista estructurada (`insumos`) con los dataframes listos para reporteo.
#' @importFrom lubridate as_datetime with_tz as_date
#' @export
cargar_insumos <- function(
  pool,
  id_proyecto,
  corte,
  fuentes_actividad,
  ids_pase_lista = integer(),
  procesador_pl = NULL,
  fecha_min_actividad = NULL,
  filtro_minimo_actividad = NULL,
  normalizador_actividad = NULL,
  postprocess_insumos = NULL
) {
  usuarios_cat <- cargar_usuarios_cat(pool, id_proyecto)
  brigadas_cat <- cargar_brigadas_cat(pool, id_proyecto)
  municipios_cat <- cargar_municipios_cat(pool)
  usuario_log <- cargar_usuario_log(pool, id_proyecto)

  bd_actividad <- cargar_actividad(
    pool = pool,
    fuentes = fuentes_actividad,
    fecha_min = fecha_min_actividad,
    fecha_max = corte,
    filtro_minimo = filtro_minimo_actividad,
    normalizador = normalizador_actividad
  )

  estructura_corte <- resolver_estructura_corte(usuario_log, corte)

  bd_aux <- construir_bd_aux(
    estructura_corte,
    usuarios_cat,
    brigadas_cat,
    municipios_cat
  )

  pase_lista <- list()
  if (length(ids_pase_lista) > 0) {
    if (is.null(procesador_pl)) {
      cli::cli_abort("Se requiere 'procesador_pl' para cargar pases de lista.")
    }
    pase_lista <- cargar_pases_lista(
      pool,
      ids_cuestionario = ids_pase_lista,
      procesador_pl = procesador_pl
    )
  }

  insumos <- list(
    id_proyecto = id_proyecto,
    corte = as.Date(corte),
    bd_actividad = bd_actividad,
    bd_aux = bd_aux,
    pase_lista = pase_lista,
    cat = list(
      usuarios = usuarios_cat,
      brigadas = brigadas_cat,
      municipios = municipios_cat,
      usuario_log = usuario_log
    )
  )

  # Ejecutar el Hook de inyección de reglas de negocio
  if (!is.null(postprocess_insumos)) {
    insumos <- postprocess_insumos(insumos)
  }

  return(insumos)
}

#' Procesa pases de lista desde `Registros` y los normaliza a nivel coordinador–usuario
#'
#' @description
#' `procesar_pase_lista_largo()` extrae registros de la tabla `Registros` para un cuestionario dado,
#' parsea el JSON contenido en `Resultado` (asumiendo estructura **plana** clave–valor),
#' y lo transforma a un formato tabular que preserva la relación:
#' **(id de registro, fecha de pase, coordinador, usuario reportado, variables del pase)**.
#'
#' La función está diseñada para minimizar tiempo y memoria al **evitar** construir un
#' `data.frame` ancho con miles de columnas y luego pivotarlo. En su lugar, primero
#' construye una tabla **larga** (`name`, `value`) y sólo al final castea a formato ancho
#' por usuario (`dcast`), lo que reduce copias y asignaciones masivas.
#'
#' @details
#' ## Fuentes y supuestos
#' - Fuente: tabla SQL `Registros` vía `pool` (DBI/dbplyr).
#' - Se consulta únicamente: `Id`, `FechaInicio`, `Resultado` (mínimo indispensable).
#' - `Resultado` debe ser un JSON **plano** (no anidado) del tipo `{ "key": "value", ... }`.
#'   Si el JSON no es plano, la función sigue intentando procesar mediante el mismo
#'   mecanismo; sin embargo, estructuras anidadas pueden producir columnas/valores no esperados.
#'
#' ## Convenciones de negocio (decisiones explícitas)
#' La función implementa reglas de negocio para interpretar el contenido del JSON:
#'
#' 1. **Definición de fecha de pase**
#'    - Se usa `FechaInicio` como la fecha oficial del pase.
#'    - Se convierte a `Date` (sin hora) y se guarda como `fecha`.
#'    - Se filtra por `fecha >= fecha_min`.
#'
#' 2. **Definición de coordinador**
#'    - El coordinador se toma exclusivamente del campo JSON `Obtener_usuario`.
#'    - Se normaliza con `toupper()` y `str_squish()` para homogenizar (evita discrepancias por espacios).
#'    - Si un registro **no** contiene `Obtener_usuario` o está vacío, el registro se considera
#'      "sin coordinador" y **no se incluye** en la salida principal (porque no es posible vincularlo).
#'
#' 3. **Definición de usuario reportado**
#'    - Las variables por usuario deben venir con el patrón: `<variable>_<usuario>`,
#'      donde `<usuario>` es un sufijo numérico al final del nombre (por ejemplo:
#'      `asistencia_6351142907`, `viviendas_6351142907`).
#'    - El usuario se extrae con la expresión regular `(?<=_)[0-9]+$`.
#'    - Si una clave no termina en `_<digits>`, se considera variable **global** del registro
#'      (no asociada a un usuario específico).
#'
#' 4. **Variables globales preservadas**
#'    - Se preservan explícitamente las variables globales:
#'      - `observaciones`
#'      - `finalizar`
#'    - Estas variables se **replican** a todas las filas de usuarios del mismo `id`/`fecha`/`coordinador`.
#'
#' 5. **Múltiples pases de lista del mismo coordinador en el mismo día**
#'    - **No se colapsan** en un único registro.
#'    - Cada pase corresponde a un `id` distinto y produce filas separadas.
#'    - La llave natural de la salida es:
#'      `id + fecha + obtener_usuario + usuario`.
#'    - Implicación: si un coordinador registra dos pases el mismo día, aparecerán dos "bloques"
#'      de usuarios (uno por `id`). Esto preserva auditoría y trazabilidad.
#'    - Si posteriormente el negocio requiere un "único pase por día", esa deduplicación debe
#'      hacerse aguas abajo (por ejemplo, eligiendo el `id` más reciente por `FechaInicio`
#'      o un criterio de completitud). Esta función **no decide** cuál es el "bueno";
#'      sólo preserva todos.
#'
#' 6. **Claves duplicadas dentro de un mismo pase (misma combinación id/fecha/coordinador/usuario/variable)**
#'    - Durante el casteo ancho (`dcast`), si existen múltiples valores para la misma variable,
#'      se aplica la regla:
#'      **tomar el primer valor no-NA** en el orden de aparición.
#'    - Motivación: evitar columnas con listas y mantener una tabla rectangular.
#'
#' 7. **Registros con JSON inválido**
#'    - Si `fromJSON()` falla para un registro, ese registro se omite (retorna `NULL` en el lapply).
#'    - Motivación: robustez ante corrupción de datos sin abortar todo el proceso.
#'
#' ## Rendimiento y memoria
#' - Estrategia: "long-first" (tabla larga) + `data.table::dcast` al final.
#' - Reduce asignación masiva vs. construir un ancho con `rbindlist(fill = TRUE)` sobre miles de columnas.
#'
#' @param pool Conexión/pool DBI (por ejemplo `pool::dbPool()`), compatible con `dplyr::tbl()`.
#' @param id_cuestionario Identificador del cuestionario (`EncuestaId`) a filtrar en `Registros`.
#' @param fecha_min Fecha mínima (inclusive) para considerar registros, en clase `Date`.
#'   Por defecto `as.Date("2025-03-03")`.
#' @param vars_globales Character vector de variables del JSON que se consideran globales del registro
#'   y deben replicarse por usuario. Por defecto `c("observaciones", "finalizar")`.
#'
#' @return
#' Un `data.table` en formato ancho a nivel usuario, con columnas mínimas:
#' - `id`: Id del registro en `Registros`
#' - `fecha`: `Date` derivada de `FechaInicio`
#' - `obtener_usuario`: coordinador (normalizado)
#' - `usuario`: usuario reportado (extraído del sufijo numérico de las claves)
#' - columnas adicionales: una por cada variable encontrada en el JSON (prefijo antes del sufijo numérico),
#'   más `vars_globales` si existen.
#'
#' @examples
#' \dontrun{
#' # Procesar todos los pases del cuestionario 210 desde 2025-03-03
#' pl <- procesar_pase_lista_largo(pool, id_cuestionario = 210)
#'
#' # Ver cuántos pases por coordinador y fecha (auditoría: múltiples pases/día)
#' library(data.table)
#' setDT(pl)
#' pl[, .N, by = .(obtener_usuario, fecha, id)][order(obtener_usuario, fecha)]
#' }
#'
#' @importFrom dplyr filter select mutate collect tbl
#' @importFrom jsonlite fromJSON
#' @importFrom data.table rbindlist data.table dcast
#' @importFrom stringr str_extract str_squish
#' @importFrom lubridate as_date
#' @export
procesar_pase_lista <- function(
  pool,
  id_cuestionario,
  fecha_min = as.Date("2026-02-16"),
  vars_globales = c("observaciones", "finalizar")
) {
  # 1) Extracción mínima y filtro temprano por fecha (decisión de negocio/performance)
  raw <- tbl(pool, "Registros") |>
    filter(EncuestaId == !!id_cuestionario) |>
    select(Id, FechaInicio, Resultado) |>
    collect() |>
    mutate(
      fecha = as_date(lubridate::with_tz(
        FechaInicio,
        tzone = "America/Mexico_City"
      ))
    ) |>
    filter(fecha >= !!fecha_min) |>
    select(Id, FechaInicio, fecha, Resultado)

  if (nrow(raw) == 0) {
    return(data.table())
  }

  # 2) Parsing a largo (name/value) en streaming
  dt_long <- data.table::rbindlist(
    lapply(seq_len(nrow(raw)), function(i) {
      kv <- tryCatch(
        jsonlite::fromJSON(raw$Resultado[[i]], simplifyVector = TRUE),
        error = function(e) NULL
      )
      if (is.null(kv)) {
        return(NULL)
      }

      data.table::data.table(
        id = raw$Id[[i]],
        fecha = raw$fecha[[i]],
        name = names(kv),
        value = as.character(unlist(kv, use.names = FALSE))
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(dt_long) == 0) {
    return(data.table())
  }

  # 3) Coordinador desde JSON: Obtener_usuario (decisión de negocio)
  coord <- dt_long[
    name == "Obtener_usuario",
    .(obtener_usuario = value[1]),
    by = .(id, fecha)
  ]
  coord[, obtener_usuario := toupper(str_squish(obtener_usuario))]

  dt_long <- merge(dt_long, coord, by = c("id", "fecha"), all.x = TRUE)

  # 4) Separar usuario vs variable global (decisión de negocio: patrón <var>_<usuario>)
  dt_long[, usuario := str_extract(name, "(?<=_)[0-9]+$")]
  dt_long[,
    variable := ifelse(!is.na(usuario), gsub("_[0-9]+$", "", name), name)
  ]

  # 5) Globales y por-usuario
  globales <- dt_long[
    variable %in% vars_globales,
    .(id, fecha, obtener_usuario, variable, value)
  ]

  por_usuario <- dt_long[
    !is.na(usuario) &
      !is.na(obtener_usuario) &
      obtener_usuario != "",
    .(id, fecha, obtener_usuario, usuario, variable, value)
  ]

  # 6) Wide final: una fila por id-fecha-coordinador-usuario
  # Regla de negocio/técnica: duplicados -> primer valor no NA
  pl <- data.table::dcast(
    por_usuario,
    id + fecha + obtener_usuario + usuario ~ variable,
    value.var = "value",
    fill = NA_character_,
    fun.aggregate = function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) NA_character_ else x[[1]]
    }
  )

  # 7) Adjuntar globales (replicadas) si existen
  if (nrow(globales) > 0) {
    gwide <- data.table::dcast(
      globales,
      id + fecha + obtener_usuario ~ variable,
      value.var = "value",
      fill = NA_character_,
      fun.aggregate = function(x) {
        x <- x[!is.na(x)]
        if (length(x) == 0) NA_character_ else x[[1]]
      }
    )
    pl <- merge(
      pl,
      gwide,
      by = c("id", "fecha", "obtener_usuario"),
      all.x = TRUE
    )
  }

  as_tibble(pl[])
}
