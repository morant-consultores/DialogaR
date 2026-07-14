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

cargar_usuarios_asignados <- function(pool, fuentes_actividad) {
  tablas <- purrr::map_chr(fuentes_actividad, "tabla")
  ids    <- stringr::str_extract(tablas, "(?<=snapshot_id_)\\d+")
  ids    <- ids[!is.na(ids)]
  if (length(ids) == 0) return(integer(0))
  dplyr::tbl(pool, "UsuariosEncuesta") |>
    dplyr::filter(EncuestaId %in% ids, Activo == TRUE) |>
    dplyr::pull(UsuarioId)
}

cargar_usuarios_cat <- function(pool, id_proyecto, usuarios_asignados = NULL, cargo_coordinador = "Coordinador de Brigada") {
  q <- dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(
      IdProyecto == !!id_proyecto,
      Capacitacion == TRUE | Cargo == !!cargo_coordinador
    )
  if (!is.null(usuarios_asignados) && length(usuarios_asignados) > 0) {
    q <- q |> dplyr::filter(Id %in% !!usuarios_asignados | Cargo == !!cargo_coordinador)
  }
  q |>
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

cargar_coordinadores_cat <- function(pool, id_proyecto, cargo_coordinador = "Coordinador de Brigada") {
  dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(
      IdProyecto == !!id_proyecto,
      Cargo == !!cargo_coordinador
    ) |>
    dplyr::select(Id, Num, Nombre, APaterno, AMaterno, Status) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(nombre:a_materno, ~ tidyr::replace_na(.x, ""))) |>
    dplyr::transmute(
      id_supervisor   = id,
      supervisor      = as.character(num),
      nombre_coordinador = toupper(stringr::str_squish(paste(nombre, a_paterno, a_materno))),
      status_coord    = status
    )
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

cargar_zonas_trabajo_cat <- function(pool) {
  dplyr::tbl(pool, "ZonaDeTrabajo") |>
    dplyr::select(IdZona, Descripcion) |>
    dplyr::collect() |>
    dplyr::transmute(
      id_zona_trabajo     = IdZona,
      nombre_zona_trabajo = Descripcion
    ) |>
    dplyr::distinct(id_zona_trabajo, .keep_all = TRUE)
}

cargar_grupos_cat <- function(pool) {
  dplyr::tbl(pool, "Grupos") |>
    dplyr::select(Id, Descripcion) |>
    dplyr::collect() |>
    dplyr::transmute(
      id_grupo     = Id,
      nombre_grupo = Descripcion
    ) |>
    dplyr::distinct(id_grupo, .keep_all = TRUE)
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

cargar_brigada_log <- function(pool, id_proyecto) {
  dplyr::tbl(pool, "BrigadasLog") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::select(
      IdHistorico, BrigadaId, NombreBrigada,
      IdUsuario, IdZonaDeTrabajo, IdGrupo, Activo, FechaInsert
    ) |>
    dplyr::collect() |>
    dplyr::mutate(
      ts_evento    = lubridate::as_datetime(lubridate::with_tz(FechaInsert, tzone = "America/Mexico_City")),
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
    tabla_ref <- if (!is.null(f$schema)) {
      DBI::Id(schema = f$schema, table = f$tabla)
    } else {
      f$tabla
    }
    # Cada fuente puede vivir en un pool distinto (p.ej. snapshot en otro servidor)
    pool_fuente <- f$pool %||% pool
    q <- dplyr::tbl(pool_fuente, tabla_ref)

    if (!is.null(filtro_minimo)) {
      q <- filtro_minimo(q, f)
    }

    # Fechas por-fuente tienen prioridad sobre el global
    f_fecha_min <- f$fecha_min %||% fecha_min
    f_fecha_max <- f$fecha_max %||% fecha_max
    if (!is.null(f_fecha_min)) {
      q <- q |> dplyr::filter(fecha >= !!as.Date(f_fecha_min))
    }
    if (!is.null(f_fecha_max)) {
      q <- q |> dplyr::filter(fecha <= !!as.Date(f_fecha_max))
    }

    if (!is.null(f$select_cols)) {
      q <- q |> dplyr::select(dplyr::any_of(f$select_cols))
    }

    df <- q |> dplyr::collect()
    has_num <- "usuario_num" %in% names(df)
    uid_col <- intersect(c("usuario_id", "UsuarioId"), names(df))[1]
    has_uid <- !is.na(uid_col)
    df <- df |>
      dplyr::mutate(
        fecha        = as.Date(fecha),
        usuario_num  = if (has_num) as.character(usuario_num) else NA_character_,
        id_usuario_actividad = if (has_uid) as.integer(.data[[uid_col]]) else NA_integer_,
        origen_datos = dplyr::coalesce(f$origen, f$tabla)
      )

    # seccion: solo si existe (proyectos con AGEB no la tienen)
    if ("seccion" %in% names(df)) {
      df <- df |> dplyr::mutate(seccion = sprintf("%04s", seccion))
    }

    if (!is.null(normalizador)) {
      df <- normalizador(df, f)
    }
    df
  })
}

resolver_estructura_corte <- function(usuario_log, corte, usuarios_asignados = NULL, coordinadores_ids = NULL) {
  q <- usuario_log |>
    dplyr::filter(fecha_evento <= as.Date(corte))
  if (!is.null(usuarios_asignados) && length(usuarios_asignados) > 0) {
    # Los coordinadores siempre se incluyen aunque no estén en UsuariosEncuesta
    ids_permitidos <- unique(c(usuarios_asignados, coordinadores_ids))
    q <- q |> dplyr::filter(IdUsuario %in% !!ids_permitidos)
  }
  q |>
    arrange(IdUsuario, IdHistorico) |>
    group_by(IdUsuario) |>
    tidyr::fill(IdCargo, IdBrigada, .direction = "down") |>
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

#' Resolver coordinador histórico de cada brigada al corte
#'
#' @description
#' Aplica LOCF sobre `BrigadasLog` para determinar qué coordinador estaba
#' al frente de cada brigada en la fecha de corte, junto con la zona de trabajo
#' y grupo vigentes en ese momento.
#'
#' @param brigada_log Data frame producido por `cargar_brigada_log()`.
#' @param corte Date o character coercible a Date. Fecha límite del análisis.
#'
#' @return Un tibble con una fila por brigada con columnas:
#'   `id_brigada`, `nombre_brigada_log`, `id_coordinador_log`,
#'   `id_zona_trabajo`, `id_grupo`, `activo_brigada`.
#'
#' @export
resolver_coordinador_en_fecha <- function(brigada_log, corte) {
  brigada_log |>
    dplyr::filter(fecha_evento <= as.Date(corte)) |>
    dplyr::arrange(BrigadaId, IdHistorico) |>
    dplyr::group_by(BrigadaId) |>
    tidyr::fill(IdUsuario, IdZonaDeTrabajo, IdGrupo, NombreBrigada, .direction = "down") |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      id_brigada         = BrigadaId,
      nombre_brigada_log = NombreBrigada,
      id_coordinador_log = IdUsuario,
      id_zona_trabajo    = IdZonaDeTrabajo,
      id_grupo           = IdGrupo,
      activo_brigada     = Activo
    )
}

construir_bd_aux <- function(
  estructura_corte,
  usuarios_cat,
  coordinadores_cat,
  brigada_corte,
  municipios_cat,
  brigadas_cat = NULL,
  zonas_cat = NULL,
  grupos_cat = NULL
) {
  bd_base <- estructura_corte |>
    dplyr::left_join(municipios_cat, by = "id_municipio")

  # brigada_corte de resolver_coordinador_en_fecha() (BrigadasLog LOCF).
  # brigadas_cat como fallback cuando brigada_corte es NULL (compatibilidad).
  bd_base <- if (!is.null(brigada_corte)) {
    dplyr::left_join(bd_base, brigada_corte, by = "id_brigada")
  } else {
    dplyr::left_join(bd_base, brigadas_cat, by = "id_brigada") |>
      dplyr::rename(
        nombre_brigada_log = nombre_brigada,
        id_coordinador_log = id_usuario_brigada
      ) |>
      dplyr::mutate(id_zona_trabajo = id_zona_trabajo_brigada, id_grupo = NA_integer_)
  }

  bd <- bd_base |>
    dplyr::left_join(
      usuarios_cat |>
        dplyr::transmute(
          id_usuario,
          vocero        = num,
          nombre_vocero = nombre_completo,
          status_vocero = status
        ),
      by = "id_usuario"
    ) |>
    # Coordinador desde BrigadasLog LOCF al corte (id_coordinador_log).
    dplyr::left_join(
      coordinadores_cat,
      by = c("id_coordinador_log" = "id_supervisor")
    )

  # Nombres de zona de trabajo y grupo (catálogos ZonaDeTrabajo / Grupos).
  # Catálogos opcionales: cuando son NULL, se conservan las columnas de nombre
  # como NA para no romper consumidores que esperan su presencia.
  bd <- if (!is.null(zonas_cat)) {
    dplyr::left_join(bd, zonas_cat, by = "id_zona_trabajo")
  } else {
    dplyr::mutate(bd, nombre_zona_trabajo = NA_character_)
  }
  bd <- if (!is.null(grupos_cat)) {
    dplyr::left_join(bd, grupos_cat, by = "id_grupo")
  } else {
    dplyr::mutate(bd, nombre_grupo = NA_character_)
  }

  # Detectar brigadas con id_coordinador_log que no existe en coordinadores_cat.
  brigadas_coord_invalido <- bd |>
    dplyr::filter(!is.na(id_coordinador_log), is.na(nombre_coordinador), !is.na(vocero)) |>
    dplyr::distinct(id_brigada, nombre_brigada_log, id_coordinador_log)

  if (nrow(brigadas_coord_invalido) > 0) {
    lineas <- brigadas_coord_invalido |>
      dplyr::transmute(msg = sprintf(
        "Brigada '%s' — IdUsuario=%d no encontrado en coordinadores (cargo o estado incorrecto en plataforma)",
        nombre_brigada_log, id_coordinador_log
      )) |>
      dplyr::pull(msg)

    cli::cli_warn(c(
      "!" = sprintf(
        "construir_bd_aux: %d brigada(s) con coordinador en BrigadasLog pero sin resolver.",
        nrow(brigadas_coord_invalido)
      ),
      stats::setNames(lineas, rep("*", length(lineas)))
    ))
  }

  brigadas_unicas <- unique(bd$nombre_brigada_log)
  brigadas_unicas <- brigadas_unicas[!is.na(brigadas_unicas)]
  usar_distrito   <- length(brigadas_unicas) > 0L &&
    any(grepl("^[0-9]{2}", brigadas_unicas))

  bd |>
    dplyr::transmute(
      distrito = if (usar_distrito) {
        stringr::str_extract(nombre_brigada_log, "^[0-9]{2}")
      } else {
        NA_character_
      },
      municipio          = municipio_log,
      nombre_brigada     = nombre_brigada_log,
      nombre_zona_trabajo,
      nombre_grupo,
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

#' Resolver brigada histórica al momento de cada registro de actividad
#'
#' @description
#' Enriquece un dataframe de actividad con `id_brigada` resuelto **al momento
#' de cada registro**, en lugar de usar la asignación actual del usuario.
#'
#' Para cada par `(usuario_num, fecha)` en `actividad`, busca en `usuario_log`
#' la última asignación de brigada cuya `fecha_evento <= fecha` (LOCF). Esto
#' permite trazar correctamente cuando un vocero trabajó en distintas brigadas
#' en períodos diferentes.
#'
#' Si `actividad` ya contiene una columna `id_brigada`, es reemplazada.
#'
#' @param actividad Data frame con al menos las columnas `usuario_num` (character)
#'   y `fecha` (Date).
#' @param usuario_log Data frame producido por `cargar_usuario_log()`;
#'   disponible en `insumos$cat$usuario_log`.
#' @param usuarios_cat Data frame producido por `cargar_usuarios_cat()`;
#'   disponible en `insumos$cat$usuarios`. Mapea `num` → `id_usuario`.
#'   Solo se usa si `num_map` es `NULL`.
#' @param num_map Data frame opcional con columnas `id_usuario` (integer) y
#'   `num` (character) que cubre **todos** los usuarios del proyecto, incluyendo
#'   los dados de baja en `UsuariosEncuesta`. Disponible en
#'   `insumos$cat$num_map`. Si se omite, se deriva de `usuarios_cat` (comportamiento
#'   anterior, que excluye usuarios con `Activo = FALSE`).
#'
#' @return `actividad` con la columna `id_brigada` (integer) resuelta
#'   históricamente. Filas sin asignación conocida tendrán `NA`.
#'
#' @importFrom tidyr fill
#' @export
resolver_brigada_en_fecha <- function(actividad, usuario_log, usuarios_cat, num_map = NULL) {
  # Tabla histórica: id_usuario + fecha_evento → id_brigada (LOCF)
  brigada_hist_id <- usuario_log |>
    dplyr::arrange(IdUsuario, IdHistorico) |>
    dplyr::group_by(IdUsuario) |>
    tidyr::fill(IdBrigada, .direction = "down") |>
    dplyr::group_by(IdUsuario, fecha_evento) |>
    dplyr::slice_tail(n = 1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      id_usuario   = IdUsuario,
      fecha_evento,
      id_brigada   = IdBrigada
    ) |>
    dplyr::arrange(id_usuario, fecha_evento, dplyr::desc(!is.na(id_brigada))) |>
    dplyr::distinct(id_usuario, fecha_evento, .keep_all = TRUE)

  # Ruta directa: actividad tiene id_usuario_actividad (FK int a Usuarios.Id)
  # Requiere ALL no-NA para garantizar que no haya rows sin resolver (hasta que
  # el snapshot tenga UsuarioId completo con backpropagación)
  hay_col_id      <- "id_usuario_actividad" %in% names(actividad)
  n_id_na         <- if (hay_col_id) sum(is.na(actividad$id_usuario_actividad)) else 0L
  tiene_id_usuario <- hay_col_id && n_id_na == 0L

  # La caída a la ruta legacy por NAs es global (un solo NA la dispara): hacerla
  # observable para no diagnosticar a ciegas un snapshot parcialmente poblado.
  if (hay_col_id && !tiene_id_usuario) {
    cli::cli_warn(c(
      "!" = "resolver_brigada_en_fecha: {n_id_na} de {nrow(actividad)} fila(s) sin {.field id_usuario_actividad}.",
      "i" = "Se usa la ruta legacy por {.field usuario_num} para TODAS las filas."
    ))
  }

  if (tiene_id_usuario) {
    return(
      actividad |>
        dplyr::select(-dplyr::any_of("id_brigada")) |>
        dplyr::mutate(.row = dplyr::row_number()) |>
        dplyr::left_join(
          brigada_hist_id,
          dplyr::join_by(id_usuario_actividad == id_usuario, fecha >= fecha_evento)
        ) |>
        dplyr::group_by(.row) |>
        dplyr::slice_max(fecha_evento, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::select(-.row, -fecha_evento)
    )
  }

  # Ruta legacy: actividad tiene usuario_num (string), requiere num_map
  if (is.null(num_map)) {
    num_map <- usuarios_cat |>
      dplyr::select(id_usuario, num)
  }

  brigada_hist_num <- brigada_hist_id |>
    dplyr::inner_join(num_map, by = "id_usuario") |>
    dplyr::select(num, fecha_evento, id_brigada) |>
    dplyr::arrange(num, fecha_evento, dplyr::desc(!is.na(id_brigada))) |>
    dplyr::distinct(num, fecha_evento, .keep_all = TRUE)

  actividad |>
    dplyr::select(-dplyr::any_of("id_brigada")) |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::left_join(
      brigada_hist_num,
      dplyr::join_by(usuario_num == num, fecha >= fecha_evento)
    ) |>
    dplyr::group_by(.row) |>
    dplyr::slice_max(fecha_evento, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(-.row, -fecha_evento)
}

# Excluir brigadas de prueba de los insumos compartidos (bd_aux y bd_actividad),
# de modo que no aparezcan en ningún reporte. Si una brigada de prueba tiene
# actividad registrada (diálogos), se emite una advertencia clara antes de
# excluirla — registrar diálogos contra una brigada de prueba es un error de
# captura que no debe pasar inadvertido.
excluir_brigadas_prueba <- function(bd_aux, bd_actividad, patron = "prueba") {
  if (is.null(bd_aux) || !"nombre_brigada" %in% names(bd_aux)) {
    return(list(bd_aux = bd_aux, bd_actividad = bd_actividad))
  }

  es_prueba <- grepl(patron, bd_aux$nombre_brigada, ignore.case = TRUE)
  es_prueba[is.na(es_prueba)] <- FALSE
  if (!any(es_prueba)) {
    return(list(bd_aux = bd_aux, bd_actividad = bd_actividad))
  }

  brig_prueba <- bd_aux[es_prueba, , drop = FALSE]
  num_brigada <- dplyr::bind_rows(
    dplyr::transmute(brig_prueba, usuario_num = vocero, nombre_brigada),
    dplyr::transmute(brig_prueba, usuario_num = supervisor, nombre_brigada)
  ) |>
    dplyr::filter(!is.na(usuario_num), usuario_num != "-") |>
    dplyr::distinct(usuario_num, .keep_all = TRUE)

  # Flag: actividad (diálogos) registrada contra brigadas de prueba.
  if (!is.null(bd_actividad) && "usuario_num" %in% names(bd_actividad)) {
    act_prueba <- bd_actividad |>
      dplyr::inner_join(num_brigada, by = "usuario_num")

    if (nrow(act_prueba) > 0) {
      tiene_desglose <- "desglose" %in% names(act_prueba)
      resumen <- act_prueba |>
        dplyr::group_by(nombre_brigada) |>
        dplyr::summarise(
          registros = dplyr::n(),
          dialogos  = if (tiene_desglose) sum(desglose == "Efectivo", na.rm = TRUE) else NA_integer_,
          .groups   = "drop"
        )
      lineas <- resumen |>
        dplyr::transmute(msg = sprintf(
          "Brigada '%s': %d registro(s) de actividad%s — EXCLUIDA de los reportes",
          nombre_brigada, registros,
          if (tiene_desglose) sprintf(", %d diálogo(s) efectivo(s)", dialogos) else ""
        )) |>
        dplyr::pull(msg)

      cli::cli_warn(c(
        "!" = sprintf(
          "%d brigada(s) de prueba con actividad registrada fueron excluidas de los reportes.",
          nrow(resumen)
        ),
        " " = "Revisar por qué se registraron diálogos contra una brigada de prueba.",
        stats::setNames(lineas, rep("*", length(lineas)))
      ))
    }

    bd_actividad <- bd_actividad |>
      dplyr::anti_join(num_brigada, by = "usuario_num")
  }

  list(bd_aux = bd_aux[!es_prueba, , drop = FALSE], bd_actividad = bd_actividad)
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
#'   Cada elemento admite un campo opcional `pool` con una conexión DBI propia
#'   (por ejemplo, cuando la fuente vive en un servidor distinto al de `pool`,
#'   como un snapshot); si se omite, se usa el `pool` principal.
#' @param ids_pase_lista Numeric vector. IDs de los cuestionarios de pase de lista.
#' @param procesador_pl Function. Función para parsear el JSON del pase de lista.
#' @param fecha_min_actividad Date. Límite inferior de la extracción de actividad.
#' @param filtro_minimo_actividad Function. Filtros preliminares en SQL.
#' @param normalizador_actividad Function. Ajustes de tipos posteriores a la descarga.
#' @param postprocess_insumos Function. Hook (Callback) opcional para inyectar reglas de negocio específicas del proyecto al objeto final.
#' @param cargo_coordinador Character. Valor exacto del campo `Cargo` en la tabla
#'   `Usuarios` que identifica a los coordinadores de brigada. Varía por proyecto
#'   (ej. `"Coordinador de Brigada"`, `"Coordinador"`, `"Supervisor"`).
#'   Por defecto `"Coordinador de Brigada"`.
#' @param excluir_pruebas Logical. Si `TRUE` (por defecto), excluye de `bd_aux` y
#'   `bd_actividad` las brigadas cuyo nombre coincide con `patron_pruebas`, de modo
#'   que no aparezcan en ningún reporte. Si alguna brigada de prueba tiene actividad
#'   (diálogos) registrada, se emite una advertencia clara antes de excluirla.
#' @param patron_pruebas Character. Expresión regular (case-insensitive) para
#'   identificar brigadas de prueba por `nombre_brigada`. Por defecto `"prueba"`.
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
  postprocess_insumos = NULL,
  cargo_coordinador = "Coordinador de Brigada",
  excluir_pruebas = TRUE,
  patron_pruebas = "prueba"
) {
  usuarios_asignados  <- cargar_usuarios_asignados(pool, fuentes_actividad)
  usuarios_cat        <- cargar_usuarios_cat(pool, id_proyecto, usuarios_asignados, cargo_coordinador)
  coordinadores_cat   <- cargar_coordinadores_cat(pool, id_proyecto, cargo_coordinador)
  brigadas_cat        <- cargar_brigadas_cat(pool, id_proyecto)
  municipios_cat      <- cargar_municipios_cat(pool)
  zonas_cat           <- cargar_zonas_trabajo_cat(pool)
  grupos_cat          <- cargar_grupos_cat(pool)
  usuario_log         <- cargar_usuario_log(pool, id_proyecto)
  brigada_log         <- cargar_brigada_log(pool, id_proyecto)
  num_map             <- dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::select(Id, Num) |>
    dplyr::collect() |>
    dplyr::transmute(id_usuario = Id, num = as.character(Num)) |>
    dplyr::distinct(id_usuario, .keep_all = TRUE)
  # Asignaciones usuario-cuestionario: base para reincorporar coordinadores con
  # cuestionario asignado (aunque no hayan registrado diálogos) en el reporte.
  usuarios_encuesta   <- dplyr::tbl(pool, "UsuariosEncuesta") |>
    dplyr::select(UsuarioId, EncuestaId, Activo) |>
    dplyr::collect()

  bd_actividad <- cargar_actividad(
    pool = pool,
    fuentes = fuentes_actividad,
    fecha_min = fecha_min_actividad,
    fecha_max = corte,
    filtro_minimo = filtro_minimo_actividad,
    normalizador = normalizador_actividad
  )

  estructura_corte <- resolver_estructura_corte(usuario_log, corte, usuarios_asignados, coordinadores_cat$id_supervisor)
  brigada_corte    <- resolver_coordinador_en_fecha(brigada_log, corte)

  bd_aux <- construir_bd_aux(
    estructura_corte,
    usuarios_cat,
    coordinadores_cat,
    brigada_corte,
    municipios_cat,
    zonas_cat = zonas_cat,
    grupos_cat = grupos_cat
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
    cargo_coordinador = cargo_coordinador,
    bd_actividad = bd_actividad,
    bd_aux = bd_aux,
    pase_lista = pase_lista,
    cat = list(
      usuarios    = usuarios_cat,
      brigadas    = brigadas_cat,
      brigada_log = brigada_log,
      municipios  = municipios_cat,
      zonas       = zonas_cat,
      grupos      = grupos_cat,
      usuario_log = usuario_log,
      num_map     = num_map,
      usuarios_encuesta = usuarios_encuesta
    )
  )

  # Ejecutar el Hook de inyección de reglas de negocio
  if (!is.null(postprocess_insumos)) {
    insumos <- postprocess_insumos(insumos)
  }

  # Excluir brigadas de prueba al final, respetando lo que haya hecho el hook.
  if (isTRUE(excluir_pruebas)) {
    limpio <- excluir_brigadas_prueba(
      insumos$bd_aux,
      insumos$bd_actividad,
      patron = patron_pruebas
    )
    insumos$bd_aux       <- limpio$bd_aux
    insumos$bd_actividad <- limpio$bd_actividad
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


#' Inferir coordinador desde brigada para voceros sin IdSupervisor en UsuarioLog
#'
#' @description
#' Hook auxiliar diseñado para usarse dentro de `postprocess_insumos`. Cuando
#' `construir_bd_aux` emite un warning por brigadas sin `IdSupervisor` registrado
#' en la plataforma, esta función intenta poblar los campos de coordinador
#' (`nombre_coordinador`, `supervisor`, `status_coord`) usando la relación
#' brigada → coordinador del catálogo de usuarios.
#'
#' La inferencia es **unívoca** cuando hay exactamente un coordinador activo por
#' brigada. Si hay más de uno, la fila se deja intacta y se emite un warning
#' para resolución manual.
#'
#' @details
#' Esta función **no reemplaza** la corrección en plataforma. Es una mitigación
#' explícita y opt-in para proyectos donde se sabe que `IdSupervisor` no se
#' popula correctamente. Debe llamarse en el hook `postprocess_insumos` del
#' script de proyecto:
#'
#' ```r
#' cargo_coordinador <- "Jefe de Brigada"   # ajustar por proyecto
#'
#' cargar_insumos(
#'   ...,
#'   cargo_coordinador = cargo_coordinador,
#'   postprocess_insumos = function(insumos) {
#'     inferir_coordinador_desde_brigada(insumos, cargo_coordinador = cargo_coordinador)
#'   }
#' )
#' ```
#'
#' @param insumos Lista producida por `cargar_insumos()`.
#' @param cargo_coordinador Valor del campo `cargo` que identifica coordinadores
#'   en `insumos$cat$usuarios`. Debe coincidir con el valor usado en `cargar_insumos()`.
#' @param on_ambiguo Qué hacer cuando una brigada tiene más de un coordinador:
#'   - `"warn_skip"` (default): emite warning y deja la fila sin modificar.
#'   - `"error"`: aborta con un error explícito.
#'
#' @return `insumos` con `bd_aux` parcheado donde la inferencia fue unívoca.
#'
#' @section Seguridad y Privacidad:
#' Nivel de datos: INTERNO (estructura operativa, sin PII de encuestados).
#' Control ISO 27001: A.8.2 (Clasificación de información).
#'
#' @export
inferir_coordinador_desde_brigada <- function(
  insumos,
  cargo_coordinador = "Coordinador de Brigada",
  on_ambiguo = c("warn_skip", "error")
) {
  on_ambiguo <- match.arg(on_ambiguo)

  bd_aux       <- insumos$bd_aux
  usuarios_cat <- insumos$cat$usuarios
  brigadas_cat <- insumos$cat$brigadas

  # Filas candidatas: voceros sin coordinador asignado, con brigada conocida
  candidatos <- bd_aux |>
    dplyr::filter(is.na(nombre_coordinador), !is.na(nombre_brigada), nombre_brigada != "-") |>
    dplyr::distinct(nombre_brigada)

  if (nrow(candidatos) == 0) {
    return(insumos)
  }

  # Mapa brigada → coordinador desde el catálogo
  coord_brigada <- usuarios_cat |>
    dplyr::filter(
      toupper(cargo) == toupper(cargo_coordinador),
      !is.na(id_brigada)
    ) |>
    dplyr::left_join(
      brigadas_cat |> dplyr::select(id_brigada, nombre_brigada),
      by = "id_brigada"
    ) |>
    dplyr::filter(!is.na(nombre_brigada)) |>
    dplyr::transmute(
      nombre_brigada,
      supervisor_inf      = num,
      nombre_coord_inf    = nombre_completo,
      status_coord_inf    = status
    )

  # Detectar ambigüedad: brigadas con más de un coordinador en catálogo
  conteo <- coord_brigada |>
    dplyr::count(nombre_brigada, name = "n_coords")

  ambiguas <- conteo |> dplyr::filter(n_coords > 1)

  if (nrow(ambiguas) > 0) {
    detalle <- ambiguas |>
      dplyr::left_join(
        coord_brigada |> dplyr::group_by(nombre_brigada) |>
          dplyr::summarise(
            coords = paste(nombre_coord_inf, collapse = ", "),
            .groups = "drop"
          ),
        by = "nombre_brigada"
      ) |>
      dplyr::transmute(msg = sprintf(
        "Brigada '%s': %d coordinadores (%s)",
        nombre_brigada, n_coords, coords
      )) |>
      dplyr::pull(msg)

    msg <- sprintf(
      "inferir_coordinador_desde_brigada: %d brigada(s) amb\u00f3gua(s) (m\u00e1s de un coordinador en cat\u00e1logo). No se infiere para estas brigadas.",
      nrow(ambiguas)
    )

    if (on_ambiguo == "error") {
      stop(paste(c(msg, detalle), collapse = "\n  "))
    } else {
      cli::cli_warn(c(
        "!" = msg,
        " " = "Resuelve manualmente en el hook postprocess_insumos.",
        stats::setNames(detalle, rep("*", length(detalle)))
      ))
    }
  }

  # Solo inferir donde la relación es unívoca
  coord_univoca <- coord_brigada |>
    dplyr::semi_join(conteo |> dplyr::filter(n_coords == 1), by = "nombre_brigada")

  # Intersección: brigadas candidatas con inferencia posible
  a_parchear <- candidatos |>
    dplyr::inner_join(coord_univoca, by = "nombre_brigada")

  if (nrow(a_parchear) == 0) {
    return(insumos)
  }

  # Parchear bd_aux: solo filas candidatas con brigada unívoca
  bd_aux_parcheada <- bd_aux |>
    dplyr::left_join(a_parchear, by = "nombre_brigada") |>
    dplyr::mutate(
      nombre_coordinador = dplyr::if_else(
        is.na(nombre_coordinador) & !is.na(nombre_coord_inf),
        nombre_coord_inf,
        nombre_coordinador
      ),
      supervisor = dplyr::if_else(
        is.na(supervisor) & !is.na(supervisor_inf),
        supervisor_inf,
        supervisor
      ),
      status_coord = dplyr::if_else(
        is.na(status_coord) & !is.na(status_coord_inf),
        status_coord_inf,
        status_coord
      )
    ) |>
    dplyr::select(-supervisor_inf, -nombre_coord_inf, -status_coord_inf)

  n_parcheados <- bd_aux_parcheada |>
    dplyr::filter(
      nombre_brigada %in% a_parchear$nombre_brigada,
      !is.na(nombre_coordinador)
    ) |>
    nrow()

  cli::cli_inform(c(
    "v" = sprintf(
      "inferir_coordinador_desde_brigada: coordinador inferido para %d brigada(s), %d vocero(s) actualizados.",
      nrow(a_parchear), n_parcheados
    )
  ))

  insumos$bd_aux <- bd_aux_parcheada
  insumos
}
