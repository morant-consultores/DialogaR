# FreeTDS usa ISO 8859-1 estricto: el rango 0x80-0x9F está indefinido.
# iconv("UTF-8", "latin1") en macOS/Linux usa Windows-1252 internamente,
# por lo que caracteres como •, ™, € convierten sin devolver NA pero
# producen bytes en 0x80-0x9F que FreeTDS no puede manejar.
# La función detecta ambos casos: chars fuera de latin1 Y bytes en 0x80-0x9F.
.tiene_problemas_freetds <- function(x) {
  en_latin1 <- iconv(x, "UTF-8", "latin1", sub = NA_character_)
  any(!is.na(x) & (
    is.na(en_latin1) |
    grepl("[\x80-\x9F]", en_latin1, useBytes = TRUE)
  ))
}

sanitizar_para_freetds <- function(df) {
  dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    ~ if (.tiene_problemas_freetds(.x)) iconv(.x, "UTF-8", "latin1", sub = "") else .x
  ))
}

# Devuelve id, posición de columna (@Pn), nombre y valor original de cada
# celda problemática para identificar el parámetro que falla en el error.
diagnosticar_no_latin1 <- function(df) {
  char_cols <- which(sapply(df, is.character))
  cols_afectadas <- Filter(
    function(col) .tiene_problemas_freetds(df[[col]]),
    names(char_cols)
  )
  if (length(cols_afectadas) == 0) {
    cli::cli_alert_info("Sin caracteres problemáticos para FreeTDS.")
    return(invisible(tibble::tibble()))
  }
  purrr::map_dfr(cols_afectadas, function(col) {
    en_latin1 <- iconv(df[[col]], "UTF-8", "latin1", sub = NA_character_)
    mask <- !is.na(df[[col]]) & (
      is.na(en_latin1) |
      grepl("[\x80-\x9F]", en_latin1, useBytes = TRUE)
    )
    tibble::tibble(
      param   = paste0("@P", char_cols[[col]]),
      columna = col,
      id      = if ("id" %in% names(df)) df$id[mask] else which(mask),
      valor   = df[[col]][mask]
    )
  })
}

limpiar_texto_sql_agresivo <- function(x) {
  limpio <- x |>
    stringi::stri_enc_toutf8() |>
    stringi::stri_replace_all_regex("\\p{Z}+", " ") |>
    stringr::str_squish() |>
    stringi::stri_replace_all_regex("[\\u0080-\\u009F\\u0100-\\uFFFF]", "")
  iconv(limpio, "UTF-8", "latin1", sub = "")
}

categorizar_ds <- function(
  pool,
  encuesta_id,
  numeros_prueba,
  fecha_inicio,
  puerta = T,
  ids,
  remover_columnas = c(),
  max_id = NULL
) {
  # Filtrar por Id > max_id en el servidor: Id es auto-increment asignado al
  # momento del INSERT, por lo que registros subidos tarde siguen teniendo un
  # Id mayor al último procesado, independientemente de su FechaInicio.
  # El !Id %in% ids en R actúa como red de seguridad ante duplicados.
  registros_tbl <- tbl(pool, "Registros") |>
    filter(
      EncuestaId == !!encuesta_id,
      !UsuarioNum %in% !!numeros_prueba
    )

  if (!is.null(max_id) && !is.na(max_id)) {
    max_id_num <- as.numeric(max_id)
    registros_tbl <- registros_tbl |> filter(Id > !!max_id_num)
  } else {
    fecha_inicio_dt <- as.POSIXct(fecha_inicio, tz = "UTC")
    registros_tbl <- registros_tbl |> filter(FechaInicio >= !!fecha_inicio_dt)
  }

  aux_collected <- registros_tbl |> collect()

  aux <- aux_collected |>
    filter(!Id %in% ids)

  ja <- aux |>
    filter(n() > 1, .by = IdCliente) |>
    nrow()

  if (ja > 0) {
    print(paste("Registros repetidos", ja))
  } else {
    print("Sin registros repetidos")
  }

  n_after_antijoin <- nrow(aux)
  cli::cli_alert_info(
    "[{encuesta_id}] Tras anti_join: {n_after_antijoin} registros nuevos"
  )

  if (n_after_antijoin > 0) {
    aux <- aux |>
      distinct(IdCliente, .keep_all = TRUE)

    aux <- aux |>
      pmap_df(function(
        Id,
        EncuestaId,
        FechaInicio,
        FechaFin,
        UsuarioNum,
        Resultado,
        isComplete,
        TipoRegistro,
        ...
      ) {
        list_respuestas <- list()

        list_respuestas$id = Id
        list_respuestas$fecha_inicio = as_datetime(lubridate::force_tz(
          FechaInicio,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$fecha_fin = as_datetime(lubridate::force_tz(
          FechaFin,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$fecha = as_date(lubridate::force_tz(
          FechaInicio,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$usuario_num = UsuarioNum
        list_respuestas$isComplete = isComplete
        list_respuestas$duracion_minutos = round(
          as.numeric(FechaFin - FechaInicio),
          digits = 2
        )

        list_respuestas$TipoRegistro = TipoRegistro
        list_respuestas$EncuestaId = EncuestaId

        list_respuestas <- list_respuestas |>
          append(
            Resultado |>
              jsonlite::fromJSON()
          )

        bd_respuestas <- do.call(
          c,
          lapply(seq_along(list_respuestas), function(i) {
            nombre <- names(list_respuestas)[i]
            valores <- list_respuestas[[i]]
            if (length(valores) > 1) {
              setNames(
                as.list(valores),
                paste0(nombre, "_O", seq_along(valores))
              )
            } else {
              setNames(list(valores), nombre)
            }
          })
        )
        return(bd_respuestas)
      }) |>
      select(-any_of(remover_columnas))

    if (puerta == T & nrow(aux) > 0) {
      aux <- aux |>
        mutate(
          desglose = case_when(
            TipoRegistro == "Diálogo efectivo" ~ "Efectivo",
            TipoRegistro == "Diálogo cancelado" ~ "Cancelado",
            TipoRegistro %in%
              c("Vivienda visitada", "No aplica") &
              puerta == "Sí, rechazaron" ~ "Sí, rechazaron",
            TipoRegistro %in%
              c("Vivienda visitada", "No aplica") &
              puerta == "No abrieron" ~ "No abrieron",
            T ~ "ERROR"
          ),
          across(
            any_of("edad"),
            ~ if_else(as.numeric(.x) > 120, NA_real_, as.numeric(.x))
          ),
          across(
            c(
              any_of(c(
                "nombres", "apellido_pate", "apellido_mate",
                "direccion_calle", "direccion_colonia",
                "direccion_num_ext", "direccion_num_int"
              )),
              contains("other")
            ),
            limpiar_texto_sql_agresivo
          )
        ) |>
        select(c(id, fecha, usuario_num, isComplete), everything())
    }
    return(
      aux |>
        janitor::clean_names()
    )
  } else {
    print("Sin registros nuevos")
    return(tibble::tibble())
  }
}

# Mapea nombres de columna (snake_case) a tipos T-SQL.
# id_usuario es NOT NULL a nivel DDL: el motor rechaza filas sin ese valor.
mapear_tipo_tsql <- function(nombre_columna) {
  dplyr::case_when(
    nombre_columna %in% c("id", "encuesta_id", "usuario_num") ~ "INT",
    nombre_columna == "id_usuario"                            ~ "INT NOT NULL",
    nombre_columna %in% c("fecha_inicio", "fecha_fin")       ~ "DATETIME2",
    nombre_columna == "fecha"                                 ~ "DATE",
    nombre_columna == "duracion_minutos"                      ~ "FLOAT",
    nombre_columna == "is_complete"                           ~ "BIT",
    TRUE                                                      ~ "NVARCHAR(255)"
  )
}

#' Genera el query DDL para crear la tabla snapshot con sintaxis T-SQL
#'
#' La tabla se crea en el esquema \code{Reportes}. Si ya existe se elimina
#' antes de recrearla. La columna \code{id_usuario} se define como
#' \code{INT NOT NULL}; cualquier intento de insertar filas con ese campo
#' en NULL será rechazado por el motor.
#'
#' @param columnas_finales Vector de caracteres con los nombres de las columnas.
#' @param id_opinometro Entero que identifica el cuestionario.
#'
#' @return Un string con la sentencia SQL para crear la tabla en T-SQL.
crear_query_snapshot <- function(columnas_finales, id_opinometro) {
  nombre_tabla <- glue::glue("[Reportes].[snapshot_id_{id_opinometro}]")

  definiciones_columnas <- purrr::map_chr(
    columnas_finales,
    ~ glue::glue("  [{..1}] {mapear_tipo_tsql(..1)}")
  ) |>
    paste(collapse = ",\n")

  query <- glue::glue(
    "IF OBJECT_ID('{nombre_tabla}', 'U') IS NOT NULL\n",
    "  DROP TABLE {nombre_tabla};\n\n",
    "CREATE TABLE {nombre_tabla} (\n",
    "{definiciones_columnas}\n",
    ");"
  )

  return(query)
}

#' Valida que id_usuario no tenga NAs antes de escribir al snapshot
#'
#' Emite un \code{warning()} por cada fila con \code{id_usuario} en NA.
#' Llamar antes de cualquier INSERT para evitar que el motor rechace las filas.
#'
#' @param df Data frame con los datos del snapshot ya procesados.
#'
#' @return \code{df} de forma invisible (permite uso en pipes).
validar_id_usuario_snapshot <- function(df) {
  if (!"id_usuario" %in% names(df)) {
    warning("La columna 'id_usuario' no existe en los datos. Es obligatoria para el snapshot.")
    return(invisible(df))
  }
  n_na <- sum(is.na(df$id_usuario))
  if (n_na > 0) {
    warning(sprintf(
      "%d fila(s) tienen 'id_usuario' = NA. La columna es NOT NULL en el esquema Reportes y el INSERT fallará.",
      n_na
    ))
  }
  invisible(df)
}

#' Extrae campos del JSON Resultado con SQL JSON_VALUE
#'
#' @param pool Pool de conexión a la base de datos.
#' @param codigos Vector de claves a extraer del campo \code{Resultado}.
#' @param encuesta_id Entero o vector de enteros con los IDs de encuesta.
#'
#' @return \code{tbl} lazy con las columnas fijas de Registros más una columna por cada clave en \code{codigos}.
obtener_respuesta <- function(pool, codigos, encuesta_id) {
  query_claves <- paste0(
    "REPLACE(JSON_VALUE(r.Resultado, '$.", codigos, "'), 'ñ', 'n') AS ", codigos,
    collapse = ", "
  )
  encuesta_id_str <- toString(encuesta_id)

  query <- glue::glue("
    SELECT
      r.Id,
      r.EncuestaId,
      r.FechaInicio,
      r.FechaFin,
      r.FechaCreada,
      r.UbicacionAplicada,
      r.UsuarioNum,
      r.isComplete,
      {query_claves}
    FROM
      Registros r
    WHERE EncuestaId in ({encuesta_id_str})
  ")

  dplyr::tbl(pool, dplyr::sql(query)) |>
    janitor::clean_names()
}

#' Determina las columnas finales del snapshot a partir de registros reales
#'
#' Descubre los campos del cuestionario muestreando los JSON \code{Resultado}
#' más recientes en lugar de depender de metadatos externos. Las claves
#' presentes en al menos uno de los registros de la muestra se incluyen
#' (tras aplicar los filtros de \code{config$patrones_excluir}).
#'
#' @param pool Pool de conexión a la base de datos.
#' @param id_opinometro Entero que identifica el cuestionario.
#' @param config Lista de configuración; ver \code{config_snapshot}.
#' @param n_muestra Número de registros recientes a muestrear para inferir el esquema. Default: 30.
#'
#' @return Lista con \code{columnas} (vector de nombres en snake_case) y \code{log} (advertencias).
generar_columnas_snapshot <- function(
  pool,
  id_opinometro,
  config,
  n_muestra = 30
) {
  log <- list()

  # arrange(desc(Id)) garantiza que la muestra refleje el esquema más reciente
  # del cuestionario ante cambios de versión mid-campo.
  muestra_json <- tbl(pool, "Registros") |>
    filter(EncuestaId == !!id_opinometro, !is.na(Resultado)) |>
    select(Resultado) |>
    arrange(desc(Id)) |>
    head(n_muestra) |>
    collect() |>
    pull(Resultado)

  if (length(muestra_json) == 0) {
    stop(sprintf(
      "No se encontraron registros con Resultado para encuesta_id = %d.",
      id_opinometro
    ))
  }

  nombres_cuestionario <- muestra_json |>
    purrr::map(~ names(jsonlite::fromJSON(.x))) |>
    purrr::reduce(union)

  nombres_cuestionario <- nombres_cuestionario[
    !grepl(config$patrones_excluir, nombres_cuestionario)
  ]

  if (!"edad" %in% janitor::make_clean_names(nombres_cuestionario)) {
    log$advertencias <- c(
      log$advertencias,
      "La variable 'edad' no se encontró en el cuestionario."
    )
  }

  if (!"id_usuario" %in% janitor::make_clean_names(nombres_cuestionario)) {
    warning(
      "'id_usuario' no está en los elementos del cuestionario. ",
      "El pipeline debe proveerla antes del INSERT; de lo contrario el snapshot fallará."
    )
    log$advertencias <- c(
      log$advertencias,
      "'id_usuario' no encontrada en el cuestionario — validar que el pipeline la incluya."
    )
  }

  llaves_finales <- unique(c(
    config$columnas_obligatorias,
    janitor::make_clean_names(nombres_cuestionario)
  ))

  return(list(
    columnas = llaves_finales,
    log = log
  ))
}

config_snapshot <- list(
  columnas_obligatorias = janitor::make_clean_names(c(
    "Id",
    "FechaInicio",
    "FechaFin",
    "UsuarioNum",
    "IdUsuario",
    "isComplete",
    "TipoRegistro",
    "fecha",
    "duracion_minutos",
    "desglose",
    "municipio"
  )),
  patrones_excluir = "intentos|introduccion|Pregunta|gps_"
)
