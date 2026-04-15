# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_validadores.R
# OBJETIVO: Contratos de datos en fronteras ETL — falla rápido con mensajes
#           accionables antes de que un cambio en la BD llegue a los reportes
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno / Core
# -------------------------------------------------------------------------
# NOTAS:
#   - Todas las funciones son internas (no exportadas).
#   - Uso: df <- validar_X(df)  →  retorna df invisiblemente si todo está bien.
#   - En caso de violación: rlang::abort() con clase "error_contrato_<contexto>".
#   - Los data frames vacíos (nrow == 0) siempre pasan; evita falsos positivos
#     en flujos con early-return.
# -------------------------------------------------------------------------

# ---- Helpers internos ---------------------------------------------------

# Formatea las primeras `max` filas para incluirlas en mensajes de error
.fmt_filas <- function(df, max = 5) {
  paste(
    utils::capture.output(
      print(utils::head(df, max), row.names = FALSE)
    ),
    collapse = "\n"
  )
}

# Verifica que todas las columnas requeridas estén presentes
.check_cols <- function(df, requeridas, clase) {
  faltantes <- setdiff(requeridas, names(df))
  if (length(faltantes) > 0) {
    rlang::abort(
      c(
        paste0("Columnas requeridas faltantes: ", paste(faltantes, collapse = ", ")),
        i = paste0("Columnas presentes: ", paste(names(df), collapse = ", "))
      ),
      class = clase
    )
  }
}

# ---- Catálogos ----------------------------------------------------------

#' @noRd
validar_usuarios_cat <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("id_usuario", "num", "cargo", "status", "municipio_usuario", "id_brigada")
  .check_cols(df, requeridas, "error_contrato_usuarios")

  # status debe ser logical o integer {0, 1} (SQLite almacena boolean como integer)
  if (!is.logical(df$status) && !is.integer(df$status) && !is.numeric(df$status)) {
    rlang::abort(
      c(
        paste0("La columna `status` debe ser logical o integer, encontrado: ", paste(class(df$status), collapse = "/")),
        i = paste("Valores de muestra:", paste(utils::head(df$status, 5), collapse = ", "))
      ),
      class = "error_contrato_usuarios"
    )
  }
  valores_status <- unique(df$status[!is.na(df$status)])
  if (!all(valores_status %in% c(TRUE, FALSE, 1L, 0L, 1, 0))) {
    rlang::abort(
      c(
        "La columna `status` contiene valores fuera de {TRUE, FALSE}",
        i = paste("Valores encontrados:", paste(utils::head(valores_status, 10), collapse = ", "))
      ),
      class = "error_contrato_usuarios"
    )
  }

  # id_usuario: sin NA
  nas <- which(is.na(df$id_usuario))
  if (length(nas) > 0) {
    rlang::abort(
      c(
        paste0("id_usuario contiene ", length(nas), " NA(s)"),
        i = .fmt_filas(df[nas, ])
      ),
      class = "error_contrato_usuarios"
    )
  }

  # id_usuario: sin duplicados
  dupes <- df$id_usuario[duplicated(df$id_usuario)]
  if (length(dupes) > 0) {
    rlang::abort(
      c(
        paste0("id_usuario tiene ", length(dupes), " valor(es) duplicado(s)"),
        i = paste("IDs duplicados:", paste(utils::head(unique(dupes), 10), collapse = ", "))
      ),
      class = "error_contrato_usuarios"
    )
  }

  invisible(df)
}

#' @noRd
validar_brigadas_cat <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("id_brigada", "nombre_brigada", "activo_brigada", "id_zona_trabajo_brigada")
  .check_cols(df, requeridas, "error_contrato_brigadas")

  # Los primeros 2 caracteres de nombre_brigada deben ser el código de distrito (entero)
  nombres_validos <- df$nombre_brigada[!is.na(df$nombre_brigada)]
  if (length(nombres_validos) > 0) {
    prefijos <- substr(nombres_validos, 1, 2)
    invalidos <- unique(prefijos[is.na(suppressWarnings(as.integer(prefijos)))])
    if (length(invalidos) > 0) {
      rlang::abort(
        c(
          paste0(
            length(invalidos),
            " brigada(s) con código de distrito no numérico en los primeros 2 caracteres"
          ),
          i = paste("Prefijos inválidos:", paste(utils::head(invalidos, 10), collapse = ", ")),
          i = "Se esperan prefijos numéricos como '06', '07', etc."
        ),
        class = "error_contrato_brigadas"
      )
    }
  }

  invisible(df)
}

# ---- Actividad ----------------------------------------------------------

# Valores canónicos de desglose; pueden sobreescribirse via parámetro
.VALORES_DESGLOSE <- c("Efectivo", "No abrieron", "Sí, rechazaron", "Cancelado", "ERROR")

#' @noRd
validar_bd_actividad <- function(df, valores_desglose = NULL) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("fecha", "usuario_num", "seccion", "desglose",
                  "duracion_minutos", "fecha_inicio", "fecha_fin")
  .check_cols(df, requeridas, "error_contrato_actividad")

  # fecha debe ser Date
  if (!inherits(df$fecha, "Date")) {
    rlang::abort(
      c(
        paste0("La columna `fecha` debe ser Date, encontrado: ", paste(class(df$fecha), collapse = "/")),
        i = "Usa as.Date() para coercionar antes de llamar esta función"
      ),
      class = "error_contrato_actividad"
    )
  }

  # desglose: valores permitidos
  vals <- if (!is.null(valores_desglose)) valores_desglose else .VALORES_DESGLOSE
  invalidos_df <- df[!df$desglose %in% vals & !is.na(df$desglose), ]
  if (nrow(invalidos_df) > 0) {
    rlang::abort(
      c(
        paste0("desglose contiene ", nrow(invalidos_df), " valor(es) fuera del conjunto permitido"),
        i = paste("Permitidos:", paste(vals, collapse = ", ")),
        i = paste("Encontrados:", paste(utils::head(unique(invalidos_df$desglose), 10), collapse = ", "))
      ),
      class = "error_contrato_actividad"
    )
  }

  # duracion_minutos >= 0
  malos_dur <- df[!is.na(df$duracion_minutos) & df$duracion_minutos < 0, ]
  if (nrow(malos_dur) > 0) {
    rlang::abort(
      c(
        paste0("duracion_minutos tiene ", nrow(malos_dur), " valor(es) negativo(s)"),
        i = .fmt_filas(malos_dur[, c("fecha", "usuario_num", "duracion_minutos")])
      ),
      class = "error_contrato_actividad"
    )
  }

  # fecha_fin >= fecha_inicio
  fi <- tryCatch(as.POSIXct(df$fecha_inicio), error = function(e) NULL)
  ff <- tryCatch(as.POSIXct(df$fecha_fin),    error = function(e) NULL)
  if (!is.null(fi) && !is.null(ff)) {
    malos_par <- df[!is.na(fi) & !is.na(ff) & ff < fi, ]
    if (nrow(malos_par) > 0) {
      rlang::abort(
        c(
          paste0("fecha_fin < fecha_inicio en ", nrow(malos_par), " registro(s)"),
          i = .fmt_filas(malos_par[, c("fecha", "usuario_num", "fecha_inicio", "fecha_fin")])
        ),
        class = "error_contrato_actividad"
      )
    }
  }

  invisible(df)
}

# ---- Estructura operativa -----------------------------------------------

#' @noRd
validar_bd_aux <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("distrito", "municipio", "nombre_brigada",
                  "nombre_coordinador", "vocero", "status_vocero")
  .check_cols(df, requeridas, "error_contrato_bd_aux")

  # Todo vocero activo debe tener coordinador y brigada asignados
  activos <- df[!is.na(df$status_vocero) & df$status_vocero == TRUE, ]

  sin_coord <- activos[is.na(activos$nombre_coordinador), ]
  if (nrow(sin_coord) > 0) {
    rlang::abort(
      c(
        paste0(nrow(sin_coord), " vocero(s) activo(s) sin coordinador asignado"),
        i = paste("Voceros:", paste(utils::head(sin_coord$vocero, 10), collapse = ", ")),
        i = "Todo vocero con status_vocero == TRUE debe tener nombre_coordinador no-NA"
      ),
      class = "error_contrato_bd_aux"
    )
  }

  sin_brigada <- activos[is.na(activos$nombre_brigada), ]
  if (nrow(sin_brigada) > 0) {
    rlang::abort(
      c(
        paste0(nrow(sin_brigada), " vocero(s) activo(s) sin brigada asignada"),
        i = paste("Voceros:", paste(utils::head(sin_brigada$vocero, 10), collapse = ", ")),
        i = "Todo vocero con status_vocero == TRUE debe tener nombre_brigada no-NA"
      ),
      class = "error_contrato_bd_aux"
    )
  }

  invisible(df)
}

# ---- Pase de lista ------------------------------------------------------

#' @noRd
validar_pase_lista <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("supervisor", "vocero", "fecha")
  .check_cols(df, requeridas, "error_contrato_pase_lista")

  for (col in requeridas) {
    nas <- which(is.na(df[[col]]))
    if (length(nas) > 0) {
      rlang::abort(
        c(
          paste0("`", col, "` contiene ", length(nas), " NA(s) en pase de lista"),
          i = .fmt_filas(df[nas, c("supervisor", "vocero", "fecha")])
        ),
        class = "error_contrato_pase_lista"
      )
    }
  }

  invisible(df)
}

# ---- Métricas diarias ---------------------------------------------------

#' @noRd
validar_metricas_diarias <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  cols_conteo <- c("viviendas_visitadas_nube", "efectivos", "no_abrieron_nube",
                   "rechazaron_nube", "cancelado_nube", "sin_informacion_nube")
  cols_presentes <- intersect(cols_conteo, names(df))

  for (col in cols_presentes) {
    negativos <- df[!is.na(df[[col]]) & df[[col]] < 0, ]
    if (nrow(negativos) > 0) {
      rlang::abort(
        c(
          paste0("`", col, "` tiene ", nrow(negativos), " valor(es) negativo(s)"),
          i = .fmt_filas(negativos[, c("usuario_num", col)])
        ),
        class = "error_contrato_metricas_diarias"
      )
    }
  }

  if (all(c("efectivos", "viviendas_visitadas_nube") %in% names(df))) {
    malos <- df[
      !is.na(df$efectivos) & !is.na(df$viviendas_visitadas_nube) &
        df$efectivos > df$viviendas_visitadas_nube,
    ]
    if (nrow(malos) > 0) {
      rlang::abort(
        c(
          paste0("efectivos > viviendas_visitadas_nube en ", nrow(malos), " fila(s)"),
          i = .fmt_filas(malos[, c("usuario_num", "efectivos", "viviendas_visitadas_nube")])
        ),
        class = "error_contrato_metricas_diarias"
      )
    }
  }

  invisible(df)
}

# ---- Base de contactos --------------------------------------------------

#' @noRd
validar_base_contactos <- function(df, na_chr = "-") {
  if (nrow(df) == 0) return(invisible(df))

  # Post-filtro: cada fila debe tener al menos celular o correo
  if (all(c("celular", "correo") %in% names(df))) {
    sin_contacto <- df[df$celular == na_chr & df$correo == na_chr, ]
    if (nrow(sin_contacto) > 0) {
      rlang::abort(
        c(
          paste0(nrow(sin_contacto), " fila(s) sin celular ni correo después del filtro"),
          i = paste("Valor centinela usado:", na_chr),
          i = .fmt_filas(sin_contacto)
        ),
        class = "error_contrato_contactos"
      )
    }
  }

  # nombre no debe ser NA
  if ("nombre" %in% names(df)) {
    sin_nombre <- df[is.na(df$nombre), ]
    if (nrow(sin_nombre) > 0) {
      rlang::abort(
        c(paste0(nrow(sin_nombre), " contacto(s) con `nombre` NA")),
        class = "error_contrato_contactos"
      )
    }
  }

  # edad: rango [1, 120] (NA es aceptable)
  if ("edad" %in% names(df)) {
    malos_edad <- df[!is.na(df$edad) & (df$edad < 1 | df$edad > 120), ]
    if (nrow(malos_edad) > 0) {
      rlang::abort(
        c(
          paste0("edad fuera del rango [1, 120] en ", nrow(malos_edad), " fila(s)"),
          i = .fmt_filas(malos_edad[, intersect(c("nombre", "edad"), names(malos_edad))])
        ),
        class = "error_contrato_contactos"
      )
    }
  }

  invisible(df)
}

# ---- Auditoría ----------------------------------------------------------

#' @noRd
validar_auditoria <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  requeridas <- c("totalEvaluacion", "dictamenFinal")
  .check_cols(df, requeridas, "error_contrato_auditoria")

  te <- suppressWarnings(as.numeric(df$totalEvaluacion))

  # totalEvaluacion no parseables como número (ignorar vacíos/NA)
  no_parseable <- df[
    is.na(te) & !is.na(df$totalEvaluacion) & nchar(trimws(df$totalEvaluacion)) > 0,
  ]
  if (nrow(no_parseable) > 0) {
    rlang::abort(
      c(
        paste0(nrow(no_parseable), " registro(s) con totalEvaluacion no numérico"),
        i = paste(
          "Valores:",
          paste(utils::head(unique(no_parseable$totalEvaluacion), 10), collapse = ", ")
        )
      ),
      class = "error_contrato_auditoria"
    )
  }

  # totalEvaluacion fuera de [0, 100]
  fuera_rango <- df[!is.na(te) & (te < 0 | te > 100), ]
  if (nrow(fuera_rango) > 0) {
    rlang::abort(
      c(
        paste0(nrow(fuera_rango), " registro(s) con totalEvaluacion fuera de [0, 100]"),
        i = paste(
          "Valores:",
          paste(utils::head(unique(fuera_rango$totalEvaluacion), 10), collapse = ", ")
        )
      ),
      class = "error_contrato_auditoria"
    )
  }

  invisible(df)
}
