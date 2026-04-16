# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_validadores.R
# OBJETIVO: Contratos de datos en fronteras ETL con clasificación por severidad
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno / Core
# -------------------------------------------------------------------------
# NIVELES DE SEVERIDAD
#
#   INTEGRIDAD  →  rlang::abort()
#     Detiene la ejecución porque el dato ya no es confiable:
#     · Columnas requeridas ausentes (la función no puede correr)
#     · PK duplicadas en catálogos (causaría explosión de filas en joins)
#     · Multiplicación o pérdida de filas tras un join
#     · Tipos incorrectos en columnas usadas como clave de join
#
#   CALIDAD  →  rlang::warn() con resumen consolidado
#     La pipeline puede continuar, pero se señalan áreas de riesgo:
#     · Valores fuera de rango o fuera de enum esperado
#     · Reglas de negocio violadas (vocero sin coordinador, etc.)
#     · NAs en campos opcionales relevantes
#     Todos los problemas de calidad se recolectan dentro del validador y
#     se emiten en un único mensaje, para que el usuario vea el panorama
#     completo sin detener la ejecución.
#
# NOTAS DE USO:
#   df <- validar_X(df)   →  retorna df invisiblemente si todo está bien.
#   Todos los data frames vacíos (nrow == 0) pasan sin revisión para evitar
#   falsos positivos en flujos con early-return.
# -------------------------------------------------------------------------

# ==========================================================================
# Helpers internos — INTEGRIDAD
# ==========================================================================

# Verifica que las columnas requeridas estén presentes. Abort si faltan.
.check_cols <- function(df, requeridas, clase) {
  faltantes <- setdiff(requeridas, names(df))
  if (length(faltantes) > 0) {
    rlang::abort(
      c(
        paste0("[INTEGRIDAD] Columnas requeridas faltantes: ", paste(faltantes, collapse = ", ")),
        i = paste0("Columnas presentes: ", paste(names(df), collapse = ", "))
      ),
      class = clase
    )
  }
}

# Verifica que una columna clave (PK) no tenga duplicados.
# Una PK duplicada en un catálogo multiplica filas silenciosamente en joins.
.check_no_dupes <- function(df, col_pk, clase) {
  dupes <- df[[col_pk]][duplicated(df[[col_pk]])]
  dupes <- unique(dupes[!is.na(dupes)])
  if (length(dupes) > 0) {
    rlang::abort(
      c(
        paste0(
          "[INTEGRIDAD] `", col_pk, "` tiene ", length(dupes),
          " valor(es) duplicado(s). Un join con esta tabla multiplicaría filas."
        ),
        i = paste("IDs duplicados (primeros 10):", paste(utils::head(dupes, 10), collapse = ", "))
      ),
      class = clase
    )
  }
}

# Verifica integridad de un join ya ejecutado: el número de filas del resultado
# debe ser igual al del data frame de la izquierda (left/inner one-to-many safe).
# Usar cuando un join debería ser estrictamente many-to-one.
.check_join_count <- function(df_antes, df_despues, descripcion, clase) {
  n_antes  <- nrow(df_antes)
  n_despues <- nrow(df_despues)

  if (n_despues > n_antes) {
    rlang::abort(
      c(
        paste0(
          "[INTEGRIDAD] ", descripcion, ": el join multiplicó filas.",
          " Antes: ", n_antes, ", Después: ", n_despues,
          " (+", n_despues - n_antes, " filas extra)."
        ),
        i = "Verifica que la tabla de la derecha no tenga claves duplicadas."
      ),
      class = clase
    )
  }

  if (n_despues < n_antes) {
    rlang::abort(
      c(
        paste0(
          "[INTEGRIDAD] ", descripcion, ": el join perdió filas.",
          " Antes: ", n_antes, ", Después: ", n_despues,
          " (-", n_antes - n_despues, " filas)."
        ),
        i = "Verifica que todas las claves del lado izquierdo existan en el catálogo."
      ),
      class = clase
    )
  }
}

# ==========================================================================
# Helpers internos — CALIDAD
# ==========================================================================

# Formatea las primeras `max` filas para incluirlas en mensajes de aviso
.fmt_filas <- function(df, max = 5) {
  paste(
    utils::capture.output(
      print(utils::head(df, max), row.names = FALSE)
    ),
    collapse = "\n"
  )
}

# Emite un único warning consolidado con todos los problemas de calidad
# recolectados. Si la lista está vacía, no emite nada.
.emit_quality_summary <- function(issues, context, clase) {
  if (length(issues) == 0) return(invisible(NULL))
  msgs <- c(
    sprintf("[CALIDAD — %s] %d área(s) de riesgo detectada(s):", context, length(issues)),
    unlist(issues)
  )
  rlang::warn(msgs, class = clase)
}

# ==========================================================================
# Catálogos
# ==========================================================================

#' @noRd
validar_usuarios_cat <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  requeridas <- c("id_usuario", "num", "cargo", "status", "municipio_usuario", "id_brigada")
  .check_cols(df, requeridas, "error_contrato_usuarios")

  # id_usuario no debe tener duplicados (PK del catálogo)
  .check_no_dupes(df, "id_usuario", "error_contrato_usuarios")

  # status debe ser logical o integer/numeric (SQLite almacena boolean como 0/1)
  if (!is.logical(df$status) && !is.integer(df$status) && !is.numeric(df$status)) {
    rlang::abort(
      c(
        paste0(
          "[INTEGRIDAD] `status` debe ser logical o integer/numeric, encontrado: ",
          paste(class(df$status), collapse = "/")
        ),
        i = paste("Valores de muestra:", paste(utils::head(df$status, 5), collapse = ", "))
      ),
      class = "error_contrato_usuarios"
    )
  }

  # --- CALIDAD ---
  issues <- list()

  # Valores de status fuera de {TRUE, FALSE, 1, 0}
  valores_status <- unique(df$status[!is.na(df$status)])
  if (!all(valores_status %in% c(TRUE, FALSE, 1L, 0L, 1, 0))) {
    issues <- c(issues, list(paste0(
      "! `status` contiene valores inesperados: ",
      paste(utils::head(valores_status, 10), collapse = ", ")
    )))
  }

  # id_usuario con NAs (registro huérfano)
  nas_id <- which(is.na(df$id_usuario))
  if (length(nas_id) > 0) {
    issues <- c(issues, list(paste0(
      "! ", length(nas_id), " fila(s) con id_usuario NA (registros huérfanos)"
    )))
  }

  .emit_quality_summary(issues, "usuarios_cat", "warn_calidad_usuarios")
  invisible(df)
}

#' @noRd
validar_brigadas_cat <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  requeridas <- c("id_brigada", "nombre_brigada", "activo_brigada", "id_zona_trabajo_brigada")
  .check_cols(df, requeridas, "error_contrato_brigadas")

  # id_brigada no debe tener duplicados (PK del catálogo)
  .check_no_dupes(df, "id_brigada", "error_contrato_brigadas")

  # --- CALIDAD ---
  issues <- list()

  # Prefijo numérico de distrito (convención, no regla universal)
  nombres_validos <- df$nombre_brigada[!is.na(df$nombre_brigada)]
  if (length(nombres_validos) > 0) {
    prefijos  <- substr(nombres_validos, 1, 2)
    invalidos <- unique(prefijos[is.na(suppressWarnings(as.integer(prefijos)))])
    if (length(invalidos) > 0) {
      issues <- c(issues, list(paste0(
        "! ", length(invalidos),
        " brigada(s) con código de distrito no numérico al inicio del nombre.",
        " Si este proyecto no usa la convención 'NN_NOMBRE', ignora este aviso.",
        " Prefijos: ", paste(utils::head(invalidos, 10), collapse = ", ")
      )))
    }
  }

  .emit_quality_summary(issues, "brigadas_cat", "warn_calidad_brigadas")
  invisible(df)
}

# ==========================================================================
# Actividad
# ==========================================================================

# Valores canónicos de desglose; pueden sobreescribirse via parámetro
.VALORES_DESGLOSE <- c("Efectivo", "No abrieron", "Sí, rechazaron", "Cancelado", "ERROR")

#' @noRd
validar_bd_actividad <- function(df, valores_desglose = NULL) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  requeridas <- c("fecha", "usuario_num", "seccion", "desglose",
                  "duracion_minutos", "fecha_inicio", "fecha_fin")
  .check_cols(df, requeridas, "error_contrato_actividad")

  # fecha debe ser Date (tipo incorrecto rompe cálculos temporales aguas abajo)
  if (!inherits(df$fecha, "Date")) {
    rlang::abort(
      c(
        paste0(
          "[INTEGRIDAD] `fecha` debe ser Date, encontrado: ",
          paste(class(df$fecha), collapse = "/")
        ),
        i = "Usa as.Date() para coercionar antes de llamar esta función."
      ),
      class = "error_contrato_actividad"
    )
  }

  # --- CALIDAD ---
  issues <- list()

  vals <- if (!is.null(valores_desglose)) valores_desglose else .VALORES_DESGLOSE
  invalidos_df <- df[!df$desglose %in% vals & !is.na(df$desglose), ]
  if (nrow(invalidos_df) > 0) {
    issues <- c(issues, list(paste0(
      "! desglose: ", nrow(invalidos_df), " valor(es) fuera del conjunto permitido.",
      " Permitidos: ", paste(vals, collapse = ", "), ".",
      " Encontrados: ", paste(utils::head(unique(invalidos_df$desglose), 10), collapse = ", ")
    )))
  }

  malos_dur <- df[!is.na(df$duracion_minutos) & df$duracion_minutos < 0, ]
  if (nrow(malos_dur) > 0) {
    issues <- c(issues, list(paste0(
      "! duracion_minutos: ", nrow(malos_dur), " valor(es) negativo(s).\n",
      .fmt_filas(malos_dur[, c("fecha", "usuario_num", "duracion_minutos")])
    )))
  }

  fi <- tryCatch(as.POSIXct(df$fecha_inicio), error = function(e) NULL)
  ff <- tryCatch(as.POSIXct(df$fecha_fin),    error = function(e) NULL)
  if (!is.null(fi) && !is.null(ff)) {
    malos_par <- df[!is.na(fi) & !is.na(ff) & ff < fi, ]
    if (nrow(malos_par) > 0) {
      issues <- c(issues, list(paste0(
        "! fecha_fin < fecha_inicio en ", nrow(malos_par), " registro(s).\n",
        .fmt_filas(malos_par[, c("fecha", "usuario_num", "fecha_inicio", "fecha_fin")])
      )))
    }
  }

  .emit_quality_summary(issues, "bd_actividad", "warn_calidad_actividad")
  invisible(df)
}

# ==========================================================================
# Estructura operativa
# ==========================================================================

#' @noRd
validar_bd_aux <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  requeridas <- c("distrito", "municipio", "nombre_brigada",
                  "nombre_coordinador", "vocero", "status_vocero")
  .check_cols(df, requeridas, "error_contrato_bd_aux")

  # --- CALIDAD ---
  issues <- list()

  # Voceros activos sin coordinador o sin brigada asignados
  activos <- df[!is.na(df$status_vocero) & df$status_vocero == TRUE, ]

  sin_coord <- activos[is.na(activos$nombre_coordinador), ]
  if (nrow(sin_coord) > 0) {
    issues <- c(issues, list(paste0(
      "! ", nrow(sin_coord), " vocero(s) activo(s) sin coordinador asignado.",
      " Voceros: ", paste(utils::head(sin_coord$vocero, 10), collapse = ", "),
      ". Todo vocero activo debería tener nombre_coordinador no-NA."
    )))
  }

  sin_brigada <- activos[is.na(activos$nombre_brigada), ]
  if (nrow(sin_brigada) > 0) {
    issues <- c(issues, list(paste0(
      "! ", nrow(sin_brigada), " vocero(s) activo(s) sin brigada asignada.",
      " Voceros: ", paste(utils::head(sin_brigada$vocero, 10), collapse = ", "),
      ". Todo vocero activo debería tener nombre_brigada no-NA."
    )))
  }

  .emit_quality_summary(issues, "bd_aux", "warn_calidad_bd_aux")
  invisible(df)
}

# ==========================================================================
# Pase de lista
# ==========================================================================

#' @noRd
validar_pase_lista <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  # supervisor, vocero y fecha son claves de identificación del registro;
  # un NA en cualquiera de ellas hace el registro no atribuible.
  requeridas <- c("supervisor", "vocero", "fecha")
  .check_cols(df, requeridas, "error_contrato_pase_lista")

  for (col in requeridas) {
    nas <- which(is.na(df[[col]]))
    if (length(nas) > 0) {
      rlang::abort(
        c(
          paste0(
            "[INTEGRIDAD] `", col, "` contiene ", length(nas),
            " NA(s) en pase de lista. El registro no puede atribuirse correctamente."
          ),
          i = .fmt_filas(df[nas, c("supervisor", "vocero", "fecha")])
        ),
        class = "error_contrato_pase_lista"
      )
    }
  }

  invisible(df)
}

# ==========================================================================
# Métricas diarias
# ==========================================================================

#' @noRd
validar_metricas_diarias <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  # (Sin checks de integridad estructural; las métricas son derivadas y
  #  sus columnas varían por proyecto. Los checks van a CALIDAD.)

  # --- CALIDAD ---
  issues <- list()

  cols_conteo <- c("viviendas_visitadas_nube", "efectivos", "no_abrieron_nube",
                   "rechazaron_nube", "cancelado_nube", "sin_informacion_nube")
  cols_presentes <- intersect(cols_conteo, names(df))

  for (col in cols_presentes) {
    negativos <- df[!is.na(df[[col]]) & df[[col]] < 0, ]
    if (nrow(negativos) > 0) {
      issues <- c(issues, list(paste0(
        "! `", col, "` tiene ", nrow(negativos), " valor(es) negativo(s).\n",
        .fmt_filas(negativos[, c("usuario_num", col)])
      )))
    }
  }

  if (all(c("efectivos", "viviendas_visitadas_nube") %in% names(df))) {
    malos <- df[
      !is.na(df$efectivos) & !is.na(df$viviendas_visitadas_nube) &
        df$efectivos > df$viviendas_visitadas_nube,
    ]
    if (nrow(malos) > 0) {
      issues <- c(issues, list(paste0(
        "! efectivos > viviendas_visitadas_nube en ", nrow(malos), " fila(s).\n",
        .fmt_filas(malos[, c("usuario_num", "efectivos", "viviendas_visitadas_nube")])
      )))
    }
  }

  .emit_quality_summary(issues, "metricas_diarias", "warn_calidad_metricas")
  invisible(df)
}

# ==========================================================================
# Base de contactos
# ==========================================================================

#' @noRd
validar_base_contactos <- function(df, na_chr = "-") {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  # El filtro de construir_base_contactos debe garantizar que todo registro
  # restante tenga al menos un medio de contacto. Si no, hay un bug en el flujo.
  if (all(c("celular", "correo") %in% names(df))) {
    sin_contacto <- df[df$celular == na_chr & df$correo == na_chr, ]
    if (nrow(sin_contacto) > 0) {
      rlang::abort(
        c(
          paste0(
            "[INTEGRIDAD] ", nrow(sin_contacto),
            " fila(s) sin celular ni correo tras el filtro de contactos.",
            " El filtro debería haber eliminado estas filas."
          ),
          i = paste("Valor centinela usado:", na_chr),
          i = .fmt_filas(sin_contacto)
        ),
        class = "error_contrato_contactos"
      )
    }
  }

  # --- CALIDAD ---
  issues <- list()

  if ("nombre" %in% names(df)) {
    sin_nombre <- df[is.na(df$nombre), ]
    if (nrow(sin_nombre) > 0) {
      issues <- c(issues, list(paste0(
        "! ", nrow(sin_nombre), " contacto(s) con `nombre` NA."
      )))
    }
  }

  if ("edad" %in% names(df)) {
    malos_edad <- df[!is.na(df$edad) & (df$edad < 1 | df$edad > 120), ]
    if (nrow(malos_edad) > 0) {
      issues <- c(issues, list(paste0(
        "! edad fuera del rango [1, 120] en ", nrow(malos_edad), " fila(s).\n",
        .fmt_filas(malos_edad[, intersect(c("nombre", "edad"), names(malos_edad))])
      )))
    }
  }

  .emit_quality_summary(issues, "base_contactos", "warn_calidad_contactos")
  invisible(df)
}

# ==========================================================================
# Auditoría
# ==========================================================================

#' @noRd
validar_auditoria <- function(df) {
  if (nrow(df) == 0) return(invisible(df))

  # --- INTEGRIDAD ---
  requeridas <- c("totalEvaluacion", "dictamenFinal")
  .check_cols(df, requeridas, "error_contrato_auditoria")

  # --- CALIDAD ---
  issues <- list()

  te <- suppressWarnings(as.numeric(df$totalEvaluacion))

  no_parseable <- df[
    is.na(te) & !is.na(df$totalEvaluacion) & nchar(trimws(df$totalEvaluacion)) > 0,
  ]
  if (nrow(no_parseable) > 0) {
    issues <- c(issues, list(paste0(
      "! ", nrow(no_parseable), " registro(s) con totalEvaluacion no numérico.",
      " Valores: ", paste(utils::head(unique(no_parseable$totalEvaluacion), 10), collapse = ", ")
    )))
  }

  fuera_rango <- df[!is.na(te) & (te < 0 | te > 100), ]
  if (nrow(fuera_rango) > 0) {
    issues <- c(issues, list(paste0(
      "! ", nrow(fuera_rango), " registro(s) con totalEvaluacion fuera de [0, 100].",
      " Valores: ", paste(utils::head(unique(fuera_rango$totalEvaluacion), 10), collapse = ", ")
    )))
  }

  .emit_quality_summary(issues, "auditoria", "warn_calidad_auditoria")
  invisible(df)
}
