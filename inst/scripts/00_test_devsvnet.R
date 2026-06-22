# =========================================================================
# SCRIPT: 00_test_devsvnet.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Flow test contra DEVSVNET-V2 para verificar si los cambios de
#           reglas de negocio en la plataforma resuelven inconsistencias
#           conocidas o introducen nuevas.
#
# CÓMO USAR:
#   1. Agrega en tu .Renviron las credenciales del entorno dev:
#        pool_dev_server=DEVSVNET-V2
#        pool_dev_database=<nombre_bd>
#        pool_dev_uid=<usuario>
#        pool_dev_pwd=<contraseña>
#   2. Fuente este script desde una sesión R con el paquete cargado:
#        devtools::load_all()
#        source("inst/scripts/00_test_devsvnet.R")
#   3. Lee el resumen final: sección "RESULTADO GENERAL".
#
# NOTA: No requiere Google Drive. Las dependencias de geometría/referencia
#       estática no son necesarias para validar la consistencia del ETL.
# =========================================================================

library(DialogaR)

if (utils::packageVersion("DialogaR") < "0.3.0") {
  stop(
    "Este script requiere DialogaR >= 0.3.0 (rama feat/brigada-log-coordinator).\n",
    "Instalar con: remotes::install_github('morant-consultores/DialogaR@feat/brigada-log-coordinator')",
    call. = FALSE
  )
}

library(dplyr)

# =========================================================================
# 1. PARÁMETROS DE PROYECTOS A TESTEAR
# =========================================================================

PROYECTOS <- list(
  list(
    nombre            = "Sonora",
    id_proyecto       = 26L,
    cargo_coordinador = "Coordinador de Brigada",
    fuentes_actividad = list(),
    ids_pase_lista    = integer(0),
    fecha_min_actividad = as.Date("2025-02-16"),
    fecha_min_pl      = as.Date("2026-02-16")
  )
)

corte <- Sys.Date() - 1L

# =========================================================================
# 2. HELPERS INTERNOS DEL TEST
# =========================================================================

# Acumulador de warnings capturados durante un bloque
capturar_warnings <- function(expr) {
  warns <- character(0)
  result <- withCallingHandlers(
    expr,
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(result = result, warnings = warns)
}

# Imprime resultados de una validación de estructura
validar_objeto <- function(nombre, objeto, nrow_min = 1L, cols_requeridas = character()) {
  ok <- TRUE
  if (!is.data.frame(objeto)) {
    cli::cli_alert_danger("{nombre}: NO es un data.frame")
    return(FALSE)
  }
  if (nrow(objeto) < nrow_min) {
    cli::cli_alert_warning("{nombre}: {nrow(objeto)} filas (esperado >= {nrow_min})")
    ok <- FALSE
  }
  faltantes <- setdiff(cols_requeridas, names(objeto))
  if (length(faltantes) > 0) {
    cli::cli_alert_warning("{nombre}: columnas faltantes — {paste(faltantes, collapse=', ')}")
    ok <- FALSE
  }
  if (ok) cli::cli_alert_success("{nombre}: OK ({nrow(objeto)} filas)")
  invisible(ok)
}

# =========================================================================
# 3. CONEXIÓN + EJECUCIÓN (dentro de función para que on.exit tenga alcance correcto)
# =========================================================================

ejecutar_test <- function() {
  cli::cli_h1("Conectando a DEVSVNET-V2 (perfil: dev)")

  pool <- tryCatch(
    conectar_base_datos(perfil = "dev"),
    error = function(e) {
      cli::cli_abort(c(
        "x" = "No se pudo conectar a DEVSVNET-V2.",
        "i" = conditionMessage(e),
        "i" = "Verifica que pool_dev_server, pool_dev_database, pool_dev_uid y pool_dev_pwd están en .Renviron"
      ))
    }
  )
  on.exit(tryCatch(pool::poolClose(pool), error = function(e) invisible(NULL)), add = TRUE)
  cli::cli_alert_success("Conectado a DEVSVNET-V2.")

# =========================================================================
# 4. EJECUCIÓN DE ETL POR PROYECTO
# =========================================================================

resultados <- list()

for (proy in PROYECTOS) {
  cli::cli_h1("Proyecto: {proy$nombre} (id={proy$id_proyecto})")

  procesador_pl_local <- local({
    fmin <- proy$fecha_min_pl
    function(pool, id_cuestionario) {
      procesar_pase_lista(
        pool           = pool,
        id_cuestionario = id_cuestionario,
        fecha_min      = fmin
      )
    }
  })

  filtro_minimo_actividad <- function(q, f) {
    q |> dplyr::filter(!is.na(seccion), seccion != "")
  }

  normalizador_actividad <- function(df, f) {
    df |> dplyr::mutate(is_complete = as.character(is_complete))
  }

  # ETL con captura de warnings
  cli::cli_h2("ETL: cargar_insumos")

  etl_out <- capturar_warnings(
    tryCatch(
      cargar_insumos(
        pool                    = pool,
        id_proyecto             = proy$id_proyecto,
        corte                   = corte,
        fuentes_actividad       = proy$fuentes_actividad,
        ids_pase_lista          = proy$ids_pase_lista,
        procesador_pl           = procesador_pl_local,
        fecha_min_actividad     = proy$fecha_min_actividad,
        filtro_minimo_actividad = filtro_minimo_actividad,
        normalizador_actividad  = normalizador_actividad,
        cargo_coordinador       = proy$cargo_coordinador,
        postprocess_insumos     = NULL
      ),
      error = function(e) {
        cli::cli_alert_danger("Error en cargar_insumos: {conditionMessage(e)}")
        NULL
      }
    )
  )

  insumos  <- etl_out$result
  warnings_etl <- etl_out$warnings

  if (is.null(insumos)) {
    resultados[[proy$nombre]] <- list(
      ok          = FALSE,
      error       = "cargar_insumos falló",
      warnings    = warnings_etl
    )
    next
  }

  cli::cli_alert_success(
    "ETL completado: {nrow(insumos$bd_actividad)} registros de actividad."
  )

  # ---- Validaciones estructurales -----------------------------------------

  cli::cli_h2("Validaciones")

  validar_objeto(
    "bd_aux",
    insumos$bd_aux,
    nrow_min = 1L,
    cols_requeridas = c(
      "nombre_brigada", "nombre_coordinador", "supervisor",
      "nombre_vocero", "vocero"
    )
  )

  validar_objeto(
    "bd_actividad",
    insumos$bd_actividad,
    nrow_min = 1L,
    cols_requeridas = c("fecha", "usuario_num", "desglose", "duracion_minutos")
  )

  # ---- Métricas de consistencia -------------------------------------------

  cli::cli_h2("Métricas de consistencia de plataforma")

  # Voceros sin coordinador asignado en bd_aux
  sin_coord <- insumos$bd_aux |>
    dplyr::filter(!is.na(vocero), is.na(nombre_coordinador)) |>
    dplyr::distinct(nombre_brigada, vocero, nombre_vocero)

  if (nrow(sin_coord) > 0) {
    cli::cli_alert_warning(
      "{nrow(sin_coord)} vocero(s) sin coordinador resuelto en bd_aux:"
    )
    for (i in seq_len(min(nrow(sin_coord), 10L))) {
      cli::cli_bullets(c(
        "*" = "Brigada '{sin_coord$nombre_brigada[i]}' — vocero {sin_coord$vocero[i]} ({sin_coord$nombre_vocero[i]})"
      ))
    }
    if (nrow(sin_coord) > 10L) {
      cli::cli_bullets(c("i" = "... y {nrow(sin_coord) - 10} más"))
    }
  } else {
    cli::cli_alert_success("Todos los voceros activos tienen coordinador resuelto.")
  }

  # Brigadas con IdUsuario apuntando a usuario inexistente o con cargo incorrecto
  brigadas_coord_invalido <- insumos$bd_aux |>
    dplyr::filter(!is.na(nombre_brigada), is.na(nombre_coordinador)) |>
    dplyr::distinct(nombre_brigada) |>
    dplyr::left_join(
      insumos$cat$brigadas |> dplyr::select(nombre_brigada, id_usuario_brigada),
      by = "nombre_brigada"
    ) |>
    dplyr::filter(!is.na(id_usuario_brigada))

  if (nrow(brigadas_coord_invalido) > 0) {
    cli::cli_alert_warning(
      "{nrow(brigadas_coord_invalido)} brigada(s) con IdUsuario en Brigadas pero sin coordinador resuelto (cargo incorrecto o usuario eliminado en plataforma)."
    )
  } else {
    cli::cli_alert_success("Todas las brigadas con IdUsuario tienen coordinador válido.")
  }

  # resolver_brigada_en_fecha: actividad sin brigada resuelta
  cols_act <- c("usuario_num", "fecha")
  tiene_actividad <- nrow(insumos$bd_actividad) > 0 &&
    all(cols_act %in% names(insumos$bd_actividad))

  if (!tiene_actividad) {
    cli::cli_alert_info("Sin actividad cargada — omitiendo resolver_brigada_en_fecha.")
    n_sin_brigada <- 0L
    warns_brigada <- character(0)
  } else {
    bd_con_brigada <- capturar_warnings(
      insumos$bd_actividad |>
        resolver_brigada_en_fecha(
          usuario_log  = insumos$cat$usuario_log,
          usuarios_cat = insumos$cat$usuarios,
          num_map      = insumos$cat$num_map
        )
    )
    n_sin_brigada <- sum(is.na(bd_con_brigada$result$id_brigada))
    warns_brigada <- bd_con_brigada$warnings
    if (n_sin_brigada > 0) {
      cli::cli_alert_warning(
        "{n_sin_brigada} registro(s) de actividad sin id_brigada resuelto."
      )
    } else {
      cli::cli_alert_success("id_brigada resuelto en todos los registros de actividad.")
    }
  }

  # Pases de lista
  for (id_pl in proy$ids_pase_lista) {
    pl_key <- paste0("pl_", id_pl)
    pl     <- insumos$pase_lista[[pl_key]]
    if (is.null(pl) || nrow(pl) == 0) {
      cli::cli_alert_warning("pase_lista${pl_key}: sin registros para el periodo.")
    } else {
      cli::cli_alert_success("pase_lista${pl_key}: OK ({nrow(pl)} registros).")
    }
  }

  resultados[[proy$nombre]] <- list(
    ok                      = TRUE,
    n_actividad             = nrow(insumos$bd_actividad),
    n_bd_aux                = nrow(insumos$bd_aux),
    n_sin_coord             = nrow(sin_coord),
    n_brigadas_coord_invalido = nrow(brigadas_coord_invalido),
    n_sin_brigada           = n_sin_brigada,
    warnings_etl            = warnings_etl,
    warnings_brigada        = warns_brigada
  )
}

# =========================================================================
# 5. RESULTADO GENERAL
# =========================================================================

cli::cli_h1("RESULTADO GENERAL — DEVSVNET-V2")

for (nombre in names(resultados)) {
  r <- resultados[[nombre]]
  cli::cli_h2("{nombre}")

  if (!r$ok) {
    cli::cli_alert_danger("FALLÓ: {r$error}")
    next
  }

  cli::cli_bullets(c(
    "i" = "Registros de actividad : {r$n_actividad}",
    "i" = "Filas en bd_aux        : {r$n_bd_aux}"
  ))

  # Inconsistencias
  if (r$n_sin_coord == 0 && r$n_brigadas_coord_invalido == 0) {
    cli::cli_alert_success("Sin inconsistencias de coordinador. Cambio de plataforma RESUELVE el issue.")
  } else {
    cli::cli_alert_warning(
      "{r$n_sin_coord} vocero(s) sin coordinador | {r$n_brigadas_coord_invalido} brigada(s) con IdUsuario inválido"
    )
  }

  if (r$n_sin_brigada == 0) {
    cli::cli_alert_success("Sin registros de actividad sin brigada resuelta.")
  } else {
    cli::cli_alert_warning("{r$n_sin_brigada} registro(s) de actividad sin brigada resuelta.")
  }

  # Warnings internos capturados
  all_warns <- c(r$warnings_etl, r$warnings_brigada)
  if (length(all_warns) == 0) {
    cli::cli_alert_success("Sin warnings internos.")
  } else {
    cli::cli_alert_warning("{length(all_warns)} warning(s) capturado(s) durante el ETL:")
    for (w in all_warns) {
      cli::cli_bullets(c("*" = w))
    }
  }
}

cli::cli_alert_success("Test de DEVSVNET-V2 completado.")
} # fin ejecutar_test()

ejecutar_test()
