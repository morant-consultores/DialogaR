# =========================================================================
# SCRIPT: 05_run_sonora_reporte_semanal.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Validación del reporte semanal de productividad (Sonora/Hermosillo)
#           con la lógica corregida de deduplicación y verificación de integridad.
#
# PRERREQUISITO: Haber ejecutado 04_run_sonora_insumos.R en la misma sesión.
#   Objetos requeridos en el entorno: bd_completa, bd_aux, insumos, corte, id_proyecto
# =========================================================================

library(DialogaR)
library(dplyr)

# =========================================================================
# 1. VERIFICAR PRERREQUISITOS
# =========================================================================

prereqs <- c("bd_completa", "bd_aux", "insumos", "corte", "id_proyecto")
faltantes <- prereqs[!sapply(prereqs, exists)]
if (length(faltantes) > 0) {
  cli::cli_abort(c(
    "Faltan objetos del entorno. Ejecuta primero 04_run_sonora_insumos.R",
    "x" = "Faltantes: {paste(faltantes, collapse = ', ')}"
  ))
}

# =========================================================================
# 2. RECONECTAR A LA BASE DE DATOS
# =========================================================================

pool <- conectar_base_datos()

# =========================================================================
# 3. GENERAR REPORTE SEMANAL
# =========================================================================

cli::cli_h1("Generando reporte semanal de productividad")

reporte_semanal <- generar_reporte_brigadas(
  reporte       = "semanal",
  corte         = corte,
  id_proyecto   = id_proyecto,
  pool          = pool,
  bd_completa   = bd_completa,
  bd_aux        = bd_aux,
  insumos       = insumos,
  week_start    = 6,
  umbral_cortos = 2
)

registros <- reporte_semanal$registros
rango     <- reporte_semanal$rango_fechas

# =========================================================================
# 4. VALIDACIONES EXTERNAS
# =========================================================================

cli::cli_h1("Validando integridad del reporte")

# 4.1 Sin voceros duplicados -----------------------------------------------
voceros_dup <- registros |>
  filter(!is.na(vocero), vocero != "SIN ASIGNAR") |>
  dplyr::count(vocero, sort = TRUE) |>
  filter(n > 1)

if (nrow(voceros_dup) > 0) {
  cli::cli_alert_danger(
    "{nrow(voceros_dup)} vocero(s) duplicado(s) en el reporte:"
  )
  print(voceros_dup)
} else {
  cli::cli_alert_success("Sin voceros duplicados.")
}

# 4.2 Total por fila == suma de columnas de fecha --------------------------
totales_inconsistentes <- registros |>
  dplyr::rowwise() |>
  dplyr::mutate(
    total_calculado = sum(
      dplyr::c_across(dplyr::any_of(as.character(rango))),
      na.rm = TRUE
    ),
    coincide = total_calculado == Total
  ) |>
  dplyr::ungroup() |>
  filter(!coincide)

if (nrow(totales_inconsistentes) > 0) {
  cli::cli_alert_danger(
    "{nrow(totales_inconsistentes)} fila(s) con columna Total inconsistente:"
  )
  print(dplyr::select(
    totales_inconsistentes,
    vocero, nombre_vocero, Total, total_calculado
  ))
} else {
  cli::cli_alert_success("Columna Total consistente en todos los registros.")
}

# 4.3 Registros efectivos en bd_completa vs Total del reporte --------------
n_esperado <- bd_completa |>
  filter(
    fecha >= min(rango),
    fecha <= max(rango),
    desglose == "Efectivo"
  ) |>
  nrow()

n_real <- sum(registros$Total, na.rm = TRUE)

if (n_real != n_esperado) {
  cli::cli_alert_danger(
    "Totales no coinciden: bd_completa tiene {n_esperado} efectivos, reporte suma {n_real} (delta: {n_real - n_esperado})."
  )
} else {
  cli::cli_alert_success(
    "Total de registros efectivos verificado: {n_real} == {n_esperado}."
  )
}

# =========================================================================
# 5. RESUMEN
# =========================================================================

cli::cli_h2("Resumen del reporte")
cli::cli_bullets(c(
  "*" = "Rango: {format(min(rango))} \u2014 {format(max(rango))}",
  "*" = "Filas en el reporte            : {nrow(registros)}",
  "*" = "Voceros \u00fanicos (sin SIN ASIGNAR): {sum(!is.na(registros$vocero) & registros$vocero != 'SIN ASIGNAR')}",
  "*" = "Total registros efectivos       : {n_real}"
))

# =========================================================================
# 6. CERRAR CONEXIÓN
# =========================================================================

pool::poolClose(pool)
cli::cli_alert_success("Validaci\u00f3n completada.")
