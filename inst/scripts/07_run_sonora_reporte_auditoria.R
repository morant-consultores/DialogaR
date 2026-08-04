# =========================================================================
# SCRIPT: 07_run_sonora_reporte_auditoria.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Generar el reporte de auditoría (encuesta 285, Lorenia/Hermosillo)
#           para la semana operativa del 25 al 31 de julio de 2026.
#
#           La semana operativa de este proyecto es sábado-viernes, por lo
#           que no coincide con la semana natural lunes-domingo que
#           generar_reporte_metricas() calcula por defecto a partir de
#           `corte`. Se usa el rango manual (fecha_inicio_auditoria /
#           fecha_fin_auditoria) para fijar exactamente el 25-31 de julio.
#
#           Se combinan ambas fuentes de auditoría (fuente_auditoria =
#           "combinar"): el histórico legado (EvaluacionRegistro) y las
#           auditorías del bot (ResultadoAuditoriaBot), privilegiando el
#           bot cuando un mismo registro fue auditado por ambos — esto es
#           lo que da continuidad histórica al reporte de Lorenia.
#
# PRERREQUISITO: Haber ejecutado 04_run_sonora_insumos.R en la misma sesión.
#   Objetos requeridos en el entorno: bd_completa, bd_aux, insumos
#
# DEPENDENCIAS EXTERNAS:
#   - DialogaR con la rama feat/auditoria-bot-fuente (fuente_auditoria,
#     fecha_inicio_auditoria/fecha_fin_auditoria en generar_reporte_metricas())
#   - Credenciales en .Renviron
# =========================================================================

library(DialogaR)

if (!"fuente_auditoria" %in% names(formals(generar_reporte_metricas))) {
  stop(
    "Este script requiere la version de DialogaR de la rama feat/auditoria-bot-fuente ",
    "(agrega fuente_auditoria y fecha_inicio_auditoria/fecha_fin_auditoria a generar_reporte_metricas()).",
    call. = FALSE
  )
}

library(dplyr)

# =========================================================================
# 1. VERIFICAR PRERREQUISITOS
# =========================================================================

prereqs <- c("bd_completa", "bd_aux", "insumos")
faltantes <- prereqs[!sapply(prereqs, exists)]
if (length(faltantes) > 0) {
  cli::cli_abort(c(
    "Faltan objetos del entorno. Ejecuta primero 04_run_sonora_insumos.R",
    "x" = "Faltantes: {paste(faltantes, collapse = ', ')}"
  ))
}

# =========================================================================
# 2. PARÁMETROS
# =========================================================================

encuesta_id             <- 285L
fecha_inicio_auditoria  <- as.Date("2026-07-25")
fecha_fin_auditoria     <- as.Date("2026-07-31")
corte                   <- fecha_fin_auditoria
excluir_brigadas        <- NULL

# Ruta del acumulado histórico (RDS) del reporte de Lorenia. Ajustar antes
# de ejecutar si el reporte debe mantener continuidad con corridas previas;
# dejar en NULL para no filtrar/actualizar histórico en esta corrida.
path_historicos <- NULL

# =========================================================================
# 3. RECONECTAR A LA BASE DE DATOS
# =========================================================================

pool <- conectar_base_datos()

# =========================================================================
# 4. GENERAR REPORTE DE AUDITORÍA
# =========================================================================

cli::cli_h1("Generando reporte de auditoría: {fecha_inicio_auditoria} a {fecha_fin_auditoria}")

reporte_auditoria <- generar_reporte_metricas(
  pool                    = pool,
  insumos                 = insumos,
  bd_completa             = bd_completa,
  bd_aux                  = bd_aux,
  encuesta_id             = encuesta_id,
  corte                   = corte,
  fecha_inicio_auditoria  = fecha_inicio_auditoria,
  fecha_fin_auditoria     = fecha_fin_auditoria,
  excluir_brigadas        = excluir_brigadas,
  filtrar_historicos      = !is.null(path_historicos),
  path_historicos         = path_historicos,
  fuente_auditoria        = "combinar"
)

cli::cli_alert_success(
  "Reporte generado: {nrow(reporte_auditoria$res_auditoria)} voceros/brigadas en la semana, {nrow(reporte_auditoria$observaciones)} observaciones."
)

# =========================================================================
# 5. CONSTRUIR Y GUARDAR WORKBOOK
# =========================================================================

wb <- crear_workbook_auditoria(reporte_auditoria)

nombre_archivo <- sprintf(
  "Reporte_Auditoria_%s_%s_a_%s.xlsx",
  encuesta_id, fecha_inicio_auditoria, fecha_fin_auditoria
)
ruta_salida <- file.path(getwd(), nombre_archivo)

openxlsx::saveWorkbook(wb, ruta_salida, overwrite = TRUE)
cli::cli_alert_success("Workbook guardado en: {ruta_salida}")

# =========================================================================
# 6. CERRAR CONEXIÓN
# =========================================================================

pool::poolClose(pool)
cli::cli_alert_success("Reporte de auditoría completado.")
