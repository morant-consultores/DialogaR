# =========================================================================
# SCRIPT: 01_run_productividad_chihuahua.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Script de ejecución diaria para el reporte de Chihuahua
# =========================================================================

# 1. Load the engine!
library(DialogaR)
library(dplyr)
library(lubridate)

# 2. Define Parameters for today's run
corte <- Sys.Date() - 1
id_proyecto <- 26
municipios_validos <- c("CHIHUAHUA", "JUAREZ", "NUEVO CASAS GRANDES")

# 3. Define the specific Hook for this project (Business Rules)
hook_chihuahua <- function(insumos) {
  # We use the generic tool, but apply local rules
  insumos$bd_actividad <- insumos$bd_actividad |>
    filtros_particulares_chih(municipios = municipios_validos, corte = corte)

  return(insumos)
}

# 4. Connect to DB (Using your secure .Renviron method)
pool <- conectar_base_datos()

# 5. EXECUTE: Extract Data
cli::cli_h1("Iniciando Extracción ETL")
insumos_chih <- cargar_insumos(
  pool = pool,
  id_proyecto = id_proyecto,
  corte = corte,
  fuentes_actividad = list(list(tabla = "snapshot_id_285", origen = "Lorenia")),
  ids_pase_lista = c(287),
  procesador_pl = procesar_pase_lista,
  postprocess_insumos = hook_chihuahua # Injecting the hook!
)

# 6. EXECUTE: Build Dashboards
cli::cli_h1("Construyendo Tablas de Productividad")
dashboards <- construir_productividad_diaria(
  bd_actividad = insumos_chih$bd_actividad,
  bd_aux = insumos_chih$bd_aux,
  pl = insumos_chih$pase_lista$pl_287,
  corte = corte,
  distritos = c("06", "08")
)

# 7. EXECUTE: Export or Upload
# (Call your uploading function here)
cli::cli_alert_success("Pipeline completado exitosamente.")
