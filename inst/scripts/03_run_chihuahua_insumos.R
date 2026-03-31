# =========================================================================
# SCRIPT: 03_run_chihuahua_insumos.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Carga y construcción de insumos para el proyecto Chihuahua
#           (Capital + Sur). Equivalente a la lógica de producción local,
#           pero usando las funciones exportadas de DialogaR.
#
# DEPENDENCIAS EXTERNAS:
#   - Credenciales en .Renviron (ver README)
#   - Acceso a Google Drive con googledrive::drive_auth()
#   - Tablas en BD: snapshot_id_292 (capital), snapshot_id_293 (sur)
#   - Cuestionario de pase de lista: 210
# =========================================================================

library(DialogaR)
library(dplyr)
library(lubridate)
library(googledrive)

# =========================================================================
# 1. PARÁMETROS
# =========================================================================

corte <- Sys.Date() - 1
id_proyecto <- 17L

# Carpeta de Drive con archivos de referencia del proyecto
drive_folder <- as_id(
  "https://drive.google.com/drive/u/0/folders/11ZpRvCM0URybVuN8CLJbuEGb-H3-l3r2"
)

fecha_min_pl <- as.Date("2026-03-14") # Ajustar según el inicio del periodo de interés


# =========================================================================
# 2. REGLAS DE NEGOCIO LOCALES
# =========================================================================

# Filtros geográficos y temporales específicos para Chihuahua
filtros_particulares_chih <- function(bd, municipios, corte) {
  bd |>
    filter(
      toupper(municipio) %in% toupper(municipios),
      fecha <= corte
    )
}

# Parches puntuales en bd_completa (correcciones manuales de datos)
parches_bd <- function(bd) {
  # Agregar aquí correcciones conocidas, por ejemplo:
  # bd |> filter(!usuario_num %in% c("XXX"))   # bajas especiales
  bd
}

# =========================================================================
# 3. DESCARGA DE ARCHIVOS DE REFERENCIA DESDE DRIVE
# =========================================================================

cli::cli_h1("Descargando archivos de referencia desde Drive")

# Autenticación (usa caché si ya está autenticado)
googledrive::drive_auth()

# Directorio temporal para los archivos descargados
dir_tmp <- tempdir()

# Listar todos los archivos de la carpeta una sola vez
archivos_drive <- googledrive::drive_ls(drive_folder)

descargar_drive <- function(nombre_archivo) {
  ruta_local <- file.path(dir_tmp, nombre_archivo)
  archivo <- archivos_drive[archivos_drive$name == nombre_archivo, ]
  if (nrow(archivo) == 0) {
    cli::cli_abort("No se encontró '{nombre_archivo}' en la carpeta de Drive.")
  }
  googledrive::drive_download(
    file = archivo,
    path = ruta_local,
    overwrite = TRUE
  )
  ruta_local
}

# Archivos de referencia
path_plantilla <- descargar_drive("plantilla_chihuahua.pptx")
path_shp_secc <- descargar_drive("shp_secc.rda")
path_rango_edad <- descargar_drive("rango_edad.rds")
path_shp_mun <- descargar_drive("shp_mun.rda")
path_seccion_2020 <- descargar_drive("seccion_2020.rda")
path_shp_df <- descargar_drive("shp_df.rda")
path_metas <- descargar_drive("Metas DS 2da Etapa Mar-Jul2026.xlsx")
path_secc_comp <- descargar_drive("secciones_completas.csv")

# Cargar en entorno
shp_secc <- readr::read_rds(path_shp_secc)
shp_mun <- readr::read_rds(path_shp_mun)
seccion_2020 <- readr::read_rds(path_seccion_2020)
shp_df <- readr::read_rds(path_shp_df)
rango_edad <- readr::read_rds(path_rango_edad)
metas <- readxl::read_excel(path_metas)
secciones_completas <- readr::read_csv(path_secc_comp, show_col_types = FALSE)

municipios_validos <- metas |>
  dplyr::pull(Municipio) |>
  unique() |>
  toupper() |>
  stringi::stri_trans_general("Latin-ASCII")

cli::cli_alert_success("Archivos de referencia cargados.")

# =========================================================================
# 4. CONEXIÓN A BASE DE DATOS
# =========================================================================

pool <- conectar_base_datos()

# =========================================================================
# 5. ETL: CARGA DE INSUMOS
# =========================================================================

cli::cli_h1("Iniciando Extracción ETL")

# Wrapper de procesador_pl usando la nueva API de procesar_pase_lista
# (retorna data.table directamente, sin necesidad de extraer $pase_lista)
procesador_pl <- function(pool, id_cuestionario) {
  procesar_pase_lista(
    pool = pool,
    id_cuestionario = id_cuestionario,
    fecha_min = fecha_min_pl
  )
}

insumos <- cargar_insumos(
  pool = pool,
  id_proyecto = id_proyecto,
  corte = corte,
  fuentes_actividad = list(
    list(tabla = "snapshot_id_292", origen = "chih_capital"),
    list(tabla = "snapshot_id_293", origen = "chih_sur")
  ),
  ids_pase_lista = c(210L),
  procesador_pl = procesador_pl
)

cli::cli_alert_success(
  "Insumos cargados: {nrow(insumos$bd_actividad)} registros de actividad."
)

# =========================================================================
# 6. CONSTRUCCIÓN DE bd_completa Y OBJETOS AUXILIARES
# =========================================================================

cli::cli_h1("Construyendo objetos de análisis")

bd_completa <- insumos$bd_actividad |>
  filtros_particulares_chih(municipios = municipios_validos, corte = corte) |>
  parches_bd()

bd_aux <- insumos$bd_aux
brigadas <- insumos$cat$brigadas
voceros <- bd_aux |> filter(status_vocero == TRUE)
coordinadores <- bd_aux |>
  distinct(
    distrito,
    nombre_coordinador,
    supervisor,
    status_coord,
    nombre_brigada
  ) |>
  filter(!is.na(supervisor))

# Zonas auxiliares para cruces geográficos
aux_zonas <- bd_aux |>
  distinct(distrito, municipio, nombre_brigada)

cli::cli_alert_success(
  "bd_completa: {nrow(bd_completa)} registros | voceros: {nrow(voceros)} | coordinadores: {nrow(coordinadores)}"
)

# =========================================================================
# 7. VALIDACIÓN (comparar estructura con valores esperados)
# =========================================================================

cli::cli_h1("Validando estructura de objetos")

validar <- function(
  nombre,
  objeto,
  nrow_min = 1L,
  cols_requeridas = character()
) {
  ok <- TRUE
  if (!is.data.frame(objeto)) {
    cli::cli_alert_danger("{nombre}: NO es un data.frame")
    ok <- FALSE
  } else {
    if (nrow(objeto) < nrow_min) {
      cli::cli_alert_warning(
        "{nombre}: {nrow(objeto)} filas (esperado >= {nrow_min})"
      )
      ok <- FALSE
    }
    faltantes <- setdiff(cols_requeridas, names(objeto))
    if (length(faltantes) > 0) {
      cli::cli_alert_warning(
        "{nombre}: columnas faltantes — {paste(faltantes, collapse=', ')}"
      )
      ok <- FALSE
    }
  }
  if (ok) {
    cli::cli_alert_success("{nombre}: OK ({nrow(objeto)} filas)")
  }
  invisible(ok)
}

validar(
  "bd_completa",
  bd_completa,
  nrow_min = 1L,
  cols_requeridas = c("fecha", "usuario_num", "desglose", "duracion_minutos")
)

validar(
  "bd_aux",
  bd_aux,
  nrow_min = 1L,
  cols_requeridas = c(
    "distrito",
    "municipio",
    "nombre_brigada",
    "nombre_coordinador",
    "supervisor",
    "nombre_vocero",
    "vocero"
  )
)

validar(
  "voceros",
  voceros,
  nrow_min = 1L,
  cols_requeridas = c("vocero", "nombre_vocero")
)

validar(
  "coordinadores",
  coordinadores,
  nrow_min = 1L,
  cols_requeridas = c("supervisor", "nombre_coordinador")
)

validar("metas", metas, nrow_min = 1L)

# Pase de lista
pl_210 <- insumos$pase_lista$pl_210
if (is.null(pl_210) || nrow(pl_210) == 0) {
  cli::cli_alert_warning("pase_lista$pl_210: sin registros para el periodo")
} else {
  cli::cli_alert_success("pase_lista$pl_210: OK ({nrow(pl_210)} registros)")
}

# =========================================================================
# 8. CERRAR CONEXIÓN
# =========================================================================

pool::poolClose(pool)
cli::cli_alert_success("Pipeline de insumos completado.")
