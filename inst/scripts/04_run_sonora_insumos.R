# =========================================================================
# SCRIPT: 04_run_sonora_insumos.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Carga y construcción de insumos para el proyecto Sonora
#           (Hermosillo). Equivalente a la lógica de producción local,
#           pero usando las funciones exportadas de DialogaR.
#
# DEPENDENCIAS EXTERNAS:
#   - Credenciales en .Renviron (ver README)
#   - Acceso a Google Drive con googledrive::drive_auth()
#   - Tabla en BD: snapshot_id_285 (Lorenia/Hermosillo)
#   - Cuestionario de pase de lista: 287
# =========================================================================

library(DialogaR)
library(dplyr)
library(lubridate)
library(googledrive)

# =========================================================================
# 1. PARÁMETROS
# =========================================================================

corte <- Sys.Date() - 1
id_proyecto <- 26L

# Carpeta de Drive con archivos de referencia del proyecto
drive_folder <- as_id(
  "https://drive.google.com/drive/u/0/folders/1OJAFQd3qrQOOi4mW_aSb4yM5ewmwPIcr"
)

fecha_min_pl <- as.Date("2026-02-16")
fecha_min_actividad <- as.Date("2025-02-16")

# =========================================================================
# 2. REGLAS DE NEGOCIO LOCALES
# =========================================================================

# Filtro mínimo sobre la consulta SQL (antes de collect)
filtro_minimo_actividad <- function(q, f) {
  q |> filter(!is.na(seccion), seccion != "")
}

# Normalizador post-collect: garantiza tipos consistentes
normalizador_actividad <- function(df, f) {
  df |> mutate(is_complete = as.character(is_complete))
}

# Parches puntuales sobre bd_completa (correcciones manuales de datos)
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

# Listar contenido de la carpeta una sola vez
files_tbl <- googledrive::drive_ls(drive_folder) |>
  select(name, id)

# Verificar que todos los archivos requeridos existan
need <- c(
  "secciones_ds.xlsx",
  "sonora.rda",
  "rango_edad.rds",
  "shp_mun.rda",
  "Plantilla_MORANT.pptx"
)

missing_files <- setdiff(need, files_tbl$name)
if (length(missing_files) > 0) {
  cli::cli_abort("Faltan archivos en Drive: {paste(missing_files, collapse=', ')}")
}

dups <- files_tbl |>
  filter(name %in% need) |>
  dplyr::count(name) |>
  filter(n > 1) |>
  pull(name)
if (length(dups) > 0) {
  cli::cli_abort("Nombres duplicados en Drive: {paste(dups, collapse=', ')}")
}

# Resolver rutas locales y descargar
need_tbl <- files_tbl |>
  filter(name %in% need) |>
  mutate(local_path = file.path(tempdir(), name))

for (i in seq_len(nrow(need_tbl))) {
  googledrive::drive_download(
    file = googledrive::as_id(need_tbl$id[i]),
    path = need_tbl$local_path[i],
    overwrite = TRUE
  )
}

# Cargar en entorno
metas <- readxl::read_xlsx(
  need_tbl$local_path[need_tbl$name == "secciones_ds.xlsx"],
  sheet = "Equipo 1"
) |> janitor::clean_names()

sonora    <- readr::read_rds(need_tbl$local_path[need_tbl$name == "sonora.rda"])
rango_edad <- readr::read_rds(need_tbl$local_path[need_tbl$name == "rango_edad.rds"])
shp_mun   <- readr::read_rds(need_tbl$local_path[need_tbl$name == "shp_mun.rda"])
pptx      <- officer::read_pptx(need_tbl$local_path[need_tbl$name == "Plantilla_MORANT.pptx"])

# Geometría de secciones filtrada a Hermosillo
shp_secc <- sonora$info$shp$seccion |>
  dplyr::left_join(
    dplyr::as_tibble(dplyr::select(shp_mun, dplyr::contains("municipio"))),
    by = dplyr::join_by(municipio_24)
  ) |>
  dplyr::filter(nombre_municipio_24 == "HERMOSILLO")

# Tabla de zonas auxiliares para cruce geográfico
aux_zonas <- dplyr::as_tibble(shp_secc) |>
  dplyr::left_join(dplyr::as_tibble(shp_mun), by = dplyr::join_by(municipio_24)) |>
  dplyr::transmute(
    seccion = stringr::str_replace(seccion, "08_", "")
  )

cli::cli_alert_success("Archivos de referencia cargados.")

# =========================================================================
# 4. CONEXIÓN A BASE DE DATOS
# =========================================================================

pool <- conectar_base_datos()

# =========================================================================
# 5. ETL: CARGA DE INSUMOS
# =========================================================================

cli::cli_h1("Iniciando Extracción ETL")

procesador_pl <- function(pool, id_cuestionario) {
  out <- procesar_pase_lista(
    pool = pool,
    id_cuestionario = id_cuestionario,
    fecha_min = fecha_min_pl
  )
  out$pase_lista
}

insumos <- cargar_insumos(
  pool = pool,
  id_proyecto = id_proyecto,
  corte = corte,
  fuentes_actividad = list(
    list(tabla = "snapshot_id_285", origen = "Lorenia")
  ),
  ids_pase_lista = c(287L),
  procesador_pl = procesador_pl,
  fecha_min_actividad = fecha_min_actividad,
  filtro_minimo_actividad = filtro_minimo_actividad,
  normalizador_actividad = normalizador_actividad,
  postprocess_insumos = NULL
)

cli::cli_alert_success(
  "Insumos cargados: {nrow(insumos$bd_actividad)} registros de actividad."
)

# =========================================================================
# 6. CONSTRUCCIÓN DE bd_completa Y OBJETOS AUXILIARES
# =========================================================================

cli::cli_h1("Construyendo objetos de análisis")

bd_completa <- insumos$bd_actividad |>
  dplyr::left_join(aux_zonas, by = "seccion") |>
  dplyr::filter(desglose != "ERROR", fecha >= fecha_min_actividad) |>
  parches_bd()

bd_aux <- insumos$bd_aux
brigadas <- insumos$cat$brigadas
voceros  <- insumos$cat$usuarios

coordinadores <- bd_aux |>
  dplyr::filter(!is.na(supervisor), supervisor != "-") |>
  dplyr::distinct(nombre_coordinador, .keep_all = TRUE) |>
  dplyr::mutate(usuario_num = supervisor)

cli::cli_alert_success(
  "bd_completa: {nrow(bd_completa)} registros | voceros: {nrow(voceros)} | coordinadores: {nrow(coordinadores)}"
)

# =========================================================================
# 7. VALIDACIÓN
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
  if (ok) cli::cli_alert_success("{nombre}: OK ({nrow(objeto)} filas)")
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
    "nombre_brigada",
    "nombre_coordinador",
    "supervisor",
    "nombre_vocero",
    "vocero"
  )
)

validar("voceros",       voceros,       nrow_min = 1L)
validar("coordinadores", coordinadores, nrow_min = 1L,
        cols_requeridas = c("supervisor", "nombre_coordinador"))
validar("metas",         metas,         nrow_min = 1L)

pl_287 <- insumos$pase_lista$pl_287
if (is.null(pl_287) || nrow(pl_287) == 0) {
  cli::cli_alert_warning("pase_lista$pl_287: sin registros para el periodo")
} else {
  cli::cli_alert_success("pase_lista$pl_287: OK ({nrow(pl_287)} registros)")
}

# =========================================================================
# 8. CERRAR CONEXIÓN
# =========================================================================

pool::poolClose(pool)
cli::cli_alert_success("Pipeline de insumos completado.")
