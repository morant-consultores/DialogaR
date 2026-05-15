# PROYECTO: DialogaR
# SCRIPT: fct_reporte_auditoria.R
# OBJETIVO: Extracción, cálculo y exportación del reporte de auditoría
# -------------------------------------------------------------------------

generar_metricas_auditoria <- function(
    pool,
    bd_completa,
    brigadas,
    voceros,
    corte, 
    dia_inicio_ef = "viernes", 
    dia_fin_ef = "jueves",
    excluir_brigadas = "CAPACITACIONES", 
    simular_domingo = F
) {
  
  # --- 1. LÓGICA DE VENTANAS TEMPORALES ---
corte_dt <- as.Date(corte)

# Si estás probando entre semana, avanza al domingo de esta semana
if (simular_domingo) {
  corte_dt <- lubridate::ceiling_date(corte_dt, unit = "week", week_start = 1) - 1
  cli::cli_alert_info("Modo prueba: corte ajustado a {corte_dt} (domingo)")
}

encontrar_fecha_exacta <- function(referencia, nombre_dia) {
  dias_ref <- c("lunes" = 1, "martes" = 2, "miercoles" = 3, "miércoles" = 3,
                "jueves" = 4, "viernes" = 5, "sabado" = 6, "sábado" = 6,
                "domingo" = 7)
  
  target_num <- dias_ref[tolower(nombre_dia)]
  actual_num <- lubridate::wday(referencia, week_start = 1)
  
  diff <- actual_num - target_num
  if (diff < 0) diff <- diff + 7
  if (diff == 0) diff <- 7
  
  return(referencia - diff)
}

# Ventana EFECTIVOS (semana anterior, configurable por proyecto)
fecha_inicio_au <- lubridate::floor_date(corte_dt, unit = "week", week_start = 1)
fecha_fin_au    <- corte_dt

# Ventana EFECTIVOS (semana anterior al lunes de auditoría)
# Anclamos desde fecha_inicio_au para garantizar que queda ANTES de la semana actual
fecha_fin_ef    <- encontrar_fecha_exacta(fecha_inicio_au, dia_fin_ef)
fecha_inicio_ef <- encontrar_fecha_exacta(fecha_fin_ef, dia_inicio_ef)

cli::cli_inform(c(
  "i" = "Ventana auditoría : {fecha_inicio_au} ({lubridate::wday(fecha_inicio_au, label=TRUE, abbr=FALSE)}) → {fecha_fin_au} ({lubridate::wday(fecha_fin_au, label=TRUE, abbr=FALSE)})",
  "i" = "Ventana efectivos  : {fecha_inicio_ef} ({lubridate::wday(fecha_inicio_ef, label=TRUE, abbr=FALSE)}) → {fecha_fin_ef} ({lubridate::wday(fecha_fin_ef, label=TRUE, abbr=FALSE)})"
))
  # --- 2. PREPARACIÓN DE DATOS BASE ---
  registros_id_hist <- bd_completa |>
    dplyr::summarise(fecha = min(fecha, na.rm = TRUE), usuario_num = dplyr::first(usuario_num), .by = id) |>
    dplyr::transmute(id = as.integer(id), usuario_num)

  # --- 3. EXTRACCIÓN Y PARSEO DE JSON (CON AJUSTE CDMX) ---
  evaluacion_raw <- registros_id_hist  |>
    dplyr::inner_join(dplyr::tbl(pool, "EvaluacionRegistro") |>
    dplyr::collect() , by = dplyr::join_by(id == RegistroId)) |>
    dplyr::mutate(
      json_parseado = purrr::map(Resultado, \(x) {
        lista_cruda <- jsonlite::fromJSON(x)
        lista_limpia <- purrr::map(lista_cruda, \(item) {
          if (length(item) == 0) return(NA)
          if (length(item) > 1) return(list(item)) 
          return(item)
        })
        tibble::as_tibble_row(lista_limpia)
      })
    ) |>
    tidyr::unnest_wider(json_parseado) |>
    dplyr::mutate(
      fecha_hora_cdmx = lubridate::with_tz(Fecha, tzone = "America/Mexico_City"),
    # 2. Extraemos solo la fecha (opcional, si lo necesitas para el conteo)
    fecha = as.Date(fecha_hora_cdmx)
    ) |>
    dplyr::select(-Resultado) |>
    (\(df) {
      for (col in c("dictamenFinal", "totalEvaluacion", "observaciones")) {
        if (!col %in% names(df)) df[[col]] <- NA_character_
      }
      return(df)
    })()

  browser()
  # --- 4. PROCESAMIENTO SEMANAL ---
  evaluacion_sem <- evaluacion_raw |>
    dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au)

  bd_prom_sem <- evaluacion_sem |>
    dplyr::mutate(totalEvaluacion = dplyr::if_else(dictamenFinal == "Eliminada", "0", as.character(totalEvaluacion))) |>
    dplyr::summarise(
      total = round(mean(as.numeric(totalEvaluacion), na.rm = TRUE), 1),
      dialogos_auditados = dplyr::n(),
      optimos = sum(dictamenFinal == "Diálogo Óptimo", na.rm = TRUE),
      aceptables = sum(dictamenFinal == "Diálogo Aceptable", na.rm = TRUE),
      deficientes = sum(dictamenFinal == "Diálogo Deficiente", na.rm = TRUE),
      eliminados = sum(dictamenFinal == "Eliminada", na.rm = TRUE),
      .by = usuario_num
    )


  # --- 5. PROCESAMIENTO HISTÓRICO ---
  bd_prom_hist <- evaluacion_raw |>
    dplyr::mutate(totalEvaluacion = dplyr::if_else(dictamenFinal == "Eliminada", "0", as.character(totalEvaluacion))) |>
    dplyr::summarise(
      total = round(mean(as.numeric(totalEvaluacion), na.rm = TRUE), 1),
      dialogos_auditados = dplyr::n(),
      optimos = sum(dictamenFinal == "Diálogo Óptimo", na.rm = TRUE),
      aceptables = sum(dictamenFinal == "Diálogo Aceptable", na.rm = TRUE),
      deficientes = sum(dictamenFinal == "Diálogo Deficiente", na.rm = TRUE),
      eliminados = sum(dictamenFinal == "Eliminada", na.rm = TRUE),
      .by = usuario_num
    )

  # --- 6. CATÁLOGOS Y EFECTIVOS ---
  voceros_au <- brigadas |>
    dplyr::select(id_brigada, nombre_brigada) |>
    dplyr::left_join(voceros |> dplyr::select(nombre_completo, num, id_brigada, status), by = "id_brigada") |>
    dplyr::filter(status == TRUE) |>
    dplyr::rename(usuario_num = num)

  efectivos_sem <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_ef & fecha <= fecha_fin_ef) |>
    dplyr::summarise(efectivos = sum(desglose == "Efectivo", na.rm = TRUE),
                     fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)), .by = usuario_num)

  efectivos_hist <- bd_completa |>
    dplyr::summarise(efectivos = sum(desglose == "Efectivo", na.rm = TRUE),
                     fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)), .by = usuario_num)

  # --- 7. ENSAMBLADO Y ORDENADO ---
  res_auditoria <- voceros_au |>
    dplyr::inner_join(bd_prom_sem, by = "usuario_num") |>
    dplyr::left_join(efectivos_sem, by = "usuario_num") |>
    dplyr::rename(`Promedio de evaluaciones` = total) |>
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dplyr::desc(dialogos_auditados))

  res_auditoria_hist <- voceros_au |>
    dplyr::inner_join(bd_prom_hist, by = "usuario_num") |>
    dplyr::left_join(efectivos_hist, by = "usuario_num") |>
    dplyr::rename(`Promedio de evaluaciones` = total) |>
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dplyr::desc(dialogos_auditados))

  observaciones <- voceros_au |>
    dplyr::inner_join(
      evaluacion_sem |> dplyr::select(id, fecha, usuario_num, observaciones, dictamenFinal),
      by = "usuario_num"
    )

  # --- 8. FILTRO DE BRIGADAS ---
  if (!is.null(excluir_brigadas)) {
    patron <- paste(excluir_brigadas, collapse = "|")
    res_auditoria <- dplyr::filter(res_auditoria, !grepl(patron, nombre_brigada, ignore.case = TRUE))
    res_auditoria_hist <- dplyr::filter(res_auditoria_hist, !grepl(patron, nombre_brigada, ignore.case = TRUE))
    observaciones <- dplyr::filter(observaciones, !grepl(patron, nombre_brigada, ignore.case = TRUE))
  }

  return(list(res_auditoria = res_auditoria, observaciones = observaciones, res_auditoria_hist = res_auditoria_hist))
}
# --- FUNCIÓN EXPORTACIÓN EXCEL ---
crear_workbook_auditoria <- function(datos_auditoria) {
  wb <- openxlsx::createWorkbook()
  
  hojas <- names(datos_auditoria)
  for (hoja in hojas) {
    openxlsx::addWorksheet(wb, hoja)
    openxlsx::writeData(wb, hoja, datos_auditoria[[hoja]])
    
    # Formato condicional para promedios
    col_total <- which(names(datos_auditoria[[hoja]]) == "Promedio de evaluaciones")
    if (length(col_total) > 0) {
      openxlsx::conditionalFormatting(wb, sheet = hoja, cols = col_total,
                                      rows = 2:(nrow(datos_auditoria[[hoja]]) + 1),
                                      style = c("#FF0000", "#00FF00"), type = "colourScale")
    }
  }
  
  cli::cli_alert_success("Libro de Excel generado exitosamente.")
  return(wb)
}
crear_workbook_auditoria <- function(datos_auditoria) {
# 1. Validate that the data has the correct structure (Fail-fast)
if (!all(c("res_auditoria", "observaciones") %in% names(datos_auditoria))) {
cli::cli_abort(
"The 'datos_auditoria' object must contain 'res_auditoria' and 'observaciones'."
)
}

# 2. Initialize workbook and add sheets
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "res_auditoria")
openxlsx::addWorksheet(wb, "observaciones")
openxlsx::addWorksheet(wb, "res_auditoria_hist")


# 3. Write data to sheets
openxlsx::writeData(wb, "res_auditoria", datos_auditoria$res_auditoria)
openxlsx::writeData(wb, "observaciones", datos_auditoria$observaciones)
openxlsx::writeData(wb, "res_auditoria_hist", datos_auditoria$res_auditoria_hist)


# 4. Apply conditional formatting (Color scale)
col_total <- which(
names(datos_auditoria$res_auditoria) == "Promedio de evaluaciones"
)

if (length(col_total) > 0) {
openxlsx::conditionalFormatting(
wb,
sheet = "res_auditoria",
cols = col_total,
rows = 2:(nrow(datos_auditoria$res_auditoria) + 1), # +1 to account for the header
style = c("#FF0000", "#00FF00"), # Red to Green
type = "colourScale"
)
}

col_total <- which(
names(datos_auditoria$res_auditoria_hist) == "Promedio de evaluaciones"
)

if (length(col_total) > 0) {
openxlsx::conditionalFormatting(
wb,
sheet = "res_auditoria_hist",
cols = col_total,
rows = 2:(nrow(datos_auditoria$res_auditoria_hist) + 1), # +1 to account for the header
style = c("#FF0000", "#00FF00"), # Red to Green
type = "colourScale"
)
}

cli::cli_alert_success("Excel workbook successfully created in memory.")

# 5. Return the object to the user's session
return(wb)
}
