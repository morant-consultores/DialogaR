# PROYECTO: DialogaR
# SCRIPT: fct_reporte_auditoria.R
# OBJETIVO: Extracción, cálculo y exportación del reporte de auditoría
# -------------------------------------------------------------------------

generar_reporte_metricas <- function(pool, 
                                     insumos, 
                                     bd_completa, 
                                     bd_aux, 
                                     id_proyecto, 
                                     corte, 
                                     dia_inicio_ef = "lunes", 
                                     dia_fin_ef = "domingo", 
                                     simular_domingo = FALSE,
                                     excluir_brigadas = NULL,
                                     filtrar_historicos = FALSE) { # <-- Nuevo parámetro condicional
  
  # --- 1. LÓGICA DE FECHAS ---
  corte_dt <- as.Date(corte)
  if (simular_domingo) {
    corte_dt <- lubridate::ceiling_date(corte_dt, unit = "week", week_start = 1) - 1
    cli::cli_alert_info("Modo prueba: corte ajustado a {corte_dt} (domingo)")
  }
  
  encontrar_fecha_exacta <- function(referencia, nombre_dia) {
    dias_ref <- c("lunes" = 1, "martes" = 2, "miercoles" = 3, "miércoles" = 3,
                  "jueves" = 4, "viernes" = 5, "sabado" = 6, "sábado" = 6, "domingo" = 7)
    target_num <- dias_ref[tolower(nombre_dia)]
    actual_num <- lubridate::wday(referencia, week_start = 1)
    diff <- actual_num - target_num
    if (diff < 0) diff <- diff + 7
    if (diff == 0) diff <- 7
    return(referencia - diff)
  }
  
  fecha_inicio_au <- lubridate::floor_date(corte_dt, unit = "week", week_start = 1)
  fecha_fin_au    <- corte_dt
  fecha_fin_ef    <- encontrar_fecha_exacta(fecha_inicio_au, dia_fin_ef)
  fecha_inicio_ef <- encontrar_fecha_exacta(fecha_fin_ef, dia_inicio_ef)
  rango_fechas    <- seq.Date(fecha_inicio_ef, fecha_fin_ef, by = "day")
  
  cli::cli_inform(c(
    "v" = "Intervalos de fecha calculados:",
    "i" = "Auditoría: {.val {fecha_inicio_au}} al {.val {fecha_fin_au}}",
    "i" = "Efectivos: {.val {fecha_inicio_ef}} al {.val {fecha_fin_ef}}"
  ))
  
  # --- 2. PREPARACIÓN ESTRUCTURA BASE ---
  bd_aux_clean <- bd_aux |>
    dplyr::mutate(dplyr::across(c(nombre_coordinador, supervisor, nombre_brigada), 
                                ~tidyr::replace_na(.x, "SIN ASIGNAR"))) |>
    dplyr::arrange(dplyr::desc(status_coord)) |>
    dplyr::distinct(vocero, .keep_all = TRUE)
  
  coord_nums <- insumos$cat$usuarios |>
    dplyr::filter(cargo == "Coordinador de Brigada") |>
    dplyr::pull(num)

  # --- 3. CÁLCULO DE PRODUCCIÓN (SEM VS HIST) ---
  # Semanal (para completar fechas)
  stats_sem <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_ef & fecha <= fecha_fin_ef) |>
    dplyr::summarise(n = sum(desglose == "Efectivo", na.rm = TRUE), .by = c(usuario_num, fecha))

  # Histórica (toda la base)
  stats_hist_base <- bd_completa |>
    dplyr::summarise(
      efectivos_totales = sum(desglose == "Efectivo", na.rm = TRUE),
      fecha_ultimo_reg_hist = as.Date(max(fecha, na.rm = TRUE)),
      .by = usuario_num
    )

  # Función de apoyo para ensamblar voceros/supervisores
  ensamblar_estructura <- function(stats_df) {
    rv <- bd_aux_clean |>
      dplyr::filter(!is.na(vocero), (status_vocero == TRUE) | (vocero %in% stats_df$usuario_num)) |>
      dplyr::left_join(stats_df, by = dplyr::join_by(vocero == usuario_num))
    
    rs <- bd_aux_clean |>
      dplyr::transmute(municipio, distrito, nombre_brigada, nombre_coordinador, supervisor,
                       status_coord, nombre_vocero = nombre_coordinador, vocero = supervisor, status_vocero = status_coord) |>
      dplyr::filter(status_coord == TRUE | vocero %in% stats_df$usuario_num) |>
      dplyr::distinct(distrito, nombre_coordinador, .keep_all = TRUE) |>
      dplyr::filter(vocero %in% coord_nums) |>
      dplyr::left_join(stats_df, by = dplyr::join_by(vocero == usuario_num))
    
    sup_list <- unique(rs$vocero[!is.na(rs$vocero)])
    dplyr::bind_rows(rs, rv |> dplyr::filter(!vocero %in% sup_list)) |> 
      dplyr::distinct()
  }

  # --- 4. CREACIÓN DE HOJAS DE REGISTROS ---
  # Semanal
  hoja_registros_sem <- ensamblar_estructura(stats_sem) |>
    dplyr::filter(!(nombre_brigada == "SIN ASIGNAR" & !vocero %in% stats_sem$usuario_num)) |>
    tidyr::complete(
      tidyr::nesting(municipio, distrito, nombre_brigada, nombre_coordinador, 
                    supervisor, status_coord, nombre_vocero, vocero, status_vocero),
      fecha = rango_fechas, fill = list(n = 0)
    ) |> 
    dplyr::summarise(
      efectivos = sum(n),
      fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)),
      .by = c(municipio, distrito, nombre_brigada, nombre_coordinador, 
              supervisor, status_coord, nombre_vocero, vocero, status_vocero)
    ) |>
    dplyr::select(nombre_brigada, nombre_vocero, vocero, status_vocero, efectivos, fecha_ultimo_registro)

  # Histórica
  hoja_registros_hist_base <- ensamblar_estructura(stats_hist_base) |>
    dplyr::filter(!(nombre_brigada == "SIN ASIGNAR" & !vocero %in% stats_hist_base$usuario_num)) |>
    dplyr::transmute(nombre_brigada, nombre_vocero, vocero, status_vocero, 
                     efectivos = efectivos_totales, 
                     fecha_ultimo_registro = fecha_ultimo_reg_hist)

  # --- 5. PROCESAMIENTO DE AUDITORÍAS ---
  registros_id_hist <- bd_completa |>
    dplyr::summarise(fecha = min(fecha, na.rm = TRUE), usuario_num = dplyr::first(usuario_num), .by = id) |>
    dplyr::transmute(id = as.integer(id), usuario_num)
  
  evaluacion_raw <- dplyr::tbl(pool, "EvaluacionRegistro") |>
    dplyr::collect() |>
    dplyr::inner_join(registros_id_hist, by = dplyr::join_by(RegistroId == id)) |>
    dplyr::mutate(
      json_parseado = purrr::map(Resultado, \(x) {
        lista_cruda <- jsonlite::fromJSON(x); lista_limpia <- purrr::map(lista_cruda, \(item) {
          if (length(item) == 0) return(NA); if (length(item) > 1) return(list(item)); return(item)
        })
        tibble::as_tibble_row(lista_limpia)
      }),
      fecha_hora_cdmx = lubridate::with_tz(Fecha, tzone = "America/Mexico_City"),
      fecha = as.Date(fecha_hora_cdmx)
    ) |>
    tidyr::unnest_wider(json_parseado)
  
  evaluacion_sem <- evaluacion_raw |> dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au)

  # --- 5.1 LÓGICA DE ACTUALIZACIÓN DEL ARCHIVO DE HISTÓRICOS (.RDS) ---
path_rds <- "data-raw/registros_auditoria.rds"
  
  if (filtrar_historicos) {
    # 1. Extraer los IDs de esta semana como un vector numérico plano
    reg_semana <- evaluacion_sem |> dplyr::pull(RegistroId)
    
    # 2. Cargar históricos existentes asegurando que se extraigan como vector plano
    if (file.exists(path_rds)) {
      reg_hist_previos <- readr::read_rds(path_rds)
      reg_hist_previos <- unlist(reg_hist_previos) # Asegura romper cualquier estructura de lista previa
    } else {
      reg_hist_previos <- integer() 
    }
    
    # 3. Combinar vectores y eliminar duplicados de forma directa
    reg_hist_actualizado <- unique(c(reg_hist_previos, reg_semana))
    
    # 4. Guardar el vector numérico limpio en el RDS
    readr::write_rds(reg_hist_actualizado, path_rds)
    
    cli::cli_alert_success("Base de históricos en RDS actualizada correctamente (Formato Vector).")
  }

  # --- 6. CÁLCULO DE MÉTRICAS ---
  calc_promedios <- function(df) {
    df |>
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
  }

  bd_prom_sem  <- calc_promedios(evaluacion_sem)
  
  # Filtrar evaluacion_raw si el parámetro condicional está activo
  if (filtrar_historicos) {
    evaluacion_hist_filtrada <- evaluacion_raw |> 
      dplyr::filter(RegistroId %in% reg_hist_actualizado) # <-- Filtrado directo por vector
    bd_prom_hist <- calc_promedios(evaluacion_hist_filtrada)
  }else {
    bd_prom_hist <- calc_promedios(evaluacion_raw)
  }

  # --- 7. ENSAMBLE DE HOJAS FINALES ---
  # Semanal
  res_auditoria <- hoja_registros_sem |>
    dplyr::left_join(bd_prom_sem, by = c("vocero" = "usuario_num")) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))) |> 
    dplyr::mutate(`total` = dplyr::if_else(efectivos == 0, NaN, total)) |> 
    dplyr::rename(`Promedio de evaluaciones` = total) |> 
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::relocate(efectivos, .after = eliminados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dplyr::desc(dialogos_auditados))

  # Histórica (Filtrada o completa dependiendo de 'filtrar_historicos')
  res_auditoria_hist <- hoja_registros_hist_base |>
    dplyr::left_join(bd_prom_hist, by = c("vocero" = "usuario_num")) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))) |> 
    dplyr::mutate(`total` = dplyr::if_else(efectivos == 0, NaN, total)) |> 
    dplyr::rename(`Promedio de evaluaciones` = total) |> 
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::relocate(efectivos, .after = eliminados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dplyr::desc(dialogos_auditados))

  # Observaciones
  observaciones <- res_auditoria |>
    dplyr::select(nombre_brigada, nombre_vocero, vocero) |>
    dplyr::inner_join(
      evaluacion_sem |> dplyr::select(id = RegistroId, fecha, usuario_num, observaciones, dictamenFinal),
      by = c("vocero" = "usuario_num")
    )

  # --- 8. FILTRO DE EXCLUSIÓN ---
  if (!is.null(excluir_brigadas)) {
    patron <- paste(excluir_brigadas, collapse = "|")
    res_auditoria      <- dplyr::filter(res_auditoria,      !grepl(patron, nombre_brigada, ignore.case = TRUE))
    res_auditoria_hist <- dplyr::filter(res_auditoria_hist, !grepl(patron, nombre_brigada, ignore.case = TRUE))
    observaciones      <- dplyr::filter(observaciones,      !grepl(patron, nombre_brigada, ignore.case = TRUE))
  }

  return(list(
    res_auditoria = res_auditoria, 
    observaciones = observaciones, 
    res_auditoria_hist = res_auditoria_hist
  ))
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
