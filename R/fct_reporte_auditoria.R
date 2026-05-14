# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_reporte_auditoria.R
# OBJETIVO: Extracción, cálculo y exportación del reporte de auditoría
# AUTOR:    Rafael López / Equipo de Análisis
# FECHA:    2026-03-24
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno / Crítico (Activos de Información Tipo A)
# -------------------------------------------------------------------------
# NOTAS DE SEGURIDAD:
# - No hardcodear credenciales. Usar .Renviron.
# - Las descargas de la base de datos están optimizadas (Pushdown) para
#   no saturar la memoria RAM del servidor.
# - Regla de Negocio: Se excluyen explícitamente los voceros inactivos.
# -------------------------------------------------------------------------

#' Generar Métricas del Reporte de Auditoría
#'
#' @description
#' Extrae y procesa las evaluaciones de calidad (auditorías) realizadas a los
#' registros de campo en una ventana temporal. Desanida la información en formato JSON
#' y cruza los resultados con el catálogo de voceros activos.
#'
#' **Lógica de Negocio:**
#' * **Voceros Inactivos:** Por requerimiento directivo, los voceros con `status == FALSE`
#'   son excluidos del reporte, incluso si tuvieron auditorías en esa semana.
#' * **Eliminados:** Las auditorías con dictamen "Eliminada" se penalizan forzando
#'   su calificación total a 0.
#'
#' @param pool Conexión activa a la base de datos (DBI).
#' @param bd_completa Tibble. Hechos de actividad operativa (registros).
#' @param brigadas Tibble. Catálogo de brigadas.
#' @param voceros Tibble. Catálogo de voceros.
#' @param corte Date. Fecha de corte del reporte. Solo se usa como fallback; en modo dinámico la semana se ancla a la última fecha con auditorías reales en `EvaluacionRegistro`.
#' @param week_start Numeric. Día de inicio de la semana (1 = Lunes, 6 = Sábado, 7 = Domingo). Por defecto 1.
#' @param fecha_inicio Date. Límite inferior de la ventana. Si es `NULL`, se calcula dinámicamente usando `corte` y `week_start`.
#' @param fecha_fin Date. Límite superior de la ventana. Si es `NULL`, se calcula dinámicamente usando `corte` y `week_start`.
#' @param excluir_brigadas Character vector. Nombres o fragmentos de nombres de brigadas a excluir. Por defecto `"CAPACITACIONES"`.
#'
#' @return Lista con dos tibbles: `res_auditoria` (promedios por vocero) y `observaciones` (detalle de incidencias).
#' @export
#' Generar Métricas del Reporte de Auditoría (Semanal + Histórico)
#'
#' @description
#' Extrae y procesa las evaluaciones de calidad. Ahora incluye el desglose de 
#' dictámenes (Óptimo, Aceptable, Deficiente) tanto para el corte semanal 
#' como para el acumulado histórico.
#'
#' @return Lista con tres tibbles: `res_auditoria`, `observaciones` y `res_auditoria_hist`.
#' @export
generar_metricas_auditoria <- function(
  pool,
  bd_completa,
  brigadas,
  voceros,
  corte, # Se asume que 'corte' es el domingo que se ejecuta el script
  week_start = 1,
  excluir_brigadas = "CAPACITACIONES"
) {
  # 1. Definición de Ventanas Temporales Específicas
  # Si corte es Domingo 2026-03-29:
  
  # Ventana Auditoría: Lunes a Sábado de la semana corriente
  # lunes_auditoria = 2026-03-23 | domingo_auditoria (excluyente) = 2026-03-29
  fecha_inicio_au <- lubridate::floor_date(as.Date(corte), unit = "week", week_start = 1)
  fecha_fin_au    <- as.Date(corte) # El domingo es el límite superior (no se incluye el domingo)

  # Ventana Efectivos: Viernes (semana pasada) a Jueves (semana corriente)
  # Si hoy es Domingo, el jueves pasado es hoy - 3 días, y el viernes anterior es hoy - 9 días.
  fecha_fin_ef    <- as.Date(corte) - 2 # El viernes (excluyente), llega hasta el jueves 23:59
  fecha_inicio_ef <- fecha_fin_ef - 6  # 7 días atrás desde el viernes (inicia el viernes anterior)

  # 2. Preparación de IDs
  # Semanal (Basado en ventana de Auditoría: Lunes-Sábado)
  registros_id_sem <- bd_completa |>
    dplyr::summarise(fecha = min(fecha, na.rm = TRUE), usuario_num = dplyr::first(usuario_num), .by = id) |>
    dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au) |>
    dplyr::transmute(id = as.integer(id), usuario_num)

  # Histórico (Sin cambios, todos los IDs)
  registros_id_hist <- bd_completa |>
    dplyr::summarise(fecha = min(fecha, na.rm = TRUE), usuario_num = dplyr::first(usuario_num), .by = id) |>
    dplyr::transmute(id = as.integer(id), usuario_num)

  if (nrow(registros_id_sem) == 0) {
    cli::cli_abort("No hay registros auditables en la ventana de lunes a sábado ({fecha_inicio_au} a {fecha_fin_au}).")
  }

  # 3. Extracción y Parseo de JSON
  evaluacion_raw <- dplyr::tbl(pool, "EvaluacionRegistro") |>
    dplyr::collect() |>
   dplyr::mutate(
      json_parseado = purrr::map(Resultado, \(x) {
        lista_cruda <- jsonlite::fromJSON(x)
        # Limpieza: Convertimos elementos con múltiples valores a un solo texto
        lista_limpia <- purrr::map(lista_cruda, \(item) {
          if (length(item) == 0) {
            NA
          } else if (length(item) > 1) {
            # Si tiene más de un valor (como tus 3 preguntas), los pega con una coma
            paste(as.character(item), collapse = ", ")
          } else {
            item
          }
        })
        tibble::as_tibble_row(lista_limpia)
      })
    )
    tidyr::unnest_wider(json_parseado) |>
    dplyr::mutate(fecha = as.Date(Fecha)) |> 
    dplyr::select(-Resultado) # <--- ELIMINADO EL |> AQUÍ

  for (col in c("dictamenFinal", "totalEvaluacion", "observaciones")) {
    if (!col %in% names(evaluacion_raw)) evaluacion_raw[[col]] <- character()
  }

  # 4. Procesamiento SEMANAL (Lunes-Sábado)
  evaluacion_sem <- evaluacion_raw |>
    dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au) 

  # Cambiamos evaluacion_raw por evaluacion_sem aquí:
  bd_prom_sem <- evaluacion_sem |> 
    dplyr::mutate(totalEvaluacion = dplyr::if_else(dictamenFinal == "Eliminada", "0", as.character(totalEvaluacion))) |>
    dplyr::summarise(
      total = round(mean(as.numeric(totalEvaluacion), na.rm = TRUE), 1),
      dialogos_auditados = dplyr::n(),
      optimos     = sum(dictamenFinal == "Diálogo Óptimo", na.rm = TRUE),
      aceptables  = sum(dictamenFinal == "Diálogo Aceptable", na.rm = TRUE),
      deficientes = sum(dictamenFinal == "Diálogo Deficiente", na.rm = TRUE),
      eliminados  = sum(dictamenFinal == "Eliminada", na.rm = TRUE),
      .by = usuario_num
    )
  
  # 5. Procesamiento HISTÓRICO
  evaluacion_hist <- evaluacion_raw |>
    dplyr::inner_join(registros_id_hist, by = dplyr::join_by(RegistroId == id))

  bd_prom_hist <- evaluacion_hist |>
    dplyr::mutate(totalEvaluacion = dplyr::if_else(dictamenFinal == "Eliminada", "0", as.character(totalEvaluacion))) |>
    dplyr::summarise(
      total = round(mean(as.numeric(totalEvaluacion), na.rm = TRUE), 1),
      dialogos_auditados = dplyr::n(),
      optimos     = sum(dictamenFinal == "Diálogo Óptimo", na.rm = TRUE),
      aceptables  = sum(dictamenFinal == "Diálogo Aceptable", na.rm = TRUE),
      deficientes = sum(dictamenFinal == "Diálogo Deficiente", na.rm = TRUE),
      eliminados  = sum(dictamenFinal == "Eliminada", na.rm = TRUE),
      .by = usuario_num
    )

  # 6. Catálogos y Efectivos (Nueva Lógica de Fechas)
  voceros_au <- brigadas |>
    dplyr::select(id_brigada, nombre_brigada) |>
    dplyr::left_join(voceros |> dplyr::select(nombre_completo, num, id_brigada, status), by = "id_brigada") |>
    dplyr::filter(status == TRUE) |>
    dplyr::rename(usuario_num = num)

  # Aplicando ventana Viernes a Jueves
  efectivos_sem <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_ef & fecha < fecha_fin_ef) |>
    dplyr::summarise(efectivos = sum(desglose == "Efectivo", na.rm = TRUE), 
                     fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)), .by = usuario_num)

  # El histórico de efectivos sigue siendo global
  efectivos_hist <- bd_completa |>
    dplyr::summarise(efectivos = sum(desglose == "Efectivo", na.rm = TRUE), 
                     fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)), .by = usuario_num)

  # 7. Ensamblado de Tablas Finales
  res_auditoria <- voceros_au |>
    dplyr::inner_join(bd_prom_sem, by = "usuario_num") |>
    dplyr::left_join(efectivos_sem, by = "usuario_num") |>
    dplyr::rename(`Promedio de evaluaciones` = total) |>
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dialogos_auditados)

  res_auditoria_hist <- voceros_au |>
    dplyr::inner_join(bd_prom_hist, by = "usuario_num") |>
    dplyr::left_join(efectivos_hist, by = "usuario_num") |>
    dplyr::rename(`Promedio de evaluaciones` = total) |>
    dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
    dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dialogos_auditados)

  observaciones <- voceros_au |>
    dplyr::inner_join(
      evaluacion_sem |> dplyr::select(RegistroId, dplyr::any_of(c("Fecha", "fecha")), 
                                      usuario_num, observaciones, dictamenFinal),
      by = "usuario_num"
    )

  # 8. Filtro de Brigadas
  if (!is.null(excluir_brigadas) && length(excluir_brigadas) > 0) {
    patron <- paste(excluir_brigadas, collapse = "|")
    res_auditoria <- dplyr::filter(res_auditoria, !grepl(patron, nombre_brigada, ignore.case = TRUE))
    res_auditoria_hist <- dplyr::filter(res_auditoria_hist, !grepl(patron, nombre_brigada, ignore.case = TRUE))
    observaciones <- dplyr::filter(observaciones, !grepl(patron, nombre_brigada, ignore.case = TRUE))
  }

  return(list(
    res_auditoria = res_auditoria,
    observaciones = observaciones,
    res_auditoria_hist = res_auditoria_hist
  ))
}

#' Create Excel Workbook for Audit Report
#'
#' @description
#' Receives the audit metrics generated by `generar_metricas_auditoria`
#' and builds an Excel workbook object (`Workbook`), applying conditional formatting
#' (color scale) to the averages column.
#'
#' **Note:** This function does not save the file to disk. It returns the workbook
#' object in memory so it can be passed to downstream upload/save functions.
#'
#' @param datos_auditoria List. Output from the `generar_metricas_auditoria()` function.
#'
#' @return An `openxlsx` Workbook object.
#' @export
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
