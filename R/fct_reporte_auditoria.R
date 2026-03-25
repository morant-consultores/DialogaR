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
#' @param corte Date. Fecha de corte del reporte.
#' @param week_start Numeric. Día de inicio de la semana (1 = Lunes, 6 = Sábado, 7 = Domingo). Por defecto 1.
#' @param excluir_brigadas Character vector. Nombres o fragmentos de nombres de brigadas a excluir. Por defecto `"CAPACITACIONES"`.
#'
#' @return Lista con dos tibbles: `res_auditoria` (promedios por vocero) y `observaciones` (detalle de incidencias).
#' @export
generar_metricas_auditoria <- function(
  pool,
  bd_completa,
  brigadas,
  voceros,
  corte,
  week_start = 1,
  excluir_brigadas = "CAPACITACIONES"
) {
  # 1. Definición de Ventana Temporal
  fecha_inicio_au <- lubridate::floor_date(
    corte,
    unit = "week",
    week_start = week_start
  )
  fecha_fin_au <- lubridate::ceiling_date(
    corte,
    unit = "week",
    week_start = week_start
  )

  # 2. Identificación de IDs operados en la semana
  registros_id <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au) |>
    dplyr::transmute(id = as.integer(id), usuario_num)

  # Fail-fast defensivo: Si no hay registros esta semana, detenemos elegantemente
  if (nrow(registros_id) == 0) {
    cli::cli_abort(
      "No hay registros de actividad en la ventana solicitada ({fecha_inicio_au} a {fecha_fin_au})."
    )
  }

  ids_validos <- registros_id |> dplyr::pull(id)

  # 3. Extracción Optimizada (Evitamos el cuello de botella del collect prematuro)
  evaluacion <- dplyr::tbl(pool, "EvaluacionRegistro") |>
    dplyr::filter(RegistroId %in% !!ids_validos) |>
    dplyr::collect() |>
    dplyr::inner_join(registros_id, by = dplyr::join_by(RegistroId == id)) |>
    dplyr::mutate(
      json_parseado = purrr::map(Resultado, \(x) {
        lista_cruda <- jsonlite::fromJSON(x)
        lista_limpia <- purrr::map(lista_cruda, \(item) {
          if (length(item) == 0) NA else item
        })
        tibble::as_tibble_row(lista_limpia)
      })
    ) |>
    tidyr::unnest_wider(json_parseado) |>
    dplyr::select(-Resultado) |>
    # --- FAIL-SAFE: Defensive column injection for empty weeks ---
    (\(df) {
      if (!"dictamenFinal" %in% names(df)) {
        dplyr::mutate(df, dictamenFinal = character())
      } else {
        df
      }
    })() |>
    (\(df) {
      if (!"totalEvaluacion" %in% names(df)) {
        dplyr::mutate(df, totalEvaluacion = character())
      } else {
        df
      }
    })() |>
    (\(df) {
      if (!"observaciones" %in% names(df)) {
        dplyr::mutate(df, observaciones = character())
      } else {
        df
      }
    })()
  # 4. Cálculo de Promedios
  bd_prom <- evaluacion |>
    dplyr::mutate(
      totalEvaluacion = dplyr::if_else(
        dictamenFinal == "Eliminada",
        "0",
        as.character(totalEvaluacion)
      )
    ) |>
    dplyr::summarise(
      total = round(
        mean(as.numeric(totalEvaluacion), na.rm = TRUE),
        digits = 1
      ),
      dialogos_auditados = dplyr::n(),
      eliminados = sum(dictamenFinal == "Eliminada", na.rm = TRUE),
      .by = usuario_num
    )

  # 5. Catálogo de Voceros (Regla de negocio: Solo activos)
  voceros_au <- brigadas |>
    dplyr::select(id_brigada, nombre_brigada) |>
    dplyr::left_join(
      voceros |> dplyr::select(nombre_completo, num, id_brigada, status),
      by = "id_brigada"
    ) |>
    dplyr::select(
      nombre_brigada,
      nombre_completo,
      usuario_num = num,
      status
    ) |>
    dplyr::filter(status == TRUE)

  # 6. Base de Efectivos
  efectivos <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_au & fecha < fecha_fin_au) |>
    dplyr::summarise(
      efectivos = sum(desglose == "Efectivo", na.rm = TRUE),
      fecha_ultimo_registro = as.Date(max(fecha, na.rm = TRUE)),
      .by = usuario_num
    )

  # 7. Ensamblado Final
  res_auditoria <- voceros_au |>
    dplyr::inner_join(bd_prom, by = "usuario_num") |>
    dplyr::arrange(dplyr::desc(total), dialogos_auditados) |>
    dplyr::rename(`Promedio de evaluaciones` = total) |>
    dplyr::left_join(efectivos, by = "usuario_num")

  observaciones <- voceros_au |>
    dplyr::inner_join(
      dplyr::select(
        evaluacion,
        RegistroId,
        dplyr::any_of(c("Fecha", "fecha")), # Robusto ante variaciones de mayúsculas
        usuario_num,
        observaciones,
        dictamenFinal
      ),
      by = "usuario_num"
    )

  # 8. Filtro dinámico de brigadas (Parametrizado)
  if (!is.null(excluir_brigadas) && length(excluir_brigadas) > 0) {
    patron <- paste(excluir_brigadas, collapse = "|")
    res_auditoria <- res_auditoria |>
      dplyr::filter(!grepl(patron, nombre_brigada, ignore.case = TRUE))
    observaciones <- observaciones |>
      dplyr::filter(!grepl(patron, nombre_brigada, ignore.case = TRUE))
  }

  list(
    res_auditoria = res_auditoria,
    observaciones = observaciones
  )
}

#' Exportar Reporte de Auditoría a Excel
#'
#' @description
#' Recibe las métricas de auditoría generadas por `generar_metricas_auditoria`
#' y las exporta a un archivo de Excel, aplicando formato condicional
#' (escala de colores) a la columna de promedios.
#'
#' @param datos_auditoria Lista. Output de la función `generar_metricas_auditoria()`.
#' @param ruta_salida Character. Ruta completa donde se guardará el archivo `.xlsx`.
#'
#' @return Devuelve la ruta de salida de forma invisible.
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

  # 3. Write data to sheets
  openxlsx::writeData(wb, "res_auditoria", datos_auditoria$res_auditoria)
  openxlsx::writeData(wb, "observaciones", datos_auditoria$observaciones)

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

  cli::cli_alert_success("Excel workbook successfully created in memory.")

  # 5. Return the object to the user's session
  return(wb)
}
