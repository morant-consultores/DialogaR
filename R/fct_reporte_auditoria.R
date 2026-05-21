# ============================================================
# Módulo    : fct_reporte_auditoria
# Propósito : Generación del reporte de métricas de auditoría y producción
# Datos     : INTERNO
# Control   : A.8.2 (Clasificación), A.9.4 (Control de acceso)
# Revisión  : 2026-05-21
# ============================================================
# NOTAS DE SEGURIDAD:
# - No hardcodear credenciales. Usar .Renviron
# - Los datos resultantes se consideran activos de información tipo A
# -------------------------------------------------------------------------

#' Encontrar fecha exacta hacia el pasado basada en un día de la semana
#'
#' @description
#' Calcula de forma retrospectiva la fecha de un día de la semana específico
#' (ej. "lunes", "domingo") más cercano en el pasado con respecto a una fecha de referencia.
#' Si la fecha de referencia coincide con el día objetivo, retrocede una semana completa (7 días).
#'
#' @param referencia Objeto de clase \code{Date}. La fecha base a partir de la cual se buscará hacia atrás.
#' @param nombre_dia Caracter. El nombre del día de la semana que se desea encontrar (ej. "lunes", "domingo", "miércoles"). No distingue mayúsculas ni acentos.
#'
#' @return Un objeto de clase \code{Date} con la fecha exacta calculada.
#'
#' @seealso \code{\link[lubridate]{wday}}
#'
#' @examples
#' \dontrun{
#' encontrar_fecha_exacta(as.Date("2026-05-15"), "lunes")
#' }
#'
#' @importFrom lubridate wday
#' @keywords internal
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

#' Ensamblar estructura operativa con métricas de producción
#'
#' @description
#' Cruza la estructura limpia del personal (voceros, coordinadores, supervisores) con
#' un data frame de métricas recolectadas, consolidando tanto la actividad de los
#' voceros activos como la jerarquía de coordinadores y supervisores en una estructura unificada.
#'
#' @param stats_df Data frame o tibble. Métricas calculadas por usuario (debe incluir `usuario_num`).
#' @param bd_aux_clean Data frame o tibble. Estructura operativa depurada con columnas `vocero`, `status_vocero`, `nombre_coordinador`, `supervisor`, entre otras.
#' @param coord_nums Vector. Identificadores (`num`) de quienes tienen cargo "Coordinador de Brigada".
#'
#' @return Tibble consolidado y sin duplicados con la estructura jerárquica y métricas asociadas.
#'
#' @importFrom dplyr filter left_join transmute distinct bind_rows join_by
#' @keywords internal
ensamblar_estructura <- function(stats_df, bd_aux_clean, coord_nums) {
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

#' Generar Reporte de Métricas de Auditoría y Producción
#'
#' @description
#' Procesa y consolida la información de producción semanal e histórica de voceros y
#' coordinadores, cruza los datos con las evaluaciones de auditoría almacenadas en la
#' base de datos y genera estructuras listas para reporte. Opcionalmente actualiza y
#' filtra un archivo histórico local en formato RDS.
#'
#' Este reporte trabaja con dos periodos de tiempo: la columna `efectivos` refleja
#' la semana inmediata previa, mientras que los valores de auditoría corresponden a
#' la semana natural corriente (lunes a domingo).
#'
#' @param pool Objeto de conexión `pool` a la base de datos relacional.
#' @param insumos Lista con catálogos base; debe incluir `cat$usuarios` con el rol de los integrantes.
#' @param bd_completa Data frame o tibble con el histórico de producción y desgloses de efectividad.
#' @param bd_aux Data frame o tibble con la estructura operativa (voceros, coordinadores, supervisores y brigadas).
#' @param encuesta_id Vector. Identificador(es) de encuesta para filtrar la tabla `Registros` en la base de datos.
#' @param corte Fecha o string (`YYYY-MM-DD`) que define el límite superior para la auditoría semanal.
#' @param inicio_semanal Caracter. Día de inicio del periodo efectivo (ej. `"lunes"`). Por defecto `"lunes"`.
#' @param fin_semanal Caracter. Día de cierre del periodo efectivo (ej. `"domingo"`). Por defecto `"domingo"`.
#' @param simular_domingo Lógico. Si `TRUE`, ajusta el corte al domingo de esa misma semana. Por defecto `FALSE`.
#' @param excluir_brigadas Vector de caracteres. Nombres o patrones de brigadas a excluir del reporte. Por defecto `NULL`.
#' @param filtrar_historicos Lógico. Si `TRUE`, los promedios históricos se calculan solo con los IDs del RDS acumulado, que también se actualiza con los registros de esta semana. Por defecto `FALSE`.
#' @param path_historicos Ruta al archivo `.rds` que acumula los IDs históricos. Requerido cuando `filtrar_historicos = TRUE`. Por defecto `NULL`.
#'
#' @return Una `lista` con tres elementos:
#' \describe{
#'   \item{res_auditoria}{Métricas resumidas de la semana actual por vocero/brigada.}
#'   \item{observaciones}{Detalle de comentarios y dictámenes individuales de la semana.}
#'   \item{res_auditoria_hist}{Métricas resumidas históricas por vocero/brigada.}
#' }
#'
#' @section Seguridad y Privacidad:
#' Nivel de datos: INTERNO (estructura operativa de brigadas y voceros).
#' No registrar columnas de usuarios en logs ni mensajes de error.
#' Control ISO 27001: A.8.2 (Clasificación de información).
#'
#' @export
#' @importFrom dplyr mutate filter summarise arrange distinct left_join inner_join bind_rows transmute select rename relocate n if_else across
#' @importFrom lubridate ceiling_date floor_date wday with_tz
#' @importFrom tidyr replace_na complete nesting unnest_wider
#' @importFrom cli cli_alert_info cli_inform cli_alert_success cli_abort
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map
#' @importFrom readr read_rds write_rds
#'
#' @examples
#' \dontrun{
#' reportes <- generar_reporte_metricas(
#'   pool = mi_conexion,
#'   insumos = lista_insumos,
#'   bd_completa = df_produccion,
#'   bd_aux = df_estructura,
#'   encuesta_id = 1L,
#'   corte = "2026-05-15",
#'   filtrar_historicos = FALSE
#' )
#' }
generar_reporte_metricas <- function(pool,
                                     insumos,
                                     bd_completa,
                                     bd_aux,
                                     encuesta_id,
                                     corte,
                                     inicio_semanal = "lunes",
                                     fin_semanal = "domingo",
                                     simular_domingo = FALSE,
                                     excluir_brigadas = NULL,
                                     filtrar_historicos = FALSE,
                                     path_historicos = NULL) {

  # --- 1. LÓGICA DE FECHAS ---
  corte_dt <- as.Date(corte)
  if (simular_domingo) {
    corte_dt <- lubridate::ceiling_date(corte_dt, unit = "week", week_start = 1) - 1
    cli::cli_alert_info("Modo prueba: corte ajustado a {corte_dt} (domingo)")
  }

  fecha_inicio_au <- lubridate::floor_date(corte_dt, unit = "week", week_start = 1)
  fecha_fin_au    <- corte_dt
  fecha_fin_ef    <- encontrar_fecha_exacta(fecha_inicio_au, fin_semanal)
  fecha_inicio_ef <- encontrar_fecha_exacta(fecha_fin_ef, inicio_semanal)
  rango_fechas    <- seq.Date(fecha_inicio_ef, fecha_fin_ef, by = "day")

  if (fecha_fin_ef > fecha_fin_au) {
    cli::cli_abort(c(
      "Inconsistencia en fechas calculadas.",
      "i" = "fecha_fin_ef ({fecha_fin_ef}) no puede ser posterior a fecha_fin_au ({fecha_fin_au})."
    ))
  }
  if (fecha_inicio_ef > fecha_fin_ef) {
    cli::cli_abort(c(
      "Inconsistencia en fechas calculadas.",
      "i" = "fecha_inicio_ef ({fecha_inicio_ef}) no puede ser posterior a fecha_fin_ef ({fecha_fin_ef})."
    ))
  }

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
  stats_sem <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_ef & fecha <= fecha_fin_ef) |>
    dplyr::summarise(n = sum(desglose == "Efectivo", na.rm = TRUE), .by = c(usuario_num, fecha))

  stats_hist_base <- bd_completa |>
    dplyr::summarise(
      efectivos_totales = sum(desglose == "Efectivo", na.rm = TRUE),
      fecha_ultimo_reg_hist = as.Date(max(fecha, na.rm = TRUE)),
      .by = usuario_num
    )

  # --- 4. CREACIÓN DE HOJAS DE REGISTROS ---
  hoja_registros_sem <- ensamblar_estructura(stats_sem, bd_aux_clean, coord_nums) |>
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

  hoja_registros_hist_base <- ensamblar_estructura(stats_hist_base, bd_aux_clean, coord_nums) |>
    dplyr::filter(!(nombre_brigada == "SIN ASIGNAR" & !vocero %in% stats_hist_base$usuario_num)) |>
    dplyr::transmute(nombre_brigada, nombre_vocero, vocero, status_vocero,
                     efectivos = efectivos_totales,
                     fecha_ultimo_registro = fecha_ultimo_reg_hist)

  # --- 5. PROCESAMIENTO DE AUDITORÍAS ---
  # registros_id_hist se extrae directamente de la tabla Registros filtrada por encuesta_id,
  # lo que garantiza que el join con EvaluacionRegistro ocurra en la base de datos (pushdown)
  # y evita traer registros de otras encuestas.
  registros_id_hist <- dplyr::tbl(pool, "Registros") |>
    dplyr::filter(EncuestaId %in% encuesta_id) |>
    dplyr::transmute(id = Id, usuario_num = UsuarioNum)

  evaluacion_raw <- dplyr::tbl(pool, "EvaluacionRegistro") |>
    dplyr::inner_join(registros_id_hist, by = dplyr::join_by(RegistroId == id)) |>
    dplyr::collect() |>
    dplyr::mutate(
      json_parseado = purrr::map(Resultado, \(x) {
        lista_cruda <- jsonlite::fromJSON(x)
        lista_limpia <- purrr::map(lista_cruda, \(item) {
          if (length(item) == 0) return(NA)
          if (length(item) > 1) return(list(item))
          return(item)
        })
        tibble::as_tibble_row(lista_limpia)
      }),
      fecha_hora_cdmx = lubridate::with_tz(Fecha, tzone = "America/Mexico_City"),
      fecha = as.Date(fecha_hora_cdmx)
    ) |>
    tidyr::unnest_wider(json_parseado)

  evaluacion_sem <- evaluacion_raw |> dplyr::filter(fecha >= fecha_inicio_au & fecha <= fecha_fin_au)

  # --- 5.1 LÓGICA DE ACTUALIZACIÓN DEL ARCHIVO DE HISTÓRICOS (.RDS) ---
  if (filtrar_historicos && is.null(path_historicos)) {
    cli::cli_abort("`path_historicos` es requerido cuando `filtrar_historicos = TRUE`.")
  }

  if (filtrar_historicos) {
    reg_semana <- evaluacion_sem |> dplyr::pull(RegistroId)

    if (file.exists(path_historicos)) {
      reg_hist_previos <- unlist(readr::read_rds(path_historicos))
    } else {
      reg_hist_previos <- integer()
    }

    reg_hist_actualizado <- unique(c(reg_hist_previos, reg_semana))
    readr::write_rds(reg_hist_actualizado, path_historicos)
    cli::cli_alert_success("Base de históricos en RDS actualizada correctamente.")
  }

  # --- 6. CÁLCULO DE MÉTRICAS ---
  calc_promedios <- function(df) {
    df |>
      dplyr::mutate(totalEvaluacion = dplyr::if_else(
        dictamenFinal == "Eliminada", "0", as.character(totalEvaluacion)
      )) |>
      dplyr::summarise(
        total              = round(mean(as.numeric(totalEvaluacion), na.rm = TRUE), 1),
        dialogos_auditados = dplyr::n(),
        optimos            = sum(dictamenFinal == "Diálogo Óptimo",    na.rm = TRUE),
        aceptables         = sum(dictamenFinal == "Diálogo Aceptable", na.rm = TRUE),
        deficientes        = sum(dictamenFinal == "Diálogo Deficiente", na.rm = TRUE),
        eliminados         = sum(dictamenFinal == "Eliminada",         na.rm = TRUE),
        .by = usuario_num
      )
  }

  bd_prom_sem <- calc_promedios(evaluacion_sem)

  if (filtrar_historicos) {
    bd_prom_hist <- calc_promedios(
      evaluacion_raw |> dplyr::filter(RegistroId %in% reg_hist_actualizado)
    )
  } else {
    bd_prom_hist <- calc_promedios(evaluacion_raw)
  }

  # --- 7. ENSAMBLE DE HOJAS FINALES ---
  ensamblar_hoja <- function(hoja_base, bd_prom) {
    hoja_base |>
      dplyr::left_join(bd_prom, by = c("vocero" = "usuario_num")) |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(.x, 0))) |>
      dplyr::mutate(total = dplyr::if_else(efectivos == 0, NaN, total)) |>
      dplyr::rename(`Promedio de evaluaciones` = total) |>
      dplyr::relocate(optimos, aceptables, deficientes, .after = dialogos_auditados) |>
      dplyr::relocate(efectivos, .after = eliminados) |>
      dplyr::arrange(dplyr::desc(`Promedio de evaluaciones`), dplyr::desc(dialogos_auditados))
  }

  res_auditoria      <- ensamblar_hoja(hoja_registros_sem,       bd_prom_sem)
  res_auditoria_hist <- ensamblar_hoja(hoja_registros_hist_base,  bd_prom_hist)

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
    res_auditoria      = res_auditoria,
    observaciones      = observaciones,
    res_auditoria_hist = res_auditoria_hist
  ))
}

#' Crear Libro de Excel para Reporte de Auditoría
#'
#' @description
#' Toma la lista generada por \code{\link{generar_reporte_metricas}} y construye un objeto
#' Workbook de Excel con formato condicional de escala de colores (Rojo a Verde) en la
#' columna "Promedio de evaluaciones" de las hojas semanal e histórica.
#'
#' **Nota:** Esta función no guarda el archivo en disco. Retorna el objeto en memoria
#' para que pueda pasarse a funciones de subida o guardado.
#'
#' @param datos_auditoria Lista. Salida de `generar_reporte_metricas()`. Debe contener
#'   `res_auditoria`, `observaciones` y `res_auditoria_hist`.
#'
#' @return Un objeto `Workbook` de \pkg{openxlsx} listo para guardarse con `saveWorkbook`.
#' @export
#'
#' @importFrom openxlsx createWorkbook addWorksheet writeData conditionalFormatting
#' @importFrom cli cli_abort cli_alert_success
#'
#' @examples
#' \dontrun{
#' wb <- crear_workbook_auditoria(reportes)
#' openxlsx::saveWorkbook(wb, "Reporte_Auditoria.xlsx", overwrite = TRUE)
#' }
crear_workbook_auditoria <- function(datos_auditoria) {
  if (!all(c("res_auditoria", "observaciones", "res_auditoria_hist") %in% names(datos_auditoria))) {
    cli::cli_abort(
      "El objeto 'datos_auditoria' debe contener 'res_auditoria', 'observaciones' y 'res_auditoria_hist'."
    )
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "res_auditoria")
  openxlsx::addWorksheet(wb, "observaciones")
  openxlsx::addWorksheet(wb, "res_auditoria_hist")

  openxlsx::writeData(wb, "res_auditoria",      datos_auditoria$res_auditoria)
  openxlsx::writeData(wb, "observaciones",       datos_auditoria$observaciones)
  openxlsx::writeData(wb, "res_auditoria_hist",  datos_auditoria$res_auditoria_hist)

  aplicar_escala_color <- function(sheet, nrows) {
    col_idx <- which(names(datos_auditoria[[sheet]]) == "Promedio de evaluaciones")
    if (length(col_idx) > 0) {
      openxlsx::conditionalFormatting(
        wb, sheet = sheet, cols = col_idx,
        rows = 2:(nrows + 1),
        style = c("#FF0000", "#00FF00"),
        type = "colourScale"
      )
    }
  }

  aplicar_escala_color("res_auditoria",      nrow(datos_auditoria$res_auditoria))
  aplicar_escala_color("res_auditoria_hist", nrow(datos_auditoria$res_auditoria_hist))

  cli::cli_alert_success("Libro de Excel creado exitosamente en memoria.")
  return(wb)
}
