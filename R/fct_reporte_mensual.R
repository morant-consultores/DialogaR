# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_reporte_mensual.R
# OBJETIVO: Lógica core para la generación del consolidado de productividad
#            mensual y semanal de las brigadas
# AUTOR:    Rafael López / Equipo de Análisis
# FECHA:    2026-03-23
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno
# -------------------------------------------------------------------------
# NOTAS DE SEGURIDAD:
# - Las funciones documentadas aquí asumen que los datos provienen de
#   entradas sanitizadas (ETL previo).
# - Cualquier cambio a las reglas de negocio (ej. umbral_cortos) debe
#   ser avalado por el líder del proyecto.
# -------------------------------------------------------------------------

#' Generar reporte de productividad por brigadas (Semanal/Mensual)
#'
#' @description
#' Genera un consolidado de productividad operativa en una ventana temporal
#' (semanal o mensual). Calcula el total de diálogos efectivos, la duración
#' promedio, el tiempo de trabajo en campo y la incidencia de diálogos cortos.
#'
#' **Lógica de Negocio (Business Rules):**
#' * **Efectividad:** Solo se contabilizan como éxito los registros con desglose
#'   exactamente igual a "Efectivo" (Estándar de calidad interno).
#' * **Promedio Diario:** Se evalúa sobre el esfuerzo real. Se calcula dividiendo
#'   el total de diálogos únicamente entre los días que el vocero registró actividad
#'   (días hábiles trabajados).
#' * **Diálogos Cortos:** Interacciones cuya duración sea menor o igual al parámetro `umbral_cortos`.
#' * **Huérfanos Operativos:** Los voceros sin coordinador asignado en el catálogo administrativo
#'   no se eliminan; se agrupan bajo la categoría "SIN ASIGNAR" y disparan una alerta de auditoría.
#'
#' @param reporte Character. Tipo de ventana temporal: `"semanal"` o `"mensual"`.
#' @param corte Date. Fecha de corte del reporte.
#' @param id_proyecto Numeric/Integer. ID del proyecto en la base de datos (para cruzar altas/bajas).
#' @param pool Objeto de conexión a la base de datos (pool DBI).
#' @param bd_completa Tibble. Hechos de actividad operativa (registros limpios).
#' @param bd_aux Tibble. Estructura administrativa operativa al corte.
#' @param insumos List. Lista de insumos generados por cargar_insumos().
#' @param week_start Numeric. Día de inicio de la semana (1 = Lunes, 6 = Sábado, 7 = Domingo). Por defecto `6`.
#' @param umbral_cortos Numeric. Minutos máximos para considerar un diálogo como "corto". Por defecto `2`.
#'
#' @return Lista con tres elementos:
#' \item{registros}{Tibble con el consolidado final de métricas por vocero/coordinador.}
#' \item{cortos}{Tibble con la matriz de incidencia de diálogos cortos por fecha.}
#' \item{rango_fechas}{Vector Date que comprende la ventana temporal analizada.}
#'
#' @export
generar_reporte_brigadas <- function(
  reporte = c("semanal", "mensual"),
  corte,
  id_proyecto,
  pool,
  bd_completa,
  bd_aux,
  insumos,
  week_start = 6,
  umbral_cortos = 2
) {
  reporte <- match.arg(reporte)

  # 1. Configuración de fechas y rutas ----
  if (reporte == "semanal") {
    fecha_inicio_r <- lubridate::floor_date(
      corte,
      unit = "week",
      week_start = week_start
    )
    fecha_fin_r <- corte
  } else {
    fecha_inicio_r <- lubridate::floor_date(corte - 1, "month")
    fecha_fin_r <- lubridate::ceiling_date(corte - 1, "month")
  }

  rango_fechas <- seq.Date(fecha_inicio_r, fecha_fin_r, by = "day")

  # 2. Gestión de Altas y Bajas ----
  bajas <- dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(IdProyecto == id_proyecto) |>
    dplyr::transmute(
      IdUsuario = Id,
      fecha_baja = dplyr::if_else(
        Status == FALSE,
        as.Date(FechaUpdate),
        lubridate::today()
      ),
      usuario = Num
    ) |>
    dplyr::collect()

  altas_bajas <- insumos$cat$usuario_log |>
    dplyr::arrange(FechaInsert) |>
    dplyr::distinct(IdUsuario, .keep_all = TRUE) |>
    dplyr::transmute(IdUsuario, fecha_alta = as.Date(FechaInsert)) |>
    dplyr::left_join(bajas, by = "IdUsuario") |>
    dplyr::distinct(usuario, .keep_all = TRUE) |>
    dplyr::select(-IdUsuario)

  # --- Manejo de Huérfanos Operativos ---
  # Auditoría: Imputar 'SIN ASIGNAR' para evitar pérdida de actividad en los joins
  bd_aux <- bd_aux |>
    dplyr::mutate(
      nombre_coordinador = tidyr::replace_na(nombre_coordinador, "SIN ASIGNAR"),
      supervisor = tidyr::replace_na(supervisor, "SIN ASIGNAR"),
      nombre_brigada = tidyr::replace_na(nombre_brigada, "SIN ASIGNAR")
    )

  # --- Deduplicación por cambios de coordinador ---
  # Un vocero puede aparecer en múltiples filas por cambios históricos de coordinador.
  # Se conserva únicamente el coordinador activo (status_coord = TRUE); si no hay ninguno
  # activo, se toma la primera fila disponible.
  bd_aux <- bd_aux |>
    dplyr::arrange(dplyr::desc(status_coord)) |>
    dplyr::distinct(vocero, .keep_all = TRUE)

  # 3. Función interna para procesar métricas ----
  procesar_metricas <- function(df_stats, nombre_col_valor) {
    # Unir Voceros
    reg_voc <- bd_aux |>
      dplyr::left_join(df_stats, by = dplyr::join_by(vocero == usuario_num))

    # Unir Supervisores
    reg_sup <- bd_aux |>
      dplyr::transmute(
        municipio,
        distrito,
        nombre_brigada,
        nombre_coordinador,
        supervisor,
        status_coord,
        nombre_vocero = nombre_coordinador,
        vocero = supervisor,
        status_vocero
      ) |>
      dplyr::arrange(dplyr::desc(nombre_brigada)) |>
      dplyr::distinct(distrito, nombre_coordinador, .keep_all = TRUE) |>
      dplyr::left_join(df_stats, by = dplyr::join_by(supervisor == usuario_num))

    # Excluir de reg_voc a los coordinadores que ya tienen fila propia en reg_sup
    # (evita que aparezcan dos veces con distinto nombre_coordinador)
    supervisores <- unique(reg_sup$vocero[!is.na(reg_sup$vocero)])
    reg_voc <- reg_voc |>
      dplyr::filter(!vocero %in% supervisores)

    # Combinar y completar fechas
    dplyr::bind_rows(reg_sup, reg_voc) |>
      dplyr::distinct() |>
      tidyr::complete(
        tidyr::nesting(
          municipio,
          distrito,
          nombre_brigada,
          nombre_coordinador,
          supervisor,
          status_coord,
          nombre_vocero,
          vocero,
          status_vocero
        ),
        fecha = rango_fechas,
        fill = rlang::set_names(list(0), nombre_col_valor)
      )
  }

  # 4. Cálculo de Registros (n, duración, trabajo diario) ----
  stats_base <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_r & fecha <= fecha_fin_r) |>
    dplyr::summarise(
      n = sum(desglose == "Efectivo", na.rm = TRUE),
      duracion_promedio = mean(
        as.numeric(dplyr::if_else(
          desglose == "Efectivo",
          duracion_minutos,
          NA_real_
        )),
        na.rm = TRUE
      ),
      trabajo_diario = as.numeric(difftime(
        max(fecha_fin, na.rm = TRUE),
        min(fecha_inicio, na.rm = TRUE),
        units = "hours"
      )),
      .by = c(usuario_num, fecha)
    )

  # Integridad: voceros con actividad que no están en el catálogo → sus registros
  # se perderían en el left join de procesar_metricas (bd_aux es la tabla izquierda).
  voceros_sin_catalogo <- dplyr::setdiff(
    unique(stats_base$usuario_num),
    unique(bd_aux$vocero)
  )
  if (length(voceros_sin_catalogo) > 0) {
    n_perdidos <- stats_base |>
      dplyr::filter(usuario_num %in% voceros_sin_catalogo) |>
      dplyr::summarise(total = sum(n, na.rm = TRUE)) |>
      dplyr::pull(total)
    cli::cli_alert_danger(
      "Integridad comprometida: {length(voceros_sin_catalogo)} vocero(s) con {n_perdidos} registro(s) efectivos no figuran en el cat\u00e1logo administrativo y ser\u00e1n excluidos del reporte: {paste(voceros_sin_catalogo, collapse = ', ')}"
    )
  }

  n_total_esperado <- sum(stats_base$n, na.rm = TRUE)

  reg_final <- procesar_metricas(stats_base, "n")

  # 5. Formateo de Tabla Principal (Pivot Wider) ----
  registros_sum <- reg_final |>
    dplyr::select(-c(duracion_promedio, trabajo_diario)) |>
    tidyr::pivot_wider(names_from = fecha, values_from = n, values_fill = 0) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Total = sum(
        dplyr::c_across(dplyr::any_of(as.character(rango_fechas))),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of("NA"))

  # 6. Cálculo de Cortos ----
  stats_cortos <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_r & fecha <= fecha_fin_r) |> # Se homologó a <= para empatar con stats_base
    dplyr::summarise(
      cortos = sum(duracion_minutos <= umbral_cortos, na.rm = TRUE),
      .by = c(usuario_num, fecha)
    )

  final_cortos <- procesar_metricas(stats_cortos, "cortos") |>
    tidyr::pivot_wider(
      names_from = fecha,
      values_from = cortos,
      values_fill = 0
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Total = sum(
        dplyr::c_across(dplyr::any_of(as.character(rango_fechas))),
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of("NA"))

  # 7. Consolidación Final ----
  duracion_trabajo <- reg_final |>
    dplyr::summarise(
      dplyr::across(
        c(duracion_promedio, trabajo_diario),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by = c(
        municipio,
        nombre_brigada,
        nombre_coordinador,
        supervisor,
        status_coord,
        nombre_vocero,
        vocero,
        status_vocero
      )
    )

  resultado_final <- registros_sum |>
    dplyr::left_join(altas_bajas, by = dplyr::join_by(vocero == usuario)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dias_habiles_trabajados = sum(
        dplyr::c_across(dplyr::any_of(as.character(rango_fechas))) != 0,
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(
      duracion_trabajo,
      by = c(
        "municipio",
        "nombre_brigada",
        "nombre_coordinador",
        "supervisor",
        "status_coord",
        "nombre_vocero",
        "vocero",
        "status_vocero"
      )
    ) |>
    dplyr::mutate(
      promedio_diario = dplyr::if_else(
        dias_habiles_trabajados > 0,
        Total / dias_habiles_trabajados,
        0
      ),
      fecha_baja = dplyr::if_else(
        fecha_baja == lubridate::today(),
        as.Date(NA),
        fecha_baja
      )
    ) |>
    dplyr::ungroup()

  # Alerta de Auditoría
  huerfanos <- sum(
    resultado_final$nombre_coordinador == "SIN ASIGNAR",
    na.rm = TRUE
  )
  if (huerfanos > 0) {
    cli::cli_alert_warning(
      "Alerta de Calidad de Datos: Se detectaron {huerfanos} registros asociados a brigadistas sin coordinador (SIN ASIGNAR)."
    )
  }

  # Verificación de integridad: el Total consolidado debe igualar la suma de stats_base
  n_total_real <- sum(
    resultado_final[, as.character(rango_fechas), drop = FALSE],
    na.rm = TRUE
  )
  if (n_total_real != n_total_esperado) {
    cli::cli_alert_danger(
      "Integridad comprometida: se esperaban {n_total_esperado} registros efectivos pero el reporte contiene {n_total_real}. Diferencia: {n_total_real - n_total_esperado}."
    )
  } else {
    cli::cli_alert_success("Integridad OK: {n_total_real} registros efectivos consolidados.")
  }

  list(
    registros = resultado_final,
    cortos = final_cortos,
    rango_fechas = rango_fechas
  )
}
