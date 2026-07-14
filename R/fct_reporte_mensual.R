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

  # Coordinadores genuinos: determinados por Cargo, no por la relación supervisor en bd_aux.
  # Evita que voceros activos que también aparecen como supervisor en alguna brigada
  # (ej. brigada de pruebas) sean promovidos a fila de coordinador con metadatos incorrectos.
  coord_nums <- insumos$cat$usuarios |>
    dplyr::filter(cargo == "Coordinador de Brigada") |>
    dplyr::pull(num)

  procesar_metricas <- function(df_stats, nombre_col_valor) {
    # Unir Voceros (se excluyen filas sin vocero asignado: nunca tienen actividad
    # y generarían filas fantasma de cero tras tidyr::complete).
    # Voceros inactivos (status_vocero = FALSE) se excluyen SALVO que tengan
    # actividad en df_stats: un vocero dado de baja dentro del periodo de reporte
    # sí registró diálogos y debe aparecer en la estructura.
    reg_voc <- bd_aux |>
      dplyr::filter(
        !is.na(vocero),
        is.na(status_vocero) | status_vocero | vocero %in% df_stats$usuario_num
      ) |>
      # Conservar solo las columnas estructurales canónicas (las del nesting()
      # de complete()). Columnas extra de bd_aux (p. ej. nombre_zona_trabajo /
      # nombre_grupo en v0.3) se vuelven NA en las fechas completadas y, al no
      # estar en el nesting, dividen a cada vocero en filas vacías duplicadas
      # dentro del pivot_wider posterior.
      dplyr::select(
        municipio,
        distrito,
        nombre_brigada,
        nombre_coordinador,
        supervisor,
        status_coord,
        nombre_vocero,
        vocero,
        status_vocero
      ) |>
      dplyr::left_join(df_stats, by = dplyr::join_by(vocero == usuario_num))

    # Unir Supervisores: solo personas con Cargo = "Coordinador de Brigada".
    # status_vocero se asigna desde status_coord (status del propio coordinador),
    # no desde el status del vocero del row de bd_aux (que sería arbitrario).
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
        status_vocero = status_coord
      ) |>
      dplyr::arrange(dplyr::desc(nombre_brigada)) |>
      dplyr::distinct(supervisor, .keep_all = TRUE) |>
      dplyr::filter(vocero %in% coord_nums) |>
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

  # Voceros con actividad ausentes del catálogo al corte (coordinador dado de
  # baja, cambio de rol). No se excluyen: se reincorporan más abajo con su
  # estructura resuelta por fecha, de modo que el total de diálogos no cambie.
  voceros_sin_catalogo <- dplyr::setdiff(
    unique(stats_base$usuario_num),
    unique(bd_aux$vocero)
  )

  # Ancla de integridad: conteo directo sobre bd_completa (fuente de verdad),
  # independiente del agregado stats_base para evitar validaciones circulares.
  n_total_esperado <- sum(
    bd_completa$desglose == "Efectivo" &
      bd_completa$fecha >= fecha_inicio_r &
      bd_completa$fecha <= fecha_fin_r,
    na.rm = TRUE
  )

  # Mapa encuesta_id por vocero (1:1 en el caso normal)
  encuesta_id_map <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_r & fecha <= fecha_fin_r) |>
    dplyr::summarise(
      encuesta_id = toString(unique(encuesta_id)),
      .by = usuario_num
    )

  voceros_multi_encuesta <- bd_completa |>
    dplyr::filter(fecha >= fecha_inicio_r & fecha <= fecha_fin_r) |>
    dplyr::summarise(n = dplyr::n_distinct(encuesta_id), .by = usuario_num) |>
    dplyr::filter(n > 1)
  if (nrow(voceros_multi_encuesta) > 0) {
    cli::cli_alert_warning(
      "{nrow(voceros_multi_encuesta)} vocero(s) trabajaron en más de un cuestionario: {paste(voceros_multi_encuesta$usuario_num, collapse = ', ')}"
    )
  }

  # -------------------------------------------------------------------------
  # Reincorporación de personal fuera del catálogo al corte (resuelto por fecha)
  # -------------------------------------------------------------------------
  # Regla dura: el total de diálogos de la ventana no cambia. En vez de excluir
  # a quien registró actividad pero ya no figura en la estructura al corte, o de
  # omitir a coordinadores con cuestionario asignado, se reconstruyen sus filas
  # y se anexan a bd_aux para que el pipeline las procese de forma uniforme.
  # Brigada del vocero: UsuarioLog (bd_completa$id_brigada). Coordinador de esa
  # brigada: BrigadasLog vía resolver_coordinador_en_fecha().
  brigada_log      <- insumos$cat$brigada_log
  num_map          <- insumos$cat$num_map
  hay_brigada_hist <- !is.null(brigada_log) && !is.null(num_map) &&
    "id_brigada" %in% names(bd_completa)

  # Lookup brigada -> coordinador/metadata vigente al corte (BrigadasLog LOCF).
  estructura_brigada <- tibble::tibble(
    id_brigada = integer(), nombre_brigada = character(),
    supervisor = character(), nombre_coordinador = character(),
    status_coord = logical()
  )
  if (hay_brigada_hist) {
    estructura_brigada <- resolver_coordinador_en_fecha(brigada_log, corte) |>
      dplyr::left_join(num_map, by = c("id_coordinador_log" = "id_usuario")) |>
      dplyr::left_join(
        insumos$cat$usuarios |>
          dplyr::select(dplyr::any_of(c("id_usuario", "nombre_completo", "status"))),
        by = c("id_coordinador_log" = "id_usuario")
      ) |>
      dplyr::transmute(
        id_brigada,
        nombre_brigada     = nombre_brigada_log,
        supervisor         = as.character(num),
        nombre_coordinador = nombre_completo,
        status_coord       = status
      )
  }
  # municipio/distrito de la brigada, prestados de bd_aux cuando ya aparece por
  # otros integrantes (evita otra consulta).
  meta_brig <- bd_aux |>
    dplyr::slice_head(n = 1, by = nombre_brigada) |>
    dplyr::select(nombre_brigada, municipio, distrito)

  filas_extra     <- list()
  coord_cero_nums <- character(0)

  # (1) Huérfanos con actividad: reincorporados con su última brigada de la ventana.
  if (length(voceros_sin_catalogo) > 0) {
    ult_brigada <- if (hay_brigada_hist) {
      bd_completa |>
        dplyr::filter(
          usuario_num %in% voceros_sin_catalogo,
          fecha >= fecha_inicio_r, fecha <= fecha_fin_r,
          !is.na(id_brigada)
        ) |>
        dplyr::group_by(usuario_num) |>
        dplyr::slice_max(fecha, n = 1, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::transmute(vocero = usuario_num, id_brigada)
    } else {
      tibble::tibble(vocero = character(), id_brigada = integer())
    }

    ident <- dplyr::tbl(pool, "Usuarios") |>
      dplyr::filter(IdProyecto == !!id_proyecto, Num %in% !!as.integer(voceros_sin_catalogo)) |>
      dplyr::select(Num, Nombre, APaterno, AMaterno, Status) |>
      dplyr::collect() |>
      dplyr::transmute(
        vocero        = as.character(Num),
        nombre_vocero = toupper(stringr::str_squish(paste(Nombre, APaterno, AMaterno))),
        status_vocero = as.logical(Status)
      )

    filas_extra$orfanos <- tibble::tibble(vocero = voceros_sin_catalogo) |>
      dplyr::left_join(ult_brigada, by = "vocero") |>
      dplyr::left_join(estructura_brigada, by = "id_brigada") |>
      dplyr::left_join(meta_brig, by = "nombre_brigada") |>
      dplyr::left_join(ident, by = "vocero") |>
      dplyr::transmute(
        municipio, distrito,
        nombre_brigada     = tidyr::replace_na(nombre_brigada, "SIN ASIGNAR"),
        nombre_coordinador = tidyr::replace_na(nombre_coordinador, "SIN ASIGNAR"),
        supervisor         = tidyr::replace_na(supervisor, "SIN ASIGNAR"),
        status_coord,
        nombre_vocero, vocero, status_vocero
      )
    cli::cli_alert_info(
      "Reincorporados {length(voceros_sin_catalogo)} usuario(s) con actividad fuera del catálogo al corte (resueltos por fecha), para no alterar el total."
    )
  }

  # (2) Coordinadores con cuestionario asignado (UsuariosEncuesta) sin diálogos:
  # aparecen en ceros. Requiere el histórico de brigada para ubicar su brigada.
  usuarios_encuesta <- insumos$cat$usuarios_encuesta
  if (!is.null(usuarios_encuesta) && hay_brigada_hist) {
    encuestas_reporte <- unique(bd_completa$encuesta_id[
      bd_completa$fecha >= fecha_inicio_r & bd_completa$fecha <= fecha_fin_r
    ])
    asignados_ids <- usuarios_encuesta |>
      dplyr::filter(EncuestaId %in% encuestas_reporte, Activo == TRUE) |>
      dplyr::pull(UsuarioId)

    ya_presentes <- unique(c(bd_aux$vocero, bd_aux$supervisor, voceros_sin_catalogo))
    coord_cero <- resolver_coordinador_en_fecha(brigada_log, corte) |>
      dplyr::filter(id_coordinador_log %in% asignados_ids) |>
      dplyr::distinct(id_brigada) |>
      dplyr::left_join(estructura_brigada, by = "id_brigada") |>
      dplyr::filter(!is.na(supervisor), !supervisor %in% ya_presentes) |>
      dplyr::left_join(meta_brig, by = "nombre_brigada") |>
      dplyr::distinct(supervisor, .keep_all = TRUE)

    if (nrow(coord_cero) > 0) {
      filas_extra$coord_cero <- coord_cero |>
        dplyr::transmute(
          municipio, distrito, nombre_brigada,
          nombre_coordinador, supervisor, status_coord,
          nombre_vocero = nombre_coordinador,
          vocero        = supervisor,
          status_vocero = status_coord
        )
      coord_cero_nums <- coord_cero$supervisor
      cli::cli_alert_info(
        "Agregados {length(coord_cero_nums)} coordinador(es) con cuestionario asignado y sin diálogos, en ceros."
      )
    }
  }

  if (length(filas_extra) > 0) {
    bd_aux     <- dplyr::bind_rows(bd_aux, dplyr::bind_rows(filas_extra))
    coord_nums <- union(coord_nums, coord_cero_nums)
  }

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
    dplyr::select(-dplyr::any_of("NA")) |>
    dplyr::left_join(encuesta_id_map, by = dplyr::join_by(vocero == usuario_num)) |>
    dplyr::filter(status_coord | Total > 0 | vocero %in% coord_cero_nums)

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
    dplyr::left_join(encuesta_id_map, by = dplyr::join_by(vocero == usuario_num)) |>
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
    dplyr::ungroup() |>
    dplyr::filter(status_coord | Total > 0 | vocero %in% coord_cero_nums)

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
