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
    dplyr::filter(is.na(status_coord) | status_coord | Total > 0)

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
    dplyr::filter(is.na(status_coord) | status_coord | Total > 0)

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

#' Corregir filas de personal que cambió de rol durante la semana
#'
#' @description
#' Post-procesa el `registros` de [generar_reporte_brigadas()] para dar de alta
#' las filas de nómina de brigadistas cuyo rol cambió dentro de la ventana del
#' reporte. `generar_reporte_brigadas()` arma la estructura a partir del
#' catálogo **al corte**, por lo que pierde a quien transicionó de rol durante
#' la semana. Esta función reconstruye esas filas usando el histórico (actividad
#' fechada, pase de lista y logs), sin alterar la contabilidad de diálogos.
#'
#' **Casos que corrige:**
#' * **Vocero promovido a coordinador (Tipo A):** sus diálogos de la semana
#'   (hechos como vocero) quedan pegados a su fila de coordinador. Se mueven a
#'   una fila de vocero bajo la brigada donde efectivamente los registró
#'   (`bd_completa$id_brigada` conserva la brigada histórica por fecha) y se
#'   ponen en cero los diálogos de su fila de coordinador. El total de diálogos
#'   del reporte no cambia.
#' * **Coordinador dado de baja con asistencia en la semana (Tipo B):** al
#'   quedar sin cargo/brigada al corte desaparece del reporte. Se detecta por
#'   pase de lista (`obtener_usuario`, quien pasa lista) y se inyecta su fila de
#'   coordinador (diálogos en cero, `status_coord = FALSE`) con `fecha_baja` en
#'   su **último día de pase de lista** — el dato duro de su último día laborado.
#'
#' @param registros Tibble `registros` devuelto por [generar_reporte_brigadas()].
#' @param bd_completa Tibble de actividad con `id_brigada` resuelto por fecha
#'   (salida de [resolver_brigada_en_fecha()]).
#' @param insumos Lista de insumos (misma que se pasó a `generar_reporte_brigadas`).
#' @param rango_fechas Vector Date de la ventana (elemento `rango_fechas` de la salida).
#' @param pool Conexión DBI/pool (para resolver coordinadores fuera del catálogo).
#' @param id_proyecto Numeric/Integer. ID del proyecto.
#' @param pase_lista Tibble de pase de lista con columnas `fecha` y
#'   `obtener_usuario` (p. ej. `dplyr::bind_rows(insumos$pase_lista)`). Si es
#'   `NULL` se omite el Tipo B.
#'
#' @return El mismo `registros` con las filas corregidas/agregadas.
#' @export
corregir_transiciones_rol <- function(registros, bd_completa, insumos, rango_fechas,
                                      pool, id_proyecto, pase_lista = NULL) {
  fcols    <- as.character(rango_fechas)
  usuarios <- insumos$cat$usuarios
  bd_aux   <- insumos$bd_aux

  meta_por_coord <- bd_aux |>
    dplyr::distinct(municipio, distrito, nombre_brigada, nombre_coordinador,
                    supervisor, status_coord, nombre_zona_trabajo, nombre_grupo)

  altas <- insumos$cat$usuario_log |>
    dplyr::arrange(FechaInsert) |>
    dplyr::distinct(IdUsuario, .keep_all = TRUE) |>
    dplyr::transmute(IdUsuario, fecha_alta = as.Date(FechaInsert)) |>
    dplyr::left_join(usuarios |> dplyr::transmute(IdUsuario = id_usuario, vocero = as.character(num)),
                     by = "IdUsuario") |>
    dplyr::select(vocero, fecha_alta)

  coord_self_nums <- registros |> dplyr::filter(vocero == supervisor) |>
    dplyr::pull(vocero) |> unique()

  coord_num_por_brig <- insumos$cat$brigadas |>
    dplyr::left_join(usuarios |> dplyr::select(id_usuario, num),
                     by = c("id_usuario_brigada" = "id_usuario")) |>
    dplyr::transmute(id_brigada, coord_num = as.character(num))

  # ---------- Tipo A: vocero promovido a coordinador con actividad como vocero ----------
  act_voc <- bd_completa |>
    dplyr::filter(fecha %in% rango_fechas, desglose == "Efectivo") |>
    dplyr::count(usuario_num, id_brigada, fecha, name = "n") |>
    dplyr::left_join(coord_num_por_brig, by = "id_brigada") |>
    dplyr::filter(usuario_num %in% coord_self_nums, usuario_num != coord_num)

  filas_voc <- NULL
  if (nrow(act_voc) > 0) {
    met <- bd_completa |>
      dplyr::filter(fecha %in% rango_fechas, usuario_num %in% act_voc$usuario_num) |>
      dplyr::summarise(
        dur  = mean(as.numeric(dplyr::if_else(desglose == "Efectivo", duracion_minutos, NA_real_)), na.rm = TRUE),
        trab = as.numeric(difftime(max(fecha_fin, na.rm = TRUE), min(fecha_inicio, na.rm = TRUE), units = "hours")),
        .by = c(usuario_num, fecha)) |>
      dplyr::summarise(duracion_promedio = mean(dur, na.rm = TRUE),
                       trabajo_diario    = mean(trab, na.rm = TRUE), .by = usuario_num)

    filas_voc <- act_voc |>
      dplyr::group_by(usuario_num, coord_num, fecha) |>
      dplyr::summarise(n = sum(n), .groups = "drop") |>
      tidyr::pivot_wider(names_from = fecha, values_from = n, values_fill = 0) |>
      dplyr::rename(vocero = usuario_num, supervisor = coord_num) |>
      dplyr::left_join(meta_por_coord, by = "supervisor") |>
      dplyr::left_join(usuarios |> dplyr::transmute(vocero = as.character(num), nombre_vocero = nombre_completo), by = "vocero") |>
      dplyr::left_join(met |> dplyr::rename(vocero = usuario_num), by = "vocero") |>
      dplyr::left_join(altas, by = "vocero") |>
      dplyr::mutate(status_vocero = TRUE, fecha_baja = as.Date(NA), encuesta_id = "285")
    for (fc in fcols) if (!fc %in% names(filas_voc)) filas_voc[[fc]] <- 0
    filas_voc <- filas_voc |>
      dplyr::rowwise() |>
      dplyr::mutate(Total = sum(dplyr::c_across(dplyr::any_of(fcols)), na.rm = TRUE),
                    dias_habiles_trabajados = sum(dplyr::c_across(dplyr::any_of(fcols)) != 0, na.rm = TRUE),
                    promedio_diario = ifelse(dias_habiles_trabajados > 0, Total / dias_habiles_trabajados, 0)) |>
      dplyr::ungroup()

    # Cero los diálogos de la fila de coordinador (ya se mudaron a la fila vocera)
    mask <- registros$vocero %in% act_voc$usuario_num & registros$vocero == registros$supervisor
    registros[mask, fcols] <- 0
    registros$Total[mask] <- 0
    registros$dias_habiles_trabajados[mask] <- 0
    registros$promedio_diario[mask] <- 0
    registros$encuesta_id[mask] <- NA
    cli::cli_alert_info(
      "Transiciones de rol: {sum(mask)} vocero(s) promovido(s) con actividad reubicados a su fila de vocero."
    )
  }

  # ---------- Tipo B: coordinador con asistencia en la semana pero ausente ----------
  filas_coord <- NULL
  if (!is.null(pase_lista) && "obtener_usuario" %in% names(pase_lista)) {
    asis <- pase_lista |>
      dplyr::mutate(fecha = as.Date(fecha)) |>
      dplyr::filter(fecha %in% rango_fechas, !is.na(obtener_usuario)) |>
      dplyr::summarise(ultimo_dia = max(fecha), .by = obtener_usuario) |>
      dplyr::rename(vocero = obtener_usuario) |>
      dplyr::filter(!vocero %in% coord_self_nums)

    if (nrow(asis) > 0) {
      # Estos coordinadores ya no figuran en cat$usuarios (baja / cargo removido):
      # se resuelven directo de la BD.
      u_bd <- dplyr::tbl(pool, "Usuarios") |>
        dplyr::filter(IdProyecto == !!id_proyecto, Num %in% !!asis$vocero) |>
        dplyr::select(Id, Num, Nombre, APaterno, AMaterno, Status) |>
        dplyr::collect() |>
        dplyr::transmute(vocero = as.character(Num), id_usuario = Id,
                         nombre_completo = trimws(paste(Nombre, APaterno, AMaterno)),
                         status = as.logical(Status))
      alta_bd <- dplyr::tbl(pool, "UsuarioLog") |>
        dplyr::filter(IdProyecto == !!id_proyecto, IdUsuario %in% !!u_bd$id_usuario) |>
        dplyr::select(IdUsuario, FechaInsert) |>
        dplyr::collect() |>
        dplyr::summarise(fecha_alta = as.Date(min(FechaInsert)), .by = IdUsuario)
      blog <- dplyr::tbl(pool, "BrigadasLog") |>
        dplyr::filter(IdProyecto == !!id_proyecto, IdUsuario %in% !!u_bd$id_usuario) |>
        dplyr::select(BrigadaId, NombreBrigada, IdUsuario, FechaInsert) |>
        dplyr::collect() |>
        dplyr::arrange(FechaInsert) |>
        dplyr::summarise(nombre_brigada = dplyr::last(NombreBrigada),
                         brigada_id = dplyr::last(BrigadaId), .by = IdUsuario)
      # metadata (municipio/distrito/zona/grupo) del titular actual de esa brigada,
      # cruzada por num de coordinador (evita desajuste de formato de nombre_brigada)
      meta_actual <- coord_num_por_brig |>
        dplyr::transmute(brigada_id = id_brigada, supervisor = coord_num) |>
        dplyr::left_join(dplyr::distinct(meta_por_coord, supervisor, municipio, distrito,
                                         nombre_zona_trabajo, nombre_grupo),
                         by = "supervisor") |>
        dplyr::select(-supervisor)

      filas_coord <- asis |>
        dplyr::left_join(u_bd, by = "vocero") |>
        dplyr::left_join(alta_bd, by = c("id_usuario" = "IdUsuario")) |>
        dplyr::left_join(blog, by = c("id_usuario" = "IdUsuario")) |>
        dplyr::left_join(meta_actual, by = "brigada_id") |>
        dplyr::transmute(
          municipio, distrito, nombre_brigada,
          nombre_coordinador = nombre_completo, supervisor = vocero, status_coord = status,
          nombre_vocero = nombre_completo, vocero, status_vocero = status,
          nombre_zona_trabajo, nombre_grupo,
          fecha_alta, fecha_baja = ultimo_dia, encuesta_id = NA_character_)
      for (fc in fcols) filas_coord[[fc]] <- 0
      filas_coord <- filas_coord |>
        dplyr::mutate(Total = 0, dias_habiles_trabajados = 0,
                      duracion_promedio = NA_real_, trabajo_diario = NA_real_, promedio_diario = 0)
      cli::cli_alert_info(
        "Transiciones de rol: {nrow(filas_coord)} coordinador(es) con pase de lista pero baja al corte reincorporado(s)."
      )
    }
  }

  dplyr::bind_rows(registros, filas_voc, filas_coord)
}
