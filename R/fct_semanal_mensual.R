#' Construir Dashboards y Tablas de Productividad Diaria
#'
#' @description
#' Agrega la actividad operativa diaria en campo y la cruza con la estructura
#' administrativa y los registros de asistencia (pase de lista). Calcula resúmenes
#' jerárquicos (General, Distrito, Brigada) y genera objetos `flextable` listos
#' para producción y reporteo.
#'
#' @details
#' **Lógica de Negocio y Reglas:**
#' * **Deduplicación de Asistencia:** Si un supervisor envía múltiples pases de lista
#'   para el mismo *vocero* en un mismo día, los registros se colapsan en una sola cadena
#'   separada por comas para evitar la duplicación de filas en el cruce final.
#' * **Agrupaciones Geográficas:** Resume las métricas de forma iterativa en tres niveles:
#'   Total (General), Distrito y Brigada.
#' * **Código de Colores KPI (Flextable):**
#'   * **Verde:** Promedio >= 14.5
#'   * **Amarillo:** Promedio >= 10.0
#'   * **Naranja:** Promedio < 10.0
#' * **Estatus de Coordinadores:** Genera indicadores visuales (✅/❌) que denotan si un
#'   coordinador envió su reporte de asistencia (`numero_pases_lista > 0`).
#'
#' @param bd_actividad Tibble. Datos de actividad (típicamente `insumos$bd_actividad`).
#'   Columnas requeridas: `fecha`, `usuario_num`, `seccion`, `desglose`, `duracion_minutos`,
#'   `fecha_inicio`, `fecha_fin`.
#' @param bd_aux Tibble. Estructura administrativa (típicamente `insumos$bd_aux`).
#'   Columnas requeridas: `distrito`, `municipio`, `nombre_brigada`, `nombre_coordinador`,
#'   `supervisor`, `vocero`.
#' @param pl Tibble. Registros de asistencia (ej. `insumos$pase_lista$pl_210`).
#'   Columnas requeridas: `usuario`, `fecha`, `obtener_usuario`, `observaciones` y `asistencia`.
#' @param corte Date o Character. La fecha de corte específica para filtrar el reporte diario.
#' @param distritos Character vector. Distritos específicos a incluir en el desglose
#'   detallado de coordinadores (ej. `c("06", "08", "05")`).
#'
#' @return Una lista completa con 9 elementos:
#' \item{registros}{Actividad de campo agregada por usuario.}
#' \item{pl}{Registros de asistencia deduplicados.}
#' \item{bd_prod_voc}{Datos de productividad cruzados a nivel vocero.}
#' \item{bd_prod_coord}{Datos de productividad cruzados a nivel coordinador.}
#' \item{bd_prod}{Tabla maestra consolidada de productividad.}
#' \item{tabla_acumulada}{Tabla de resumen jerárquico (General -> Distrito -> Brigada).}
#' \item{ft_resumen}{Objeto `flextable` formateado de `tabla_acumulada` con código de colores KPI.}
#' \item{resumen_detalle}{Dataframe con el estatus de asistencia (✅/❌) de los coordinadores.}
#' \item{ft_detalle}{Objeto `flextable` formateado de `resumen_detalle`.}
#'
#' @importFrom dplyr filter group_by summarise if_else n_distinct first n mutate select across everything bind_rows full_join join_by left_join arrange distinct transmute pull case_when starts_with
#' @importFrom tidyr replace_na
#' @importFrom stringr str_extract
#' @importFrom flextable flextable bg bold fontsize width align border_inner border_outer height set_header_labels valign line_spacing
#' @importFrom officer fp_border
#' @export
construir_productividad_diaria <- function(
  bd_actividad,
  bd_aux,
  pl = NULL,
  corte,
  distritos = NULL
) {
  # ---------------------------------------------------------------------------
  # 1) REGISTROS (actividad) -> por usuario_num
  # ---------------------------------------------------------------------------
  registros <- bd_actividad |>
    filter(fecha == corte) |>
    group_by(usuario_num) |>
    summarise(
      seccion = if_else(
        n_distinct(seccion) > 1,
        paste(unique(seccion), collapse = ", "),
        as.character(first(seccion))
      ),
      viviendas_visitadas_nube = n(),
      efectivos = sum(desglose == "Efectivo", na.rm = TRUE),
      no_abrieron_nube = sum(desglose == "No abrieron", na.rm = TRUE),
      rechazaron_nube = sum(desglose == "Sí, rechazaron", na.rm = TRUE),
      cancelado_nube = sum(desglose == "Cancelado", na.rm = TRUE),
      sin_informacion_nube = sum(desglose == "ERROR", na.rm = TRUE),
      #afiliados_nube           = sum(afiliacion == "Sí", na.rm = TRUE),
      duracion_promedio_min = mean(
        duracion_minutos[desglose == "Efectiva"],
        na.rm = TRUE
      ),
      total_horas_trabajadas = as.numeric(difftime(
        max(fecha_fin),
        min(fecha_inicio),
        units = "hours"
      )),
      .groups = "drop"
    )

  # ---------------------------------------------------------------------------
  # 2) PASE DE LISTA (pl) -> colapsar múltiples pases por (supervisor, vocero)
  #    Nota: en tu flujo estabas bind_rows(pl_210, pl_210) (duplicación). Aquí NO.
  # ---------------------------------------------------------------------------
  pl <- pl |>
    mutate(
      vocero = as.character(usuario),
      fecha = as.character(fecha)
    ) |>
    filter(fecha == as.character(corte)) |>
    select(
      supervisor = obtener_usuario,
      vocero,
      asistencia:last_col(),
      observaciones
    ) |>
    summarise(
      numero_pases_lista = n(),
      across(everything(), ~.x), # mantiene columnas
      .by = c(supervisor, vocero)
    )

  # Si hay más de un pase por par supervisor-vocero, colapsa a texto
  if (any(pl$numero_pases_lista > 1, na.rm = TRUE)) {
    pl_multi <- pl |>
      filter(numero_pases_lista > 1) |>
      group_by(supervisor, vocero) |>
      summarise(
        numero_pases_lista = as.character(n()),
        across(
          .cols = -numero_pases_lista,
          .fns = ~ paste(replace_na(as.character(.x), "-"), collapse = ", ")
        ),
        .groups = "drop"
      )

    pl_single <- pl |>
      filter(numero_pases_lista <= 1) |>
      mutate(across(everything(), as.character))

    pl <- bind_rows(pl_single, pl_multi)
  } else {
    pl <- pl |> mutate(across(everything(), as.character))
  }
  # ---------------------------------------------------------------------------
  # 3) BD PROD (voceros + coordinadores) con joins + limpieza NA
  # ---------------------------------------------------------------------------
  bd_prod_voc <- bd_aux |>
    full_join(registros, by = join_by(vocero == usuario_num)) |>
    left_join(pl, by = join_by(supervisor, vocero)) |>
    arrange(as.numeric(distrito), municipio) |>
    distinct() |>
    select(any_of(names(bd_aux)), everything())

  bd_prod_coord <- bd_aux |>
    filter(nombre_brigada != "Bajas") |>
    transmute(
      distrito,
      municipio,
      nombre_brigada,
      nombre_coordinador,
      supervisor,
      status_coord,
      nombre_vocero = nombre_coordinador,
      vocero = supervisor
    ) |>
    distinct(distrito, nombre_coordinador, .keep_all = TRUE) |>
    left_join(registros, by = join_by(supervisor == usuario_num)) |>
    left_join(pl, by = join_by(supervisor, vocero)) |>
    arrange(as.numeric(distrito), municipio) |>
    distinct()

  bd_prod <- bd_prod_coord |>
    bind_rows(
      bd_prod_voc |>
        filter(!vocero %in% unique(bd_prod_coord$vocero))
    ) |>
    arrange(as.numeric(distrito), municipio) |>
    select(any_of(names(bd_aux)), everything()) |>
    mutate(
      across(where(is.numeric), ~ replace_na(.x, 0)),
      across(where(is.character), ~ replace_na(.x, "-")),
      numero_pases_lista = as.numeric(numero_pases_lista),
      distrito = str_extract(nombre_brigada, "^\\d{2}"),
      numero_pases_lista = if_else(
        is.na(numero_pases_lista),
        0,
        numero_pases_lista
      )
    ) |>
    filter(vocero != "-")

  # ---------------------------------------------------------------------------
  # 4) TABLA ACUMULADA (General / Distrito / Brigada) SIN FOR LOOPS
  # ---------------------------------------------------------------------------

  nombres <- bd_prod |>
    distinct(nombre_brigada) |>
    pull(nombre_brigada) |>
    sort()

  base_resumen <- bd_prod |>
    filter(nombre_brigada %in% nombres)

  general <- base_resumen |>
    summarise(
      nivel = "General",
      nombre = "TOTAL",
      usuarios = sum(efectivos > 0, na.rm = TRUE),
      dialogos = sum(efectivos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      promedio = round(dialogos / if_else(usuarios == 0, NA_real_, usuarios), 2)
    )

  por_distrito <- base_resumen |>
    group_by(nombre_brigada) |>
    summarise(
      nivel = "Distrito",
      nombre = paste0("Distrito ", first(distrito)),
      distrito = first(distrito),
      usuarios = sum(efectivos > 0, na.rm = TRUE),
      dialogos = sum(efectivos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      promedio = round(dialogos / if_else(usuarios == 0, NA_real_, usuarios), 2)
    )

  por_brigada <- base_resumen |>
    group_by(nombre_brigada) |>
    summarise(
      nivel = "Brigada",
      nombre = first(nombre_brigada),
      distrito = first(distrito),
      usuarios = sum(efectivos > 0, na.rm = TRUE),
      dialogos = sum(efectivos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      promedio = round(dialogos / if_else(usuarios == 0, NA_real_, usuarios), 2)
    )

  # Orden: General -> Distrito -> Brigadas del distrito
  tabla_acumulada <- bind_rows(
    general |> mutate(.ord0 = 0, .ord1 = 0, .ord2 = 0),
    por_distrito |> mutate(.ord0 = 1, .ord1 = as.numeric(distrito), .ord2 = 0),
    por_brigada |>
      mutate(
        .ord0 = 2,
        .ord1 = as.numeric(distrito),
        .ord2 = as.numeric(factor(nombre))
      )
  ) |>
    arrange(.ord0, .ord1, .ord2) |>
    select(-starts_with(".ord")) |>
    mutate(
      promedio = replace_na(promedio, 0),
      color = case_when(
        promedio >= 14.5 ~ "verde",
        promedio >= 10 ~ "amarillo",
        TRUE ~ "naranja"
      )
    )

  # Flextable resumen (colores sobre promedio)
  columnas_visibles <- c("nivel", "nombre", "usuarios", "dialogos", "promedio")
  ft_resumen <- flextable(tabla_acumulada, col_keys = columnas_visibles) |>
    bg(i = ~ color == "verde", j = "promedio", bg = "#92D050", part = "body") |>
    bg(
      i = ~ color == "amarillo",
      j = "promedio",
      bg = "#FFFF00",
      part = "body"
    ) |>
    bg(
      i = ~ color == "naranja",
      j = "promedio",
      bg = "#FFC000",
      part = "body"
    ) |>
    bold(i = ~ nivel %in% c("General", "Distrito"), part = "body") |>
    fontsize(size = 14, part = "all") |>
    width(j = 1, width = 3.3, unit = "in") |>
    width(j = 2, width = 1.35, unit = "in") |>
    width(j = 3, width = 1.22, unit = "in") |>
    align(j = 3:5, align = "right", part = "all") |>
    border_inner(
      border = fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    border_outer(
      border = fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    height(height = 0.4, part = "body")

  # ---------------------------------------------------------------------------
  # 5) DETALLE coordinadores por distrito (✅/❌) + flextable
  # ---------------------------------------------------------------------------
  resumen_detalle <- bd_prod |>
    filter(distrito %in% distritos) |>
    filter(nombre_coordinador == nombre_vocero) |>
    filter(!is.na(nombre_coordinador) & nombre_coordinador != "-") |>
    mutate(
      tiene_pase = numero_pases_lista > 0,
      estatus_individual = if_else(
        tiene_pase,
        paste0("✅ ", nombre_coordinador),
        paste0("❌ ", nombre_coordinador)
      )
    ) |>
    arrange(distrito, desc(tiene_pase), nombre_coordinador) |>
    group_by(distrito) |>
    summarise(
      total_coord = n_distinct(nombre_coordinador),
      lista_estatus = paste(unique(estatus_individual), collapse = "\n"),
      total_pases_distrito = sum(numero_pases_lista, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(nombre_titulo = paste0("Distrito ", distrito))

  ft_detalle <- flextable(
    resumen_detalle,
    col_keys = c(
      "nombre_titulo",
      "total_coord",
      "lista_estatus",
      "total_pases_distrito"
    )
  ) |>
    set_header_labels(
      nombre_titulo = "Distrito",
      total_coord = "Total Coordinadores",
      lista_estatus = "Estatus de Coordinadores (Pase de Lista)",
      total_pases_distrito = "Total Pases Distrito"
    ) |>
    valign(j = "lista_estatus", valign = "top") |>
    fontsize(size = 12, part = "all") |>
    bold(part = "header") |>
    width(j = 1, width = 1.0, unit = "in") |>
    width(j = 2, width = 1.2, unit = "in") |>
    width(j = 3, width = 4.8, unit = "in") |>
    width(j = 4, width = 1.2, unit = "in") |>
    align(j = c(1, 2, 4), align = "center", part = "all") |>
    align(j = 3, align = "left", part = "all") |>
    border_inner(
      border = fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    border_outer(
      border = fp_border(color = "black", width = 1),
      part = "all"
    ) |>
    bg(part = "header", bg = "#F2F2F2") |>
    line_spacing(space = 1.2, part = "body")

  list(
    registros = registros,
    pl = pl,
    bd_prod_voc = bd_prod_voc,
    bd_prod_coord = bd_prod_coord,
    bd_prod = bd_prod,
    tabla_acumulada = tabla_acumulada,
    ft_resumen = ft_resumen,
    resumen_detalle = resumen_detalle,
    ft_detalle = ft_detalle
  )
}
