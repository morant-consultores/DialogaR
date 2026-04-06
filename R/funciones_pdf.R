# Procesamiento -----------------------------------------------------------
#' Calcular días trabajados
#'
#' Calcula el número de días trabajados excluyendo fines de semana y días festivos.
#'
#' @param bd Un data frame que contiene una columna `fecha` con las fechas de los días trabajados.
#' @param dias_festivos Un vector de fechas correspondientes a los días festivos.
#' @return Un número entero que representa el total de días trabajados.
#' @examples
#' \dontrun{
#' calcular_dias_trabajados(bd, dias_festivos = c("2024-01-01", "2024-12-25"))
#' }
#' @noRd
calcular_dias_trabajados <- function(bd, dias_festivos = NULL) {
  aux <- bd |>
    filter(lubridate::wday(fecha) != 1)
  if (!is.null(dias_festivos)) {
    aux <- aux |>
      filter(!fecha %in% dias_festivos)
  }
  aux |>
    count(fecha) |>
    nrow()
}

#' Calcular meta por zona
#'
#' Calcula la meta de viviendas por zona para una entidad específica, utilizando datos del censo y un parámetro de ajuste.
#'
#' @param entidad Una cadena de texto que indica la entidad de interés.
#' @param zona_seccion Un data frame que contiene las secciones de la zona.
#' @param p Un número (opcional, por defecto 0.7) que indica el porcentaje de viviendas a considerar como meta.
#' @param unidad La variable de agrupación utilizada para calcular la meta en cada grupo.
#' @return Un data frame con la meta de viviendas calculada por unidad.
#' @examples
#' \dontrun{
#' calcular_meta_zona("EntidadA", zona_seccion, p = 0.8, unidad = grupo)
#' }
#' @noRd
calcular_meta_zona <- function(zona_seccion, metas_secc, p = 0.7, unidad) {
  zona_seccion |>
    left_join(metas_secc) |>
    group_by({{ unidad }}) |>
    summarise(meta = sum(tvivparhab, na.rm = T) * p)
}

#' Crear tabla de diálogos
#'
#' Genera un resumen de los diálogos efectivos en una tabla, incluyendo métricas de efectividad y datos recabados.
#'
#' @param bd Un data frame que contiene datos de diálogos y columnas de interés como `desglose`, `duracion_minutos`, `numero`, `direccion_correo`, y `redes`.
#' @param lower Un número que define el límite inferior de la duración ideal de un diálogo en minutos.
#' @param upper Un número que define el límite superior de la duración ideal de un diálogo en minutos.
#' @return Un data frame con el resumen de diálogos efectivos y otros indicadores.
#' @examples
#' \dontrun{
#' crear_tabla_dialogos(bd, lower = 5, upper = 15)
#' }
#' @noRd
crear_tabla_dialogos <- function(
  bd,
  lower,
  upper,
  dias_festivos,
  bd_cruda = NULL
) {
  labels <- c(
    "Diálogos efectivos",
    "Diálogos promedio por día",
    "Efectividad del Diálogo",
    "Diálogos efectivos con la duración ideal",
    "Total de contactos recabados",
    "Total de números de contacto recabados",
    "Total de correos recabados",
    "Nuevos seguidores en redes",
    "Afiliados a Morena"
  )
  orden <- c(
    "efectivos",
    "prom",
    "efect_viv",
    "duracion_ideal",
    "contactos_num",
    "numeros_distintos",
    "correos",
    "redes",
    "afiliacion"
  )
  abrieron <- sum(
    bd$puerta %in% c("Sí, aceptaron", "Sí, rechazaron"),
    na.rm = TRUE
  )

  bd_efec <- bd |>
    filter(desglose == "Efectivo" | is.na(puerta))

  tabla_dial <- bd_efec |>
    mutate(
      ideal = if_else(
        duracion_minutos >= lower & duracion_minutos <= upper,
        1,
        0
      )
    ) |>
    summarise(
      efectivos = sum(bd$desglose == "Efectivo", na.rm = T),
      duracion_ideal = sum(ideal) / efectivos
    ) |>
    mutate(
      prom = efectivos / calcular_dias_trabajados(bd_efec, dias_festivos),
      efect_viv = scales::percent(efectivos / abrieron),
      contactos_num = sum(!is.na(bd_efec$celular) | !is.na(bd_efec$correo)),
      numeros_distintos = length(na.omit(bd_efec$celular)),
      correos = length(na.omit(bd_efec$correo)),
      # grupos_whats = sum(bd_efec$grupo_whats == "Sí", na.rm = T),
      across(-c(duracion_ideal, efect_viv), scales::comma),
      duracion_ideal = scales::percent(duracion_ideal),
    ) |>
    tidyr::pivot_longer(everything()) |>
    mutate(
      name = factor(name, levels = orden, labels = labels)
    ) |>
    arrange(name) |>
    na.omit() |>
    filter(!name %in% c("Diálogos efectivos"))

  tabla_dial <-
    data.frame(name = "Desglose de diálogos", value = "") |>
    bind_rows(tabla_dial)

  return(tabla_dial)
}

#' Crear tabla de desglose
#'
#' Genera una tabla con el desglose de los resultados de una campaña, incluyendo métricas como días trabajados y viviendas visitadas.
#'
#' @param bd Un data frame que contiene datos de visitas y diálogos, incluyendo una columna `desglose`.
#' @param dias_festivos Un vector de fechas correspondientes a los días festivos que deben excluirse del cálculo de días trabajados.
#' @return Un data frame con el desglose de los resultados, organizado por categorías específicas.
#' @examples
#' \dontrun{
#' crear_tabla_desglose(bd, dias_festivos = c("2024-01-01", "2024-12-25"))
#' }
# crear_tabla_desglose <- function(bd, dias_festivos){
#   nombres <- c("Días trabajados", "Total de viviendas visitadas", "Diálogos rechazados", "No abrieron la puerta", "No pasaron los filtros", "Diálogos canceladas", "Penetración territorial")
#   orden <- c("dias", "viviendas", "Rechazado", "No abrieron", "No pasaron filtros", "Cancelado", "Efectivo")
#   viviendas <- nrow(bd)
#   tibble("desglose" = c("dias", "viviendas"), "n" = c(calcular_dias_trabajados(bd, dias_festivos), viviendas)) |>
#     bind_rows(
#       bd |>
#         count(desglose) |>
#         mutate(pct = scales::percent(n/viviendas, accuracy = 0.1))
#     ) |>
#     mutate(desglose = factor(desglose, levels = orden, labels = nombres),
#            n = scales::comma(n)
#     ) |>
#     filter(!is.na(desglose)) |>
#     arrange(desglose) |>
#     tidyr::replace_na(replace = list(pct = "")) |>
#     mutate(desglose = as.character(desglose))
# }
#' @noRd
crear_tabla_desglose <- function(
  bd,
  bd_juarez = NULL,
  dias_festivos,
  promedio_efec_voc_diario = NA
) {
  abrieron <- sum(
    bd$puerta %in% c("Sí, aceptaron", "Sí, rechazaron"),
    na.rm = TRUE
  )
  nombres <- c(
    "Días trabajados",
    "Total de viviendas visitadas",
    "Diálogos Efectivos",
    "Diálogos rechazados",
    "No abrieron la puerta",
    "No pasaron los filtros",
    "Diálogos cancelados",
    "Diálogos rechazados"
  )
  orden <- c(
    "dias",
    "viviendas",
    "Efectivo",
    "Rechazado",
    "No abrieron",
    "No pasaron filtros",
    "Cancelado",
    "Sí, rechazaron"
  )
  viviendas <- nrow(bd)

  tibble(
    "desglose" = c("dias", "viviendas"),
    "n" = c(calcular_dias_trabajados(bd, dias_festivos), viviendas)
  ) |>
    bind_rows(
      bd |>
        count(desglose) |>
        mutate(pct = scales::percent(n / viviendas, accuracy = 0.1))
    ) |>
    mutate(pct = ifelse(desglose == "Efectivo", "", pct)) |>
    mutate(
      desglose = factor(desglose, levels = orden, labels = nombres),
      n = scales::comma(n)
    ) |>
    filter(!is.na(desglose)) |>
    arrange(desglose) |>
    tidyr::replace_na(replace = list(pct = "")) |>
    mutate(desglose = as.character(desglose))|>
    bind_rows(
      data.frame(
        desglose = "Abrieron la puerta",
        n = scales::comma(abrieron),
        pct = scales::percent(abrieron / viviendas, accuracy = 0.1)
      )
    )  |>
    bind_rows(
      data.frame(
        desglose = "Promedio efectivos diarios por vocero",
        n = scales::comma(promedio_efec_voc_diario, accuracy = 0.1),
        pct = ""
      )
    ) |>
    mutate(
      orden = case_when(
        desglose == "Días trabajados" ~ 2,
        desglose == "Total de viviendas visitadas" ~ 3,
        desglose == "Diálogos Efectivos" ~ 8,
        desglose == "Abrieron la puerta" ~ 5,
        desglose == "Diálogos rechazados" ~ 6,
        desglose == "No abrieron la puerta" ~ 4,
        desglose == "Diálogos cancelados" ~ 7,

        TRUE ~ 9 # Cualquier otro valor
      )
    ) |>
    arrange(orden) |>
    select(-orden)
}


#' Procesamiento de efectivos históricos
#'
#' Procesa el historial de diálogos efectivos para una base de datos dada, completando fechas faltantes y calculando acumulados.
#'
#' @param bd Un data frame que contiene los registros de diálogos con una columna `desglose` que identifica el tipo de diálogo y una columna `fecha` con las fechas de cada diálogo.
#' @param corte Una fecha de corte (`Date`) que indica el límite superior de fechas a completar en el historial.
#' @return Un data frame con las fechas completadas desde la primera fecha hasta el corte, con el conteo diario de diálogos efectivos, el promedio diario, un label formateado y el acumulado.
#' @examples
#' \dontrun{
#' procesamiento_efectivos_historico(bd, corte = as.Date("2024-12-31"))
#' }
#' @noRd
procesamiento_efectivos_historico <- function(bd, corte) {
  bd |>
    filter(desglose == "Efectivo") |>
    count(fecha) |>
    tidyr::complete(
      fecha = seq.Date(min(fecha, na.rm = T), as.Date(corte), by = "day"),
      fill = list(n = 0)
    ) |>
    arrange(fecha) |>
    mutate(prom = mean(n), label = scales::comma(n), acumulado = cumsum(n))
}

#' Calcular días hasta fecha de fin excluyendo festivos y domingos
#'
#' Calcula el número de días entre la primera fecha en los datos y una fecha de fin específica, excluyendo días festivos y domingos.
#'
#' @param bd Un data frame que contiene una columna `fecha` con las fechas iniciales.
#' @param fecha_fin Una fecha de fin (`Date`) que indica el límite superior del rango de fechas a calcular.
#' @return Un número entero que representa el total de días hábiles (excluyendo domingos y días festivos) entre la primera fecha de `bd` y `fecha_fin`.
#' @examples
#' \dontrun{
#' calcular_dias_fin(bd, fecha_fin = as.Date("2024-12-31"))
#' }
#' @noRd
calcular_dias_fin <- function(bd, fecha_fin, dias_festivos) {
  aux <- tibble(
    "fecha" = as.Date(seq.Date(
      from = min(as.Date(bd$fecha)),
      to = as.Date(fecha_fin),
      by = 1
    ))
  ) |>
    mutate(dia_semana = wday(fecha)) |>
    filter(dia_semana != 7)

  if (!is.null(dias_festivos)) {
    aux <- aux |>
      filter(!fecha %in% dias_festivos)
  }
  return(
    # aux |>
    #   nrow()
    150
  )
}

#' Calcular semáforo de diálogos efectivos por zona
#'
#' Calcula el nivel de cumplimiento de los diálogos efectivos por zona, generando un semáforo de colores según el progreso respecto a la meta diaria.
#'
#' @param bd Un data frame con los datos de diálogos, que debe incluir las columnas `desglose` y `zona`.
#' @param meta_zona Un data frame que contiene la meta de diálogos por zona, con columnas `zona` y `meta`.
#' @param dias_fin Número total de días hábiles disponibles para alcanzar la meta.
#' @return Un data frame con el cálculo del semáforo por zona, incluyendo el porcentaje de cumplimiento (`semaforo`) y un color representativo (`color`).
#' @examples
#' \dontrun{
#' calcular_semaforo(bd, meta_zona, dias_fin = 20)
#' }
#' @noRd
calcular_semaforo <- function(bd, meta_zona, dias_fin) {
  aux <- bd |>
    filter(desglose == "Efectivo") |>
    summarise(dialogos_efectivos = n(), .by = seccion)

  semaforo <- meta_zona |>
    arrange(seccion) |>
    mutate(id = row_number()) |>
    full_join(aux) |>
    tidyr::replace_na(list(dialogos_efectivos = 0, meta = 0)) |>
    mutate(meta_140 = meta, meta = meta_140 + dialogos_efectivos) |>
    mutate(
      meta_diaria = meta / dias_fin,
      meta_corte = meta_diaria * length(unique(bd$fecha)),
      semaforo = dialogos_efectivos / meta_corte,
      color = case_when(
        semaforo >= 1 ~ color_verde,
        semaforo < 1 & semaforo >= 0.9 ~ color_amarillo,
        semaforo < 0.9 & semaforo > 0 ~ color_rojo,
        T ~ "gray66"
      )
    ) |>
    arrange(desc(semaforo)) |>
    mutate(seccion = factor(seccion))

  return(semaforo)
}

#' Calcular tabla resumen por zona
#'
#' Genera una tabla de resumen por zona que incluye la meta final, meta al día de corte, viviendas visitadas, diálogos efectivos y el porcentaje de avance respecto a la meta del día.
#'
#' @return Un data frame que muestra un resumen detallado por zona, incluyendo información sobre visitas y avance de metas en formato porcentual y numérico.
#' @examples
#' \dontrun{
#' calcular_tabla_zona()
#' }
#' @noRd
calcular_tabla_zona <- function(bd = bd) {
  viviendas_visitadas <- bd |>
    count(zona, name = "Viviendas visitadas")

  afiliados <- bd |>
    filter(afiliacion == "Sí" & desglose == "Efectivo") |>
    count(zona, name = "Afiliados")

  contactos <- bd |>
    filter(desglose == "Efectivo") |>
    summarise(
      contactos = sum(
        (!is.na(correo) | !is.na(celular)) & desglose == "Efectivo"
      ),
      .by = zona
    ) |>
    rename("Contactos recabados" = "contactos")

  tabla_zonas <- semaforo |>
    arrange(desc(dialogos_efectivos)) |>
    mutate(semaforo = scales::percent(semaforo, accuracy = 0.1)) |>
    select(-color) |>
    left_join(viviendas_visitadas) |>
    left_join(afiliados) |>
    left_join(contactos, by = "zona") |>
    tidyr::replace_na(list(
      `Viviendas visitadas` = 0,
      semaforo = "0.0%",
      Afiliados = 0,
      contactos = 0
    )) |>
    select(
      "Distrito" = zona,
      "Meta final" = meta,
      "Meta al día de corte" = meta_corte,
      `Viviendas visitadas`,
      "Diálogos efectivos" = dialogos_efectivos,
      "% de avance\n respecto a meta\ndel día" = semaforo,
      everything(),
      -meta_diaria,
      -id,
      -distrito
    ) |>
    #select(-c(contains("meta"))) |>
    mutate(across(where(is.numeric), scales::comma))

  return(tabla_zonas)
}

calcular_duracion_dialogos <- function(bd, low_lim = 5, upper_lim = 7, corte) {
  aux <- bd |>
    filter(desglose == "Efectivo") |>
    mutate(
      duracion = factor(
        case_when(
          duracion_minutos < low_lim ~
            stringr::str_glue("Menos de {low_lim} minutos"),
          duracion_minutos >= low_lim & duracion_minutos <= upper_lim ~
            stringr::str_glue(
              "Duración ideal:\nDe {low_lim} a {upper_lim} minutos"
            ),
          T ~ stringr::str_glue("Más de {upper_lim}  minutos")
        ),
        levels = c(
          stringr::str_glue("Menos de {low_lim} minutos"),
          stringr::str_glue("Más de {upper_lim}  minutos"),
          stringr::str_glue(
            "Duración ideal:\nDe {low_lim} a {upper_lim} minutos"
          )
        )
      ),
      # duracion = forcats::fct_rev(duracion)
    ) |>
    count(fecha, duracion) |>
    tidyr::complete(
      fecha = seq.Date(min(bd$fecha), as.Date(corte), by = "day"),
      duracion,
      fill = list(n = 0)
    ) |>
    mutate(pct = n / sum(n), .by = fecha)

  return(aux)
}

#' Calcular número de brigadistas requeridos
#'
#' Calcula el número de brigadistas requeridos diariamente para alcanzar la meta de diálogos efectivos antes de la fecha de fin.
#'
#' @param bd Un data frame con el progreso acumulado de diálogos efectivos, que incluye una columna `acumulado` que representa el total acumulado de diálogos hasta cada día.
#' @param dias_a_fin Número total de días restantes para alcanzar la meta final.
#' @param semaforo Un data frame que contiene la meta diaria de diálogos efectivos por zona, incluyendo una columna `meta_diaria` con la meta diaria de cada zona.
#' @return Un data frame con el cálculo diario del número de brigadistas requeridos para alcanzar la meta de diálogos, basado en un promedio de 15 diálogos por brigadista por día.
#' @examples
#' \dontrun{
#' calcular_brigadistas_requeridos(bd, dias_a_fin = 20, semaforo = semaforo)
#' }
#' @noRd
calcular_brigadistas_requeridos <- function(
  bd,
  dias_a_fin = dias_a_fin,
  semaforo = semaforo
) {
  meta_fin <- sum(semaforo$meta_diaria) * dias_a_fin

  res <- bd |>
    mutate(
      dia = row_number(),
      restante = meta_fin - acumulado,
      brigadistas_requeridos = restante / (15 * (dias_a_fin - dia))
    )
  return(res)
}

#' Calcular número de brigadistas por día
#'
#' Calcula el número de brigadistas únicos (voceros) activos cada día, completando los días faltantes hasta una fecha de corte.
#'
#' @param bd Un data frame que contiene los registros de actividad de brigadistas, incluyendo las columnas `fecha` y `usuario_num` (identificador único de cada brigadista).
#' @param corte Una fecha de corte (`Date`) que indica el límite superior de fechas a completar en la serie.
#' @return Un data frame con el conteo diario de brigadistas únicos activos, completando los días sin registros hasta la fecha de corte y asignando un valor de 0 en esos días.
#' @examples
#' \dontrun{
#' #calcular_brigadistas_dia(bd, corte = as.Date("2024-12-31"))
#' }
#' @noRd
calcular_brigadistas_dia <- function(bd, corte) {
  bd |>
    filter(afiliados > 0) |>
    distinct(fecha, usuario_num) |>
    group_by(fecha) |>
    summarise(voceros_dia = n()) |>
    tidyr::complete(
      fecha = seq.Date(min(bd$fecha), as.Date(corte), by = "day"),
      fill = list(voceros_dia = 0)
    )
}

calcular_brigadistas_dia2 <- function(bd, corte) {
  bd |>
    distinct(fecha, usuario_num) |>
    group_by(fecha) |>
    summarise(voceros_dia = n()) |>
    tidyr::complete(
      fecha = seq.Date(min(bd$fecha), as.Date(corte), by = "day"),
      fill = list(voceros_dia = 0)
    )
}


#' Calcular promedio diario de diálogos efectivos por brigadista
#'
#' Calcula el promedio de diálogos efectivos diarios por brigadista y completa las fechas faltantes hasta una fecha de corte.
#'
#' @param bd Un data frame que contiene los registros de diálogos, con columnas `desglose`, `fecha`, y `usuario_num` (identificador único de cada brigadista).
#' @param corte Una fecha de corte (`Date`) que indica el límite superior de fechas a completar en la serie.
#' @return Un data frame con el promedio de diálogos efectivos por día, completando fechas faltantes hasta el corte y asignando un valor promedio de `0` en días sin registros.
#' @examples
#' \dontrun{
#' calcular_dialogos_promedio(bd, corte = as.Date("2024-12-31"))
#' }
#' @noRd
calcular_dialogos_promedio <- function(bd, corte) {
  res <- bd |>
    filter(desglose == "Efectivo") |>
    summarise(n = n(), .by = c(fecha, usuario_num)) |>
    tidyr::complete(
      fecha = seq.Date(min(bd$fecha), as.Date(corte), by = "day"),
      fill = list(n = 0)
    ) |>
    summarise(prom = mean(n), .by = fecha)

  return(res)
}

#' Calcular brigadistas activos en una fecha de corte
#'
#' Calcula el número de brigadistas activos en cada zona en una fecha de corte específica, completando las zonas faltantes y aplicando un color de relleno.
#'
#' @param bd Un data frame que contiene los registros de brigadistas con las columnas `fecha`, `zona`, y `usuario_num`.
#' @param corte Una fecha específica (`Date`) para la cual se desea calcular el número de brigadistas activos por zona.
#' @param fill Un color hexadecimal para el relleno, utilizado en visualizaciones (por defecto es "#6B3A8C").
#' @param zonas Un vector de todas las zonas posibles para completar el conteo de brigadistas en zonas sin actividad en la fecha de corte.
#' @return Un data frame con el número de brigadistas activos en cada zona en la fecha de corte, completando zonas faltantes y aplicando el color de relleno.
#' @examples
#' \dontrun{
#' calcular_brigadistas_corte(bd, corte = as.Date("2024-12-31"), zonas = zonas)
#' }
#' @noRd
calcular_brigadistas_corte <- function(
  bd,
  corte,
  fill = "#6B3A8C",
  zonas = zonas
) {
  bd |>
    filter(fecha == corte) |>
    distinct(zona, usuario_num) |>
    count(zona, name = "brigadistas", sort = T) |>
    tidyr::complete(zona = zonas, fill = list(brigadistas = 0)) |>
    mutate(
      fill = fill,
      zona = forcats::fct_reorder(zona, brigadistas),
      brigadistas = if_else(brigadistas == 0, NA, brigadistas)
    )
}

#' Calcular promedio de horas trabajadas por brigadista hasta la fecha de corte
#'
#' Calcula el promedio de horas trabajadas por brigadista en cada día, excluyendo jornadas de más de 16 horas, y completa las fechas faltantes hasta la fecha de corte.
#'
#' @param bd Un data frame con registros de inicio y fin de cada jornada, que incluye las columnas `fecha`, `usuario_num`, y `fecha_inicio` (tiempo de inicio de la jornada).
#' @param corte Una fecha de corte (`Date`) hasta la cual se desea completar la serie de fechas diarias.
#' @return Un data frame con el promedio diario de horas trabajadas por brigadista, completando días faltantes hasta la fecha de corte y asignando un valor de 0 en días sin registros.
#' @examples
#' \dontrun{
#' calcular_horas_trabajadas(bd, corte = as.Date("2024-12-31"))
#' }
#' @noRd
calcular_horas_trabajadas <- function(bd, corte) {
  bd |>
    group_by(fecha = as.Date(fecha_inicio), usuario_num) |>
    summarise(
      hora_inicio = min(fecha_inicio, na.rm = TRUE),
      hora_fin = max(fecha_fin, na.rm = TRUE),
      horas_trabajadas = as.numeric(difftime(
        max(fecha_fin, na.rm = TRUE),
        min(fecha_inicio, na.rm = TRUE),
        units = "hours"
      )),
    ) |>
    summarise(horas_trabajadas = mean(horas_trabajadas, na.rm = T)) |>
    tidyr::complete(
      fecha = seq.Date(min(bd$fecha), as.Date(corte), by = "day"),
      fill = list(horas_trabajadas = 0)
    )
}

#' Calcular conocimiento en diálogos efectivos
#'
#' Calcula el número de diálogos efectivos por categorías especificadas y su proporción, permitiendo el análisis de distribución de conocimiento en el equipo.
#'
#' @param bd Un data frame con los registros de diálogos, que incluye la columna `desglose` para filtrar los diálogos efectivos.
#' @param ... Variables adicionales para agrupar y contar, permitiendo segmentar el conocimiento en distintas categorías.
#' @return Un data frame con el conteo de diálogos efectivos por cada categoría especificada y su proporción (`media`).
#' @examples
#' \dontrun{
#' calcular_conocimiento(bd, zona, usuario_num)
#' }
#' @noRd
calcular_conocimiento <- function(bd, ...) {
  bd |>
    filter(desglose == "Efectivo") |>
    count(...) |>
    mutate(media = n / sum(n))
}

#' Calcular distribución de opiniones en diálogos efectivos
#'
#' Calcula la distribución de opiniones sobre una variable específica en diálogos efectivos, permitiendo agrupar por una unidad opcional.
#'
#' @param bd Un data frame con los registros de diálogos, que incluye la columna `desglose` para filtrar los diálogos efectivos.
#' @param var_conocimiento Una cadena que indica el nombre de la columna que evalúa el conocimiento (donde el valor debe ser "Sí").
#' @param var_opinion Una cadena que indica el nombre de la columna que representa la variable de opinión a analizar.
#' @param levels Un vector con los niveles para ordenar la variable de opinión.
#' @param labels Un vector con las etiquetas para cada nivel de la variable de opinión.
#' @param fill Un color hexadecimal para el relleno, utilizado en visualizaciones (por defecto es "#6B3A8C").
#' @param unidad Una cadena opcional que indica el nombre de una columna para agrupar la opinión por una unidad específica (por defecto es `NULL`).
#' @return Un data frame con el conteo y proporción de opiniones sobre la variable seleccionada, incluyendo una etiqueta de porcentaje y un color de relleno.
#' @noRd
calcular_opinion <- function(
  bd,
  var_conocimiento,
  var_opinion,
  levels,
  labels,
  fill = "#6B3A8C",
  unidad = NULL
) {
  bd_filtered <- bd |>
    filter(desglose == "Efectivo", .data[[var_conocimiento]] == "Sí")
  if (is.null(unidad)) {
    result <- bd_filtered |>
      group_by(.data[[var_opinion]]) |>
      summarise(n = n()) |>
      mutate(
        media = n / sum(n),
        label = scales::percent(media, accuracy = 1),
        !!rlang::sym(var_opinion) := factor(
          .data[[var_opinion]],
          levels = levels,
          labels = labels
        ),
        fill = fill
      ) |>
      ungroup()
  } else {
    result <- bd_filtered |>
      group_by(.data[[unidad]], .data[[var_opinion]]) |>
      summarise(n = n()) |>
      mutate(
        media = n / sum(n),
        label = scales::percent(media, accuracy = 1),
        !!rlang::sym(var_opinion) := factor(
          .data[[var_opinion]],
          levels = levels,
          labels = labels
        ),
        fill = fill
      ) |>
      ungroup()
  }

  return(result)
}

#' Crear capa espacial filtrada para un candidato
#'
#' Crea una capa espacial (`shp`) filtrada según una variable específica, al combinar datos de meta y actividad.
#'
#' @param shp Un objeto `sf` que representa la capa espacial base (shapefile).
#' @param meta Un data frame que contiene la meta de cada unidad espacial.
#' @param bd Un data frame que contiene datos de actividad, incluyendo la variable a filtrar.
#' @param variable Una cadena que indica el nombre de la variable en `bd` para aplicar el filtro.
#' @param valor El valor específico de `variable` que se desea filtrar en `bd`.
#' @return Un objeto `sf` con la capa espacial combinada y filtrada según la proporción de actividad y el valor de la variable especificada.
#' @examples
#' \dontrun{
#' crear_shp_candidato(shp, meta, bd, variable = "apoyo", valor = "Alto")
#' }
#' @noRd
crear_shp_candidato <- function(shp, meta, bd, variable, valor) {
  aux <- meta |>
    left_join(bd) |>
    mutate(pct = n / meta) |>
    filter(pct >= 0.3 & .data[[variable]] == valor)

  shp |>
    left_join(aux)
}

#' Graficar el historial de diálogos efectivos
#'
#' Genera una gráfica interactiva o estática del historial de diálogos efectivos acumulados o diarios, con la opción de agregar una meta diaria acumulada.
#'
#' @param bd Un data frame con los registros de diálogos efectivos, incluyendo columnas como `fecha`, `acumulado`, y `n`.
#' @param color_linea_acum Color hexadecimal para la línea acumulada de diálogos.
#' @param color_meta_me Color hexadecimal para la línea de la meta mejor escenario.
#' @param color_zona Color hexadecimal para el área de la zona de referencia (por defecto es "#e9c46a").
#' @param acumulado Lógico. Si es `TRUE`, grafica el total acumulado; si es `FALSE`, grafica el conteo diario.
#' @param interactivo Lógico. Si es `TRUE`, genera una gráfica interactiva con `highcharter`; si es `FALSE`, usa `ggplot2`.
#' @param total_usuario Número total de usuarios o brigadistas activos, usado para calcular la meta diaria acumulada.
#' @param meta_diaria Meta diaria de diálogos efectivos, utilizada en el cálculo de la meta acumulada.
#' @return Una gráfica de diálogos efectivos acumulados o diarios, ya sea interactiva o estática.
#' @examples
#' \dontrun{
#' graficar_efectivos_historico(bd, color_linea_acum = "#3498db", color_meta_me = "#e74c3c", acumulado = TRUE, interactivo = FALSE, total_usuario = 50, meta_diaria = 100)
#' }
#' @noRd
graficar_efectivos_historico <- function(
  bd,
  color_linea_acum,
  color_meta_me,
  color_zona = "#e9c46a",
  acumulado,
  interactivo = T,
  total_usuario,
  meta_diaria,
  corte = Sys.Date() - 1
) {
  if (acumulado) {
    if (interactivo) {
      aux <- tibble(fecha = sort(unique(bd$fecha))) |>
        mutate(dias = row_number(), meta = dias * meta_diaria * total_usuario)

      hchart(bd, "line", hcaes(x = fecha, y = acumulado)) %>%
        hc_add_series(
          bd,
          type = "scatter",
          hcaes(x = fecha, y = acumulado),
          marker = list(radius = 4, fillColor = color_linea_acum),
          name = "Registros",
          tooltip = list(pointFormat = '{point.y} diálogos')
        ) %>%
        hc_yAxis(
          title = list(text = "Diálogos"),
          labels = list(
            formatter = JS(
              "function() { return Highcharts.numberFormat(this.value, 0, ',', ','); }"
            )
          ),
          min = 0,
          max = max(bd$acumulado) + 25
        ) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_title(text = "Efectivos Históricos Acumulados") %>%
        hc_colors(colors = c(color_linea_acum)) %>%
        hc_plotOptions(line = list(lineWidth = 2)) |>
        hc_add_series(aux, type = "line", hcaes(x = fecha, y = meta)) |>
        hc_add_series(
          aux,
          type = "scatter",
          hcaes(x = fecha, y = meta),
          tooltip = list(pointFormat = '{point.y} diálogos'),
          marker = list(radius = 4, fillColor = color_meta_me),
          name = "Meta mejor escenario"
        ) %>%
        hc_colors(colors = c("#9a8c98")) %>%
        hc_plotOptions(line = list(lineWidth = 2))
    } else {
      bd |>
        ggplot(aes(x = fecha, y = acumulado)) +
        geom_line(linewidth = 2, alpha = 0.8, color = color_linea_acum) +
        geom_point(size = 3, color = color_linea_acum) +
        scale_y_continuous(labels = scales::comma) +
        geom_text(
          aes(label = label),
          nudge_x = 0.25,
          nudge_y = 0.25,
          vjust = -1,
          check_overlap = TRUE,
          color = "gray10",
          size = 3
        ) +
        theme_minimal() +
        labs(x = "", y = "Diálogos") +
        ylim(c(0, max(bd$acumulado) + 25))
    }
  } else {
    fecha_min <- min(bd$fecha)
    fecha_max <- max(bd$fecha)
    prom <- unique(bd$prom)
    if (interactivo) {
      hchart(bd, "line", hcaes(x = fecha, y = n)) %>%
        hc_add_series(
          bd,
          type = "scatter",
          hcaes(x = fecha, y = n),
          marker = list(radius = 4, fillColor = color_linea_acum),
          name = "Diálogos"
        ) %>%
        hc_tooltip(pointFormat = '<b>{point.y}</b>') %>%
        hc_yAxis(
          title = list(text = "Diálogos"),
          labels = list(
            formatter = JS(
              "function() { return Highcharts.numberFormat(this.value, 0, ',', ','); }"
            )
          ),
          min = 0,
          max = max(bd$n) + 25,
          plotBands = list(
            list(from = 0, to = prom, color = color_zona, opacity = 0.5)
          )
        ) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_title(text = "Efectivos Históricos") %>%
        hc_colors(colors = c(color_linea_acum)) %>%
        hc_plotOptions(line = list(lineWidth = 2))
    } else {
      ggplot(bd, aes(x = fecha, y = n)) +
        # annotate("rect",
        #          fill = color_zona, alpha = 0.5,
        #          xmin = as.Date(fecha_min), xmax = as.Date(fecha_max),
        #          ymin = 0, ymax = prom) +
        geom_line(linewidth = 2, alpha = 0.8, color = "gray66") +
        geom_point(size = 3, color = color_linea_acum) +
        scale_y_continuous(
          labels = scales::comma,
          limits = c(0, max(bd$n) + 25)
        ) +
        scale_x_date(
  breaks = function(x) {
    x <- as.Date(x)

    xmin <- min(x, na.rm = TRUE)
    xmax <- min(as.Date(corte), max(x, na.rm = TRUE))  # asegura no pasar de corte

    # breaks semanales (puedes cambiar week_start si quieres)
    brks <- seq(
      from = lubridate::floor_date(xmin, "week", week_start = 1), # 1=lunes
      to   = lubridate::ceiling_date(xmax, "week", week_start = 1),
      by   = "1 week"
    )
    brks <- brks[brks >= xmin & brks <= xmax]

    # fuerza incluir el último día (xmax) aunque no sea lunes
    brks <- sort(unique(c(brks, xmax)))

    brks
  },
  date_labels = "%d %b"
)+
        geom_text(
          aes(label = label),
          # nudge_x = 0.25,
          nudge_y = 0.25,
          vjust = -1,
          check_overlap = TRUE,
          color = "gray10",
          size = 5
        ) +
        theme_minimal(base_size = 16) +
        labs(x = "", y = "Diálogos")
    }
  }
}

estilizar_tablas <- function(bd, color, remover_encabezado = T) {
  tab <- bd |>
    gt::gt() |>
    gt::cols_align(align = "left", columns = 1) |>
    gtExtras::gt_theme_nytimes() |>
    tab_options(
      table.font.size = px(18),
      table.font.names = "Monserrat",
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      column_labels.font.size = px(16),
      column_labels.font.weight = "bold"
    )

  if (remover_encabezado == T) {
    tab <- tab |>
      gt::tab_options(column_labels.hidden = TRUE)
  }
  return(tab)
}

formato_tabla <- function(
  bd,
  negritas = NULL,
  font_size = 18,
  ajuste_tab = F,
  incluye_head = F,
  height_gen = 5.96,
  width_gen = 12.42,
  borde = "black"
) {
  tabla_flex <- bd |>
    flextable(cwidth = 3, cheight = 0.7) |>
    border_remove() |>
    # flextable::delete_part(part = "header") |>
    border_inner_h(
      part = "body",
      border = fp_border(color = borde, width = 1)
    ) |>
    fontsize(size = font_size, part = "all") |>
    font(fontname = font_family, part = "all") |>
    flextable::bold(i = c(1), part = "header", bold = T) |>
    flextable::bg(i = 1, bg = borde, part = "header") |>
    color(i = 1, color = "white", part = "header") %>%
    {
      if (!is.null(negritas)) {
        # Negrita en las filas indicadas
        flextable::bold(., i = negritas, part = "body", bold = TRUE)
      } else {
        .
      }
    } %>%
    flextable::padding(part = "all", padding.top = 0, padding.bottom = 0) |>
    autofit()

  if (ajuste_tab) {
    n_filas <- bd |> nrow()
    n_colum <- bd |> ncol()

    if (incluye_head) {
      height_fil = height_gen / (n_filas + 1)
    } else {
      height_fil = height_gen / (n_filas)
    }
    width_col = width_gen / n_colum

    tabla_flex <- tabla_flex |>
      flextable::set_table_properties(layout = "autofit") %>%
      flextable::width(j = c(1:n_colum), width = width_col, unit = "in") %>%
      flextable::height_all(height = height_fil, unit = "in", part = "body") %>%
      {
        if (incluye_head) {
          flextable::height(
            x = .,
            height = height_fil,
            unit = "in",
            part = "head"
          )
        } else {
          .
        }
      } %>%
      flextable::hrule(rule = "exact", part = "body")
  }

  return(tabla_flex)
}

graficar_barras_apiladas <- function(bd, x, y, fill, title, subtitle) {
  bd |>
    ggplot(aes(x = {{ x }}, y = {{ y }}, fill = {{ fill }})) +
    geom_col(width = 0.9) +
    tema_m(base_size = 25) +
    scale_y_continuous(labels = scales::percent) +
    ggfittext::geom_bar_text(
      data = bd |>
        filter(grepl("ideal", duracion)),
      aes(label = scales::percent({{ y }}, accuracy = 1)),
      position = "stack"
    ) +
 scale_x_date(
  breaks = scales::breaks_pretty(n = 5),
  date_labels = "%d %b",
  guide = ggplot2::guide_axis(check.overlap = TRUE)
 )+
    scale_fill_manual(values = c("gray55", "gray77", color_buena)) +
    labs(title = title, subtitle = subtitle, fill = "")
}

area_general <- function(
  base,
  base_parametros,
  jovenes,
  adultos,
  mayores,
  color_texto
) {
  total <- rowSums(base_parametros)

  par_jovenes <- filter(base_parametros) |>
    pull(total_18_29) /
    total
  par_adultos <- filter(base_parametros) |>
    pull(total_30_59) /
    total
  par_mayores <- filter(base_parametros) |>
    pull(total_60_y_mas) /
    total

  base |>
    filter(desglose == "Efectivo") |>
    mutate(
      edad = as.double(edad),
      rango_edad = case_when(
        edad >= 18 & edad <= 29 ~ "Jóvenes",
        edad >= 30 & edad <= 59 ~ "Adultos",
        edad >= 60 ~ "Adultos mayores"
      ),
      rango_edad = factor(
        rango_edad,
        levels = c("Adultos mayores", "Adultos", "Jóvenes")
      )
    ) |>
    count(fecha, rango_edad) |>
    tidyr::complete(fecha, rango_edad, fill = list(n = 0)) |>
    mutate(pct = n / sum(n), .by = fecha) |>
    ggplot(aes(x = fecha, y = pct, fill = rango_edad)) +
    geom_area(alpha = 0.5) +
    scale_fill_manual(
      values = c(
        "Jóvenes" = jovenes,
        "Adultos" = adultos,
        "Adultos mayores" = mayores
      )
    ) +
    geomtextpath::geom_textline(
      aes(y = par_jovenes, x = fecha, label = "Jóvenes"),
      color = color_texto,
      linetype = "dashed",
      linewidth = 1,
      size = 5
    ) +
    geomtextpath::geom_textline(
      aes(y = par_adultos + par_jovenes, x = fecha, label = "Adultos"),
      color = color_texto,
      linetype = "dashed",
      linewidth = 1,
      size = 5
    ) +
    geomtextpath::geom_textline(
      aes(y = 1, x = fecha, label = "Adultos mayores*"),
      color = color_texto,
      linetype = "dashed",
      linewidth = 1,
      size = 5
    ) +
    #scale_x_date(date_breaks = "1 day", labels = scales::date_format("%b %d"))
    scale_x_date(labels = scales::date_format("%b %d")) +
    scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent)
}

graficar_brigadistas_dia <- function(
  brigadistas_dia,
  brigadistas_requeridos,
  corte,
  color_linea_acum = "#7d1f32"
) {
  brigadistas_dia |>
    ggplot(aes(x = fecha, y = voceros_dia)) +
    # annotate("rect",
    #          fill = color_zona, alpha = 0.5,
    #          xmin = as.Date(fecha_min), xmax = as.Date(fecha_max),
    #          ymin = 0, ymax = prom) +
    geom_line(linewidth = 2, alpha = 0.8, color = "gray66") +
    geom_point(size = 3, color = color_linea_acum) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, max(brigadistas_dia$voceros_dia) + 10)
    ) +
    #scale_x_date(date_breaks = "day", date_labels = "%d %b") +
    scale_x_date(
  breaks = function(x) {
    x <- as.Date(x)

    xmin <- min(x, na.rm = TRUE)
    xmax <- min(as.Date(corte), max(x, na.rm = TRUE))  # asegura no pasar de corte

    # breaks semanales (puedes cambiar week_start si quieres)
    brks <- seq(
      from = lubridate::floor_date(xmin, "week", week_start = 1), # 1=lunes
      to   = lubridate::ceiling_date(xmax, "week", week_start = 1),
      by   = "1 week"
    )
    brks <- brks[brks >= xmin & brks <= xmax]

    # fuerza incluir el último día (xmax) aunque no sea lunes
    brks <- sort(unique(c(brks, xmax)))

    brks
  },
  date_labels = "%d %b") +
    geom_text(
      aes(label = scales::comma(voceros_dia)),
      # nudge_x = 0.25,
      nudge_y = 0.25,
      vjust = -1,
      check_overlap = TRUE,
      color = "gray10",
      size = 4
    ) +
    theme_minimal(base_size = 16) +
    labs(x = "", y = "Voceros")
  # ggplot(aes(x = fecha, y = voceros_dia)) +
  # geom_line(color = color_CHISUNIDO, linewidth = 1.5) +
  # geom_area(stat = "identity", fill = color_CHISUNIDO, alpha = 0.5) +
  # geom_text(data = . %>%
  #             filter(fecha == corte),
  #           aes(label = paste0(voceros_dia)), vjust = 0, hjust = 0.5, size = 10) +
  # # geomtextpath::geom_textline(data = brigadistas_requeridos,
  # #                             aes(x = as.Date(fecha),  # Asegurar que fecha sea Date
  # #                                 y = brigadistas_requeridos,
  # #                                 label = glue::glue("Usuarios requeridos hoy: {round(max(brigadistas_requeridos, na.rm = TRUE))}")),  # Evitar NA
  # #                             color = "red", linewidth = 1.5) +
  # # scale_x_date(date_breaks = "1 day", date_labels = "%b %d",
  # #              expand = expansion(add = c(0.1, 0.5))) +
  # scale_y_continuous(expand = expansion(add = c(0, 10))) +
  # tema_m() +
  # theme(axis.text.x = element_text(angle = 90,  size = 12),
  #       axis.ticks.x = element_line(linewidth = 2),
  #       legend.title = element_text(hjust = 0.5),
  #       panel.grid.major.y = element_line(color="#C5C5C5",linetype = "dotted"))
}

graficar_dialogos_promedio <- function(bd) {
  bd |>
    ggplot(aes(x = fecha, y = prom)) +
    geom_col(color = "gray66", alpha = 0.6) +
    ggfittext::geom_bar_text(aes(label = round(prom))) +
    geom_smooth(
      color = color_CHISUNIDO,
      method = "lm",
      se = F,
      linewidth = 1.5
    ) +
    scale_y_continuous(labels = scales::comma) +
    tema_m(base_size = 25)
}

formato_datatable <- function(bd) {
  bd |>
    datatable(
      rownames = FALSE,
      class = 'stripe hover',
      options = list(
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json'
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$('table').css({'font-family': 'Monserrat', 'font-size': '14px'});",
          "$('.dataTables_info').css({'font-size': '12px'});",
          "$('.dataTables_length').css({'font-size': '12px'});",
          "$('.dataTables_filter').css({'font-size': '12px'});",
          "$('.dataTables_paginate').css({'font-size': '12px'});",
          "$('table.dataTable tbody tr').css({'padding': '8px 0'});",
          "}"
        ),
        searching = FALSE
      )
    )
}

graficar_barras_simples <- function(
  bd,
  x,
  y,
  fill,
  label,
  percent,
  x_title = "",
  y_title = ""
) {
  g <- bd |>
    ggplot(aes(
      x = reorder({{ x }}, {{ x }}),
      y = {{ y }},
      fill = {{ fill }},
      label = {{ label }}
    )) +
    geom_col(width = 0.6) +
    coord_flip() +
    tema_m(base_size = 20) +
    scale_fill_identity() +
    ggfittext::geom_bar_text(size = 16) +
    scale_y_continuous(labels = ~ scales::comma(.x, accuracy = 1)) +
    labs(x = x_title, y = y_title)

  if (percent == T) {
    g <- g +
      scale_y_continuous(labels = scales::percent)
  }
  return(g)
}

graficar_mapa <- function(
  bd,
  fill,
  nombre = "Brigadistas",
  linewidth = 0.5,
  low = "#F00086",
  high = color_si,
  percent = F,
  caption_text = NULL,
  width_caption = 35,
  size_caption = 9
) {
  g <- bd |>
    ggplot() +
    geom_sf(
      aes(fill = !!rlang::sym(fill)),
      linewidth = linewidth,
      color = "gray19"
    ) +
    theme_void(base_family = "Poppins", base_size = 18) +
    guides(
      color = guide_legend(override.aes = list(fill = "#dee2e6"), title = "")
    ) +
    theme(legend.position = "bottom") +
    guides(
      fill = guide_colorbar(
        barwidth = 12,
        barheight = 1,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    {
      if (!is.null(caption_text)) {
        labs(caption = stringr::str_wrap(caption_text, width = width_caption))
      }
    } +
    {
      if (!is.null(caption_text)) {
        theme(plot.caption = element_text(size = size_caption))
      }
    }
  if (percent == T) {
    g +
      scale_fill_gradient(
        low = low,
        high = high,
        guide = "colourbar",
        name = nombre,
        na.value = "white",
        labels = ~ scales::percent(.x, accuracy = 1)
      )
  } else if (percent == F) {
    g +
      scale_fill_gradient2(
        low = low,
        high = high,
        midpoint = median(bd[[fill]], na.rm = T),
        guide = "colourbar",
        name = nombre,
        na.value = "gray55"
      )
  }
}

graficar_horas_trabajadas <- function(bd, corte) {
  bd |>
    ggplot(aes(x = fecha, y = horas_trabajadas)) +
    geom_line(color = color_CHISUNIDO, linewidth = 1.5) +
    geom_area(stat = "identity", fill = color_CHISUNIDO, alpha = 0.5) +
    geom_text(
      data = . %>%
        filter(fecha == corte),
      aes(
        label = paste0(
          round(horas_trabajadas, digits = 1),
          " horas trabajadas\n",
          "en el día de corte"
        )
      ),
      vjust = -1,
      hjust = 2,
      size = 8
    ) +
    # scale_x_date(date_breaks = "1 day", date_labels = "%b %d",
    #              expand = expand_scale(add = c(0.1, 0.5))) +
    scale_x_date(
      #limits = c(min(brigadistas_dia$fecha),max(brigadistas_dia$fecha)),
      # Usamos una función anónima para calcular breaks “bonitos” (pretty)
      # y agregar además la fecha máxima (último día).
      breaks = function(x) {
        brks_num <- scales::pretty_breaks(n = 16)(x)
        brks_num <- brks_num[brks_num <= as.Date(corte) & brks_num >= min(x)]
      },
      date_labels = "%d %b",
      expand = expand_scale(add = c(0.1, 0.5))
    ) +
    scale_y_continuous(n.breaks = 8, limits = c(0, 10)) +
    tema_m() +
    theme(
      axis.text.x = element_text(angle = 90, size = 12),
      axis.ticks.x = element_line(linewidth = 2),
      legend.title = element_text(hjust = 0.5),
      panel.grid.major.y = element_line(color = "#C5C5C5", linetype = "dotted")
    ) +
    labs(
      caption = "Las horas son la diferencia entre el inicio del primer diálogo y el final del último del día"
    )
}

graficar_gauge <- function(
  bd,
  color_principal,
  color_secundario = "gray80",
  escala,
  size_text_pct
) {
  g <- bd %>%
    ggplot() +
    geom_rect(
      aes(xmin = 2, xmax = 3, ymin = 0, ymax = media),
      fill = color_principal,
      color = "white",
      alpha = .95
    ) +
    geom_rect(
      aes(xmin = 2, xmax = 3, ymin = media, ymax = escala[2]),
      fill = color_secundario,
      color = "white"
    )

  if (escala[2] == 1) {
    g <- g +
      geom_text(
        aes(
          x = 0,
          y = media,
          label = scales::percent(x = media, accuracy = 1.)
        ),
        size = size_text_pct,
        family = "Poppins",
        nudge_y = 0
      )
  } else {
    g <- g +
      geom_text(
        aes(x = 0, y = media, label = scales::comma(x = media, accuracy = 1.1)),
        size = size_text_pct,
        family = "Monserrat",
        nudge_y = 0.25
      )
  }

  g <- g +
    scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) +
    scale_y_continuous(limits = c(0, escala[2])) +
    xlab("") +
    ylab("") +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      text = element_text(size = 80, family = "Poppins")
    )

  return(g)
}
#' Genera una tabla resumen de usuarios y su actividad
#'
#' Esta función procesa una base de datos de actividad de usuarios y genera un resumen
#' con métricas clave como el número de usuarios activos, diálogos promedio por usuario,
#' horas trabajadas y porcentaje de usuarios con al menos 6 horas laboradas.
#'
#' @param bd Un data frame que contiene los datos de actividad de los usuarios. Debe incluir las columnas:
#'   - `fecha`: Fecha de la actividad.
#'   - `usuario_num`: Identificador único de usuario.
#'   - `desglose`: Tipo de resultado del diálogo (e.g. "Efectivo").
#'   - `fecha_inicio`: Hora de inicio de actividad del usuario.
#'   - `fecha_fin`: Hora de fin de actividad del usuario.
#' @param voceros_alta Número total de voceros dados de alta.
#' @param coordinadores_alta Número total de coordinadores dados de alta.
#' @param corte Fecha de corte (`Date`). Solo se consideran los registros cuya `fecha` coincida con este valor.
#'
#' @return Un data frame con las siguientes columnas:
#'   - `name`: Descripción de la métrica calculada.
#'   - `value`: Valor de la métrica calculada.
#'
#' @section Seguridad y Privacidad:
#' Esta función procesa datos de actividad de usuarios (encuestadores) que pueden incluir
#' identificadores de persona (`usuario_num`). Los resultados son métricas agregadas sin
#' identificadores individuales. Cumple con ISO 27001 A.8.2 (clasificación de información).
#'
#' @import dplyr
#' @import tidyr
#' @import scales
#' @export

crear_tabla_usuarios <- function(bd, voceros_alta, coordinadores_alta, corte) {
  labels <- c(
    "Total de usuarios dados de alta",
    "Total de coordinadores dados de alta",
    "Total de usuarios activos el día de corte",
    "Diálogos efectivos el día de corte",
    "Diálogos efectivos promedio por usuario el día de corte",
    "Horas promedio laboradas en el día de corte"
  )
  bd |>
    filter(fecha == corte) |>
    summarise(
      dialogos = sum(desglose == "Efectivo"),
      horas_trabajadas = difftime(
        max(fecha_fin),
        min(fecha_inicio),
        units = "hours"
      ),
      .by = usuario_num
    ) |>
    summarise(
      dialogos_totales = scales::comma(sum(dialogos)),
      dialogos_promedio = round(mean(dialogos), digits = 0),
      usuarios_activos_corte = n_distinct(usuario_num),
      horas_trabajadas_mean = round(mean(horas_trabajadas), 0)
        ) |>
    transmute(
      voceros_alta = voceros_alta,
      coordinadores_alta = coordinadores_alta,
      usuarios_activos_corte,
      dialogos_totales,
      dialogos_promedio,
      horas_trabajadas_mean,
    ) |>
    mutate(across(everything(), as.character)) |>
    tidyr::pivot_longer(cols = everything()) |>
    mutate(value = ifelse(value == "NaN" | is.na(value), "-", value)) |>
    mutate(name = factor(name, levels = name, labels = labels))
}


#'  Funcion creadora mapa de dialogos efectivos
#'
#' Grafica el el avance de dialogos, o de n elementos en un mapa
#'
#' @param bd Un data frame que contiene una columna `n` con el numnero de observaciones o registrs
#' @param nombre_mapa Nombre que se le asigna al label del gradiente
#' @param label_mapa Toma una variable de `bd`  y lo muestra como los labels de los poligonos
#' @param color_low Color minimimo del gradiente
#' @param color_high Nombre maximo del gradiente
#' @return Un mapa con degradado segun los valores de `n` en cada poligono
#' @examples
#' \dontrun{
#' calcular_dias_trabajados(bd, dias_festivos = c("2024-01-01", "2024-12-25"))
#' }
#' @noRd
creacion_mapas_dialogos <- function(
  bd,
  nombre_mapa,
  label_mapa,
  color_low = "#9FA3BF",
  color_high = "#351062",
  color_label = "black",
  fill_label = "white",
  etiquetas = FALSE
) {

  mapa_salida <- bd |>
    ggplot(aes(fill = n)) +
    geom_sf(linewidth = 0.2, color = "gray19") +
    theme_void() +
    scale_fill_gradient(
      low = color_low,
      high = color_high,
      name = nombre_mapa,
      na.value = "white"
    )

  if (etiquetas) {
    mapa_salida <- mapa_salida +
      ggsflabel::geom_sf_label_repel(
        data = bd |> dplyr::filter(!is.na(n)),
        aes(label = !!rlang::sym(label_mapa)),
        color = color_label,
        fill = fill_label
      )
  }

  return(mapa_salida)
}


# Geraficar lollipos de diferencias
graficar_lolipop_diferencias <- function(
  bd,
  orden_variablePrincipal,
  colores_variables_secundarias,
  nudge_x = 0.05,
  size_geom_text = 6,
  caption = "",
  wrap_y = 25,
  wrap_caption = 25,
  limits = c(0, 0.75),
  traslape = F,
  limite_dif_pct = 0.02,
  ajuste_pos = 0.02
) {
  if (traslape) {
    bd <- bd |>
      group_by(variable_principal) |>
      mutate(
        #mean_diff_pos = min(mean) + (max(mean)-min(mean))/2,
        mean_dif_traslap = (max(mean) - min(mean)),
        mean_pos = ifelse(mean_dif_traslap <= limite_dif_pct, ajuste_pos, 0),
        mean_pos = ifelse(mean != min(mean) & mean != max(mean), 0, mean_pos),
        mean_pos = ifelse(mean == min(mean), mean_pos * (-1), mean_pos),
        mean_pos = mean_pos + mean
      ) |>
      ungroup()
  }

  g <-
    bd |>
    ggplot(aes(
      x = factor(variable_principal, levels = orden_variablePrincipal),
      y = mean,
      color = tema,
      group = variable_principal
    )) +
    geom_line(color = "#a2d2ff", linewidth = 4.5, alpha = 0.5) +
    geom_point(size = 7) +
    {
      if (traslape) {
        geom_text(
          aes(label = scales::percent(x = mean, accuracy = 1.0), y = mean_pos),
          nudge_x = nudge_x,
          size = size_geom_text
        )
      }
    } +
    {
      if (!traslape) {
        geom_text(
          aes(label = scales::percent(x = mean, accuracy = 1.0)),
          nudge_x = nudge_x,
          size = size_geom_text
        )
      }
    } +
    coord_flip() +
    labs(
      color = "",
      caption = stringr::str_wrap(string = caption, width = wrap_caption)
    ) +
    scale_x_discrete(labels = function(x) {
      stringr::str_wrap(string = x, width = wrap_y)
    }) +
    scale_y_continuous(labels = scales::percent, limits = limits) +
    scale_color_manual(values = colores_variables_secundarias)
  return(g)
}

# Tabla de diferencia de opinion
tabla_cambio_opinon <- function(bd) {
  bd |>
    filter(desglose == "Efectivo" & conoce_lorenia == "Sí") |>
    select(id, opinion_lorenia, cambio_opinion) |>
    mutate(across(.cols = -c(id), .f = ~ gsub(" \\(no leer\\)", "", .x))) |>
    mutate(
      opinion_lorenia = ifelse(is.na(opinion_lorenia), "No lo conocía", opinion_lorenia)
    ) |>
    add_count(opinion_lorenia, name = "opinion_prev") |>
    add_count(cambio_opinion, name = "opinion_desp") |>
    distinct(opinion_lorenia, cambio_opinion, opinion_prev, opinion_desp) |>
    #tidyr::complete(cambio_opinion = unique(opinion_lorenia), fill = list(opinion_desp = 0,opinion_prev = 0))
    filter(opinion_lorenia == cambio_opinion) %>%
    # {
    #   total_faltante <- sum(.$opinion_desp, na.rm = TRUE) - sum(.$opinion_prev, na.rm = TRUE)
    #
    #   add_row(.,
    #           opinion_lorenia  = "No lo conocía",
    #           cambio_opinion = "No lo conocía",
    #           opinion_prev = ifelse(total_faltante > 0, total_faltante, 0),
    #           opinion_desp = 0
    #   )
    # } |>
    #pull(opinion_desp) |> sum()
    mutate(
      opinion_prev_media = opinion_prev / sum(opinion_prev),
      opinion_desp_media = opinion_desp / sum(opinion_desp)
    ) |>
    tidyr::pivot_longer(
      cols = c(opinion_lorenia, cambio_opinion),
      names_to = "momento",
      values_to = "respuesta"
    ) |>
    tidyr::pivot_longer(
      cols = c(opinion_prev, opinion_desp),
      names_to = "momento_valor",
      values_to = "valor"
    ) |>
    tidyr::pivot_longer(
      cols = c(opinion_prev_media, opinion_desp_media),
      names_to = "momento_media",
      values_to = "media"
    ) |>
    filter(
      (momento == "opinion_lorenia" &
        momento_valor == "opinion_prev" &
        momento_media == "opinion_prev_media") |
        (momento == "cambio_opinion" &
          momento_valor == "opinion_desp" &
          momento_media == "opinion_desp_media")
    ) |>
    select(respuesta, valor, media, momento) |>
    mutate(
      momento = case_match(
        momento,
        "opinion_lorenia" ~ "Previo al diálogo",
        "cambio_opinion" ~ "Posterior al diálogo"
      )
    )
}


#Graficar relacion entre afiliacion y dialogo efectivo

graficar_dialogos_afiliacion <- function(
  bd,
  corte,
  fechas_wr = NULL
) {
  if (!is.null(fechas_wr) && length(fechas_wr) > 0) {
    fechas_wr <- c(fechas_wr, as.character(corte)) |> sort()
    fechas_wr <- as.Date(unique(fechas_wr))
  }

  bd_aux <- bd |>
    summarise(dialogos = sum(desglose == "Efectivo", na.rm = TRUE), .by = fecha) |>
    arrange(fecha) |>
    mutate(
      across(where(is.numeric), ~ tidyr::replace_na(.x, 0)),
      dialogos = cumsum(dialogos)
    ) |>
    tidyr::pivot_longer(cols = -fecha, names_to = "grupo") |>
    mutate(
      grupo = ifelse(
        grupo == "afiliados",
        "Número de Afiliados",
        "Número de Diálogos"
      )
    )

  efectivos <- bd |>
    filter(desglose == "Efectivo") |>
    nrow()

  plot <- bd_aux |>
    ggplot(aes(x = fecha, y = value, color = grupo)) +
    geom_line() +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_date(
      breaks = function(x) {
        x <- as.Date(x)
        xmin <- min(x, na.rm = TRUE)
        xmax <- min(as.Date(corte), max(x, na.rm = TRUE))
        brks <- seq(
          from = lubridate::floor_date(xmin, "week", week_start = 6),
          to   = lubridate::ceiling_date(xmax, "week", week_start = 6),
          by   = "1 week"
        )
        sort(unique(c(brks[brks >= xmin & brks <= xmax], xmax)))
      },
      date_labels = "%d %b"
    ) +
    geom_vline(
      xintercept = as.Date("2025-09-01"),
      linetype = "dashed",
      color = "gray40"
    ) +
    {
      ggrepel::geom_text_repel(
        aes(
          label = ifelse(
            fecha == max(fecha, na.rm = TRUE),
            scales::comma(value),
            NA_character_
          )
        ),
        color = "gray33",
        show.legend = FALSE,
        nudge_y = max(bd_aux$value, na.rm = TRUE) * 0.03,
        direction = "y",
        segment.color = NA,
        min.segment.length = 0,
        max.overlaps = Inf,
        box.padding = 0.15,
        point.padding = 0.1,
        seed = 123
      )
    } +
    scale_color_manual(
      values = c(
        "Número de Diálogos" = color,
        "Número de Afiliados" = "#ad8a1f"
      )
    ) +
    tema_m() +
    theme(
      legend.title = element_blank()
    )

  return(plot)
}



##################################################################

# Nuevo reporte juarez

##############################################################3

# Afiliaciones acumuladas
graficar_afiliacion_acum <- function(
  bd,
  corte,
  caption_text = "",
  fechas_wr = NULL
) {
  if (!is.null(fechas_wr) && length(fechas_wr) > 0) {
    fechas_wr <- c(fechas_wr, as.character(corte)) |> sort()
    fechas_wr <- as.Date(unique(fechas_wr))
  }


  bd_aux <- bd |>
    summarise(afiliados = sum(as.numeric(afiliados), na.rm = T), .by = fecha) |>
    arrange(fecha) |>
    mutate(afiliados = cumsum(afiliados)) |>
    tidyr::pivot_longer(cols = -fecha, names_to = "grupo") |>
    mutate(
      grupo = ifelse(
        grupo == "afiliados",
        "Número de Afiliados",
        "Número de Diálogos"
      )
    )

  plot <- bd_aux |>
    ggplot(aes(x = fecha, y = value, color = grupo)) +
    geom_line() +
    geom_point(size = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    scale_x_date(
      breaks = function(x) seq(from = min(x), to = max(x), by = "14 days"),
      date_labels = "%d %b"
    ) +
    {
      if (!is.null(fechas_wr) && length(fechas_wr) > 0) {
        ggrepel::geom_text_repel(
          aes(
            label = ifelse(
              fecha %in% fechas_wr | fecha == max(fecha),
              scales::comma(value),
              NA
            )
          ),
          color = "gray33",
          show.legend = FALSE,
          vjust = -1,
          max.overlaps = 100 #,box.padding = 0.3,point.padding = 0.2
        )
      } else {
        ggrepel::geom_text_repel(
          aes(label = scales::comma(value)),
          color = "gray33",
          show.legend = FALSE,
          max.overlaps = 100 #,box.padding = 0.3,point.padding = 0.2
        )
      }
    } +
    scale_color_manual(values = c("Número de Afiliados" = "#ad8a1f")) +
    labs(y = "Afiliados", caption = caption_text) +
    tema_m() +
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      axis.title.y = element_text(family = "Poppins")
    )

  return(plot)
}


#Afiliciones por dia grafica
graficar_afiliados_dia <- function(
  afiliados_dia,
  corte,
  color_linea_acum = "#7d1f32",
  caption_text = "",
  fechas_wr = NULL
) {
  if (!is.null(fechas_wr) && length(fechas_wr) > 0) {
    fechas_wr <- as.Date(fechas_wr)
  }

  afiliados_dia |>
    ggplot(aes(x = fecha, y = afiliados)) +

    geom_line(linewidth = 2, alpha = 0.8, color = "gray66") +
    geom_point(size = 3, color = color_linea_acum) +
    scale_y_continuous(
      labels = scales::comma,
      limits = c(0, max(afiliados_dia$afiliados) + 10)
    ) +
    #scale_x_date(date_breaks = "day", date_labels = "%d %b") +
    scale_x_date(
      breaks = function(x) {
        brks_num <- scales::pretty_breaks(n = 16)(x)
        brks_num <- brks_num[brks_num <= as.Date(corte) & brks_num >= min(x)]
      },
      date_labels = "%d %b"
    ) +
    {
      if (!is.null(fechas_wr) && length(fechas_wr) > 0) {
        geom_vline(
          xintercept = fechas_wr,
          linetype = "dashed",
          color = "gray40",
          linewidth = .7
        )
      }
    } +
    geom_text(
      aes(label = scales::comma(afiliados)),
      # nudge_x = 0.25,
      nudge_y = 0.25,
      vjust = -1,
      check_overlap = TRUE,
      color = "gray10",
      size = 4
    ) +
    theme_minimal(base_size = 16) +
    labs(x = "", y = "Afiliados", caption = caption_text)
}

#Cálculo de tabla desglose de contactos y registros.
calcular_desglose_juarez <- function(bd, df_contactos) {
  tibble::tibble(
    desglose = c(
      "Total de registros",
      "Total de contactos recabados",
      "Números de contactos recabados",
      # "Personas que aceptaron unirse al grupo de WhatsApp",
      "Correos recabados"
    ),
    n = c(
      nrow(bd),
      nrow(df_contactos),
      sum(df_contactos$celular != "-"),
      # sum(df_contactos$grupo_whats == "Sí"),
      sum(df_contactos$correo != "-")
    )
  )
}

calcular_dialogos_efectivos <- function(bd) {
  bd_efec <- bd |>
    filter(desglose == "Efectivo")

  tibble::tibble(
    desglose = "Diálogos efectivos",
    n = nrow(bd_efec)
  )
}

calcular_afiliados <- function(
  bd,
  afiliados,
  promedio_afil_voc_diario = NA,
  promedio_efec_voc_diario = NA,
  dias_festivos = NULL
) {
  total_afiliados <- sum(afiliados$afiliados, na.rm = TRUE)
  dias_trabajados <- calcular_dias_trabajados(afiliados, dias_festivos)

  valores_numericos <- c(
    total_afiliados,
    sum(afiliados$afiliados[afiliados$guia == "dialogo"], na.rm = TRUE),
    sum(afiliados$afiliados[afiliados$guia == "centro"], na.rm = TRUE),
    round(total_afiliados / dias_trabajados, 0),
    sum(bd$desglose == "Efectivo", na.rm = TRUE),
    promedio_afil_voc_diario,
    promedio_efec_voc_diario
  )

  # Formatea todos menos el último con `scales::comma()` sin decimales
  valores_formateados <- c(
    scales::comma(valores_numericos[1:6], accuracy = 1),
    format(round(valores_numericos[7], 1), nsmall = 1)
  )

  tab_sal_afil <- tibble::tibble(
    desglose = c(
      "Total de afiliados a Morena",
      "Afiliados en brigadas",
      "Total de afiliados en centros de afiliación",
      "Promedio de afiliaciones diarias",
      "Diálogos efectivos",
      "Promedio afiliaciones diarias por vocero",
      "Promedio de efectivos diarios por vocero"
    ),
    n = valores_formateados
  )

  return(tab_sal_afil)
}

# Calcular de de zonas por afiliados
calcular_tabla_zona_afiliados <- function(
  bd,
  afiliados_bd,
  contacto_bd,
  afiliados_metas_bd
) {
  afiliados <- afiliados_bd |>
    summarise(Afiliados = sum(afiliados, na.rm = T), .by = zona)

  contactos <- contacto_bd |>
    #filter(desglose == "Efectivo") |>
    summarise(
      contactos = sum(correo != "-" | celular != "-", na.rm = T),
      .by = zona
    ) |>
    rename("Contactos recabados" = "contactos")

  tabla_zonas <- semaforo |>
    arrange(desc(dialogos_efectivos)) |>
    select(-c(meta, meta_diaria, meta_corte, semaforo, color)) |>
    full_join(afiliados, by = "zona") |>
    left_join(contactos, by = "zona") |>
    left_join(
      afiliados_metas_bd |>
        select(zona, meta_afil),
      by = "zona"
    ) |>
    tidyr::replace_na(list(
      Afiliados = 0,
      contactos = 0
    )) |>
    #mutate(zona = stringr::str_wrap(gsub("08_", "Distrito", zona), width = 15)) |>
    mutate(
      avance_meta_afil = Afiliados / meta_afil,
      avance_meta_afil = scales::percent(avance_meta_afil, accuracy = 1.0)
    ) |>
    select(
      "Distrito" = zona,
      Afiliados,
      "Meta total de afiliación" = meta_afil,
      "% de avance con respecto a la meta de afiliación" = avance_meta_afil,
      `Contactos recabados`,
      "Diálogos" = dialogos_efectivos,
    ) |>
    #select(-c(contains("meta"))) |>
    mutate(across(where(is.numeric), scales::comma))

  return(tabla_zonas)
}


procesamiento_afiliados_historico <- function(bd, corte) {
  bd |>
    count(fecha) |>
    tidyr::complete(
      fecha = seq.Date(min(fecha, na.rm = T), as.Date(corte), by = "day"),
      fill = list(n = 0)
    ) |>
    arrange(fecha) |>
    mutate(prom = mean(n), label = scales::comma(n), acumulado = cumsum(n))
}

graficar_afiliados_historico <- function(
  bd,
  color_linea_acum,
  color_meta_me,
  color_zona = "#e9c46a",
  acumulado,
  interactivo = T,
  total_usuario,
  meta_diaria,
  corte = Sys.Date() - 1
) {
  if (acumulado) {
    if (interactivo) {
      aux <- tibble(fecha = sort(unique(bd$fecha))) |>
        mutate(dias = row_number(), meta = dias * meta_diaria * total_usuario)

      hchart(bd, "line", hcaes(x = fecha, y = acumulado)) %>%
        hc_add_series(
          bd,
          type = "scatter",
          hcaes(x = fecha, y = acumulado),
          marker = list(radius = 4, fillColor = color_linea_acum),
          name = "Registros",
          tooltip = list(pointFormat = '{point.y} diálogos')
        ) %>%
        hc_yAxis(
          title = list(text = "Diálogos"),
          labels = list(
            formatter = JS(
              "function() { return Highcharts.numberFormat(this.value, 0, ',', ','); }"
            )
          ),
          min = 0,
          max = max(bd$acumulado) + 25
        ) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_title(text = "Afiliados Históricos Acumulados") %>%
        hc_colors(colors = c(color_linea_acum)) %>%
        hc_plotOptions(line = list(lineWidth = 2)) |>
        hc_add_series(aux, type = "line", hcaes(x = fecha, y = meta)) |>
        hc_add_series(
          aux,
          type = "scatter",
          hcaes(x = fecha, y = meta),
          tooltip = list(pointFormat = '{point.y} afiliados'),
          marker = list(radius = 4, fillColor = color_meta_me),
          name = "Meta mejor escenario"
        ) %>%
        hc_colors(colors = c("#9a8c98")) %>%
        hc_plotOptions(line = list(lineWidth = 2))
    } else {
      bd |>
        ggplot(aes(x = fecha, y = acumulado)) +
        geom_line(linewidth = 2, alpha = 0.8, color = color_linea_acum) +
        geom_point(size = 3, color = color_linea_acum) +
        scale_y_continuous(labels = scales::comma) +
        geom_text(
          aes(label = label),
          nudge_x = 0.25,
          nudge_y = 0.25,
          vjust = -1,
          check_overlap = TRUE,
          color = "gray10",
          size = 4
        ) +
        theme_minimal() +
        labs(x = "", y = "Afiliados") +
        ylim(c(0, max(bd$acumulado) + 25))
    }
  } else {
    fecha_min <- min(bd$fecha)
    fecha_max <- max(bd$fecha)
    prom <- unique(bd$prom)
    if (interactivo) {
      hchart(bd, "line", hcaes(x = fecha, y = n)) %>%
        hc_add_series(
          bd,
          type = "scatter",
          hcaes(x = fecha, y = n),
          marker = list(radius = 4, fillColor = color_linea_acum),
          name = "Afiliados"
        ) %>%
        hc_tooltip(pointFormat = '<b>{point.y}</b>') %>%
        hc_yAxis(
          title = list(text = "Afiliados"),
          labels = list(
            formatter = JS(
              "function() { return Highcharts.numberFormat(this.value, 0, ',', ','); }"
            )
          ),
          min = 0,
          max = max(bd$n) + 25,
          plotBands = list(
            list(from = 0, to = prom, color = color_zona, opacity = 0.5)
          )
        ) %>%
        hc_xAxis(title = list(text = "")) %>%
        hc_title(text = "Afiliados Históricos") %>%
        hc_colors(colors = c(color_linea_acum)) %>%
        hc_plotOptions(line = list(lineWidth = 2))
    } else {
      ggplot(bd, aes(x = fecha, y = n)) +
        # annotate("rect",
        #          fill = color_zona, alpha = 0.5,
        #          xmin = as.Date(fecha_min), xmax = as.Date(fecha_max),
        #          ymin = 0, ymax = prom) +
        geom_line(linewidth = 2, alpha = 0.8, color = "gray66") +
        geom_point(size = 3, color = color_linea_acum) +
        scale_y_continuous(
          labels = scales::comma,
          limits = c(0, max(bd$n) + 25)
        ) +
        #scale_x_date(date_breaks = "day", date_labels = "%d %b") +
        scale_x_date(
          #limits = c(min(brigadistas_dia$fecha),max(brigadistas_dia$fecha)),
          # Usamos una función anónima para calcular breaks “bonitos” (pretty)
          # y agregar además la fecha máxima (último día).
          breaks = function(x) {
            brks_num <- scales::pretty_breaks(n = 16)(x)
            brks_num <- brks_num[
              brks_num <= as.Date(corte) & brks_num >= min(x)
            ]
          },
          date_labels = "%d %b"
        ) +
        geom_text(
          aes(label = label),
          # nudge_x = 0.25,
          nudge_y = 0.25,
          vjust = -1,
          check_overlap = TRUE,
          color = "gray10",
          size = 3
        ) +
        theme_minimal(base_size = 16) +
        labs(x = "", y = "Afiliados")
    }
  }
}

area_general <-  function(base, base_parametros, jovenes,adultos,mayores, color_texto){
    total <- rowSums(base_parametros)

    par_jovenes <- filter(base_parametros) |>
      pull(total_18_29)/total
    par_adultos <- filter(base_parametros) |>
      pull(total_30_59)/total
    par_mayores <- filter(base_parametros) |>
      pull(total_60_y_mas)/total


    base |>
      filter(desglose=="Efectivo") |>
      mutate(edad=as.double(edad),
             rango_edad=case_when(edad>=18 & edad<=29 ~ "Jóvenes",
                                  edad>=30 & edad<=59 ~ "Adultos",
                                  edad>=60 ~ "Adultos mayores"),
             rango_edad=factor(rango_edad, levels=c("Adultos mayores","Adultos","Jóvenes"))) |>
      count(fecha, rango_edad) |>
      tidyr::complete(fecha, rango_edad, fill = list(n = 0)) |>
      filter(!is.na(rango_edad)) |> 
      mutate(pct = n/sum(n), .by = fecha) |>
      ggplot(aes(x = fecha, y = pct, fill = rango_edad)) +
      geom_area(alpha=0.5) +
      scale_fill_manual(values=c("Jóvenes" = jovenes,
                                 "Adultos" = adultos,
                                 "Adultos mayores" = mayores)) +
      geomtextpath::geom_textline(aes(y =par_jovenes,x=fecha,
                                      label="Jóvenes"),
                                  color = color_texto, linetype="dashed",
                                  linewidth=1, size=5) +
      geomtextpath::geom_textline(aes(y = par_adultos+par_jovenes,x=fecha,
                                      label="Adultos"),
                                  color = color_texto, linetype="dashed",
                                  linewidth=1, size=5) +
      geomtextpath::geom_textline(aes(y = 1,x=fecha,
                                      label="Adultos mayores*"),
                                  color = color_texto, linetype="dashed",
                                  linewidth=1, size=5) +
      #scale_x_date(date_breaks = "1 day", labels = scales::date_format("%b %d"))
      scale_x_date(labels = scales::date_format("%b %d"))+
      scale_y_continuous(breaks = seq(0,1,0.1), labels = scales::percent)
}


mapa_meta_binaria <- function(
  bd,
  nombre_mapa = NULL, 
  var_meta = "meta",
  color_activo = "#0a5fb3",
  color_inactivo = "gray85"
) {
  shp_proc <- bd |> 
    dplyr::mutate(
      tiene_meta = dplyr::if_else(
        !is.na(.data[[var_meta]]),
        "con_meta",
        "sin_meta"
      )
    ) |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(
      ggplot2::aes(fill = tiene_meta),
      color = NA
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        con_meta = color_activo,
        sin_meta = color_inactivo
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::guides(fill = "none")
  return(shp_proc)
}




zoom_h <- function(
  shp,
  xmin = -111.1,
  ymin = 28.95,
  xmax = -110.9,
  ymax = 29.2
) {

  bbox <- sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmin, ymax),
    c(xmax, ymax),
    c(xmax, ymin),
    c(xmin, ymin)
  )))

  area_interes <- sf::st_sfc(
    bbox,
    crs = sf::st_crs(shp)
  )

  shp |>
    sf::st_intersection(area_interes)
}
