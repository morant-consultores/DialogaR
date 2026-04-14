#' Calcular meta por sección y usuario con agrupación condicional
#'
#' Agrega la actividad de campo por sección geográfica y la cruza con las metas
#' asignadas. Si se proporciona \code{base_coordinadores}, la agrupación incluye
#' también el coordinador y la brigada responsable de cada sección.
#'
#' @param bd_actividad Data frame con el registro de actividad de campo. Debe
#'   contener al menos las columnas \code{seccion}, \code{fecha},
#'   \code{desglose} y, cuando se use \code{base_coordinadores},
#'   \code{usuario_num}.
#' @param metas Data frame con las metas por sección. Debe contener las columnas
#'   \code{seccion} y \code{meta} (numérica).
#' @param corte Fecha de corte del reporte (objeto \code{Date} o cadena
#'   interpretable por R como fecha).
#' @param base_coordinadores Data frame opcional con la asignación de voceros a
#'   coordinadores y brigadas. Debe contener las columnas \code{vocero},
#'   \code{nombre_coordinador} y \code{nombre_brigada}. Si es \code{NULL}
#'   (predeterminado) la agrupación se realiza solo por sección.
#'
#' @return Data frame con una fila por combinación de agrupación, columnas en
#'   mayúsculas con espacios, que incluye viviendas visitadas, días trabajados,
#'   diálogos efectivos, meta, fecha de corte y avance de meta (proporción
#'   numérica; \code{0} cuando la sección no tiene meta asignada o la meta es cero).
#'
#' @export
#'
#' @section Seguridad y Privacidad:
#'   Esta función procesa datos de actividad de campo que pueden contener
#'   identificadores de usuarios (\code{usuario_num}) y secciones electorales.
#'   No persiste datos en disco ni los transmite a servicios externos. Asegúrese
#'   de que los data frames de entrada provengan de fuentes autorizadas y de que
#'   los resultados se compartan únicamente con destinatarios con acceso
#'   aprobado.
meta_usuario_condicional <- function(
  bd_actividad,
  metas,
  corte,
  base_coordinadores = NULL
) {
  # -----------------------------
  # Normalización base
  # -----------------------------
  bd <- bd_actividad |>
    dplyr::filter(!is.na(seccion), seccion != "") |>
    dplyr::mutate(
      seccion = stringr::str_pad(
        as.character(seccion),
        4,
        side = "left",
        pad = "0"
      )
    )

  # -----------------------------
  # JOIN CON COORDINADORES (SI EXISTE)
  # -----------------------------
  if (!is.null(base_coordinadores)) {
    bd <- bd |>
      dplyr::left_join(base_coordinadores, by = c("usuario_num" = "vocero"))

    group_vars <- c("seccion", "nombre_coordinador", "nombre_brigada")
  } else {
    group_vars <- c("seccion")
  }

  # -----------------------------
  # Agregación principal
  # -----------------------------
  out <- bd |>
    dplyr::summarise(
      viviendas_visitadas = dplyr::n(),
      dias_trabajados = dplyr::n_distinct(fecha),
      diálogos_efectivos = sum(desglose == "Efectivo", na.rm = TRUE),
      .by = dplyr::all_of(group_vars)
    ) |>
    dplyr::left_join(
      metas |>
        dplyr::transmute(
          seccion = stringr::str_pad(
            as.character(seccion),
            4,
            side = "left",
            pad = "0"
          ),
          meta = round(meta)
        ),
      by = "seccion"
    ) |>
    dplyr::mutate(
      fecha_corte = corte,
      avance_meta = dplyr::if_else(
        is.na(meta) | meta == 0,
        0,
        diálogos_efectivos / meta
      )
    ) |>
    dplyr::relocate(fecha_corte, .after = seccion) |>
    dplyr::rename_with(~ toupper(gsub("_", " ", .x)))

  return(out)
}
