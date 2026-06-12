#' Calcular meta por sección con brigadas y fechas colapsadas
#'
#' Agrega la actividad de campo por sección geográfica (una fila por sección)
#' y la cruza con las metas asignadas. Las fechas de visita se colapsan en una
#' sola celda con \code{toString}. Si se proporciona \code{base_coordinadores},
#' las brigadas que visitaron cada sección también se colapsan en una celda.
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
#'   coordinadores y brigadas. Debe contener las columnas \code{vocero} y
#'   \code{nombre_brigada}. La columna \code{nombre_coordinador}, si está
#'   presente, es ignorada en esta versión. Si se provee, se añade la columna
#'   \code{BRIGADAS} con los nombres colapsados. Si es \code{NULL}
#'   (predeterminado) esa columna no aparece.
#'
#' @return Data frame con \strong{una fila por sección}, columnas en mayúsculas
#'   con espacios, que incluye viviendas visitadas, días trabajados, diálogos
#'   efectivos, fechas de visita (colapsadas con \code{toString}), meta, fecha
#'   de corte y avance de meta (proporción numérica; \code{0} cuando la sección
#'   no tiene meta asignada o la meta es cero). Cuando se provee
#'   \code{base_coordinadores} se añade la columna \code{BRIGADAS} con los
#'   nombres de brigada colapsados.
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
  }

  # -----------------------------
  # Agregación principal — siempre una fila por sección
  # -----------------------------
  if (!is.null(base_coordinadores)) {
    out <- bd |>
      dplyr::summarise(
        viviendas_visitadas  = dplyr::n(),
        dias_trabajados      = dplyr::n_distinct(fecha),
        diálogos_efectivos   = sum(desglose == "Efectivo", na.rm = TRUE),
        fechas_visita        = toString(sort(unique(fecha))),
        brigadas             = toString(sort(unique(na.omit(nombre_brigada)))),
        .by = "seccion"
      )
  } else {
    out <- bd |>
      dplyr::summarise(
        viviendas_visitadas  = dplyr::n(),
        dias_trabajados      = dplyr::n_distinct(fecha),
        diálogos_efectivos   = sum(desglose == "Efectivo", na.rm = TRUE),
        fechas_visita        = toString(sort(unique(fecha))),
        .by = "seccion"
      )
  }

  out <- out |>
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
