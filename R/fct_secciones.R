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
      # ISO Business Rule: NA if no meta exists, do not fake 100% compliance.
      # We also keep 'avance_meta' as a pure numeric value (no scales::percent).
      avance_meta = dplyr::if_else(
        is.na(meta) | meta == 0,
        NA_real_,
        diálogos_efectivos / meta
      )
    ) |>
    dplyr::relocate(fecha_corte, .after = seccion) |>
    dplyr::rename_with(~ toupper(gsub("_", " ", .x)))

  return(out)
}
