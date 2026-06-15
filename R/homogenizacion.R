normalizador_actividad <- function(df, f) {
  # Municipios fijos para CHIH y JUAREZ
  if (identical(f$zona, "chih")) {
    df <- df |> mutate(municipio = "CHIHUAHUA")
  }

  if (identical(f$zona, "juarez")) {
    df <- df |> mutate(municipio = "JUAREZ")
  }

  # Sur: recodificación CASAS GRANDES -> NUEVO CASAS GRANDES desde 2025-09-01
  if (identical(f$zona, "sur")) {
    df <- df |>
      mutate(
        municipio = if_else(
          municipio == "CASAS GRANDES" & fecha >= as.Date("2025-09-01"),
          "NUEVO CASAS GRANDES",
          municipio
        )
      )
  }

  # Etapa 2: isComplete como character (chih_e2 y sur_e2)
  if (isTRUE(f$etapa == 2) && "isComplete" %in% names(df)) {
    df <- df |> mutate(isComplete = as.character(isComplete))
  }

  df
}

postprocess_insumos <- function(insumos, municipios) {
  bd <- insumos$bd_actividad

  # 1) Filtro Sur: (municipio %in% municipios | municipio == "")
  #    Esto aplica sólo a sur_e1 y sur_e2 (tu lógica original)
  bd_sur <- bd |>
    filter(origen_datos %in% c("sur_e1", "sur_e2")) |>
    filter((municipio %in% municipios) | municipio == "")

  bd_no_sur <- bd |>
    filter(!origen_datos %in% c("sur_e1", "sur_e2"))

  # 2) Quitar Ascensión de sur y pasarlo a juarez
  sur_asc <- bd_sur |> filter(municipio == "ASCENSION")
  bd_sur <- bd_sur |> filter(municipio != "ASCENSION")

  # 3) Juárez incluye Ascensión
  bd_juarez <- bd_no_sur |>
    filter(origen_datos == "juarez_e1") |>
    bind_rows(sur_asc)

  bd_otro <- bd_no_sur |> filter(origen_datos != "juarez_e1")

  # 4) Filtro Juárez: quitar NAs en puerta sólo si fecha < 2025-04-01
  #    (ojo: Juárez original no filtra puerta NA, excepto este caso post-bind)
  if ("puerta" %in% names(bd_juarez)) {
    bd_juarez <- bd_juarez |>
      filter(!(is.na(puerta) & fecha < as.Date("2025-04-01")))
  }

  # 5) Re-ensamblar
  insumos$bd_actividad <- bind_rows(bd_otro, bd_juarez, bd_sur)
  insumos
}
