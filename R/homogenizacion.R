cargar_actividad <- function(
  pool,
  fuentes,
  fecha_min = NULL,
  fecha_max = NULL,
  filtro_minimo = NULL,
  normalizador = NULL
) {
  purrr::map_dfr(fuentes, function(f) {
    tabla_ref <- if (!is.null(f$schema)) DBI::Id(schema = f$schema, table = f$tabla) else f$tabla
    q <- tbl(pool, tabla_ref)

    # 1) Filtro mínimo por fuente (SQL)
    if (!is.null(filtro_minimo)) {
      q <- filtro_minimo(q, f)
    }

    # 2) Fechas: prioriza por-fuente y si no, usa global
    f_fecha_min <- f$fecha_min %||% fecha_min
    f_fecha_max <- f$fecha_max %||% fecha_max

    if (!is.null(f_fecha_min)) {
      q <- q |> filter(fecha >= !!as.Date(f_fecha_min))
    }
    if (!is.null(f_fecha_max)) {
      q <- q |> filter(fecha <= !!as.Date(f_fecha_max))
    }

    # 3) Proyección opcional
    if (!is.null(f$select_cols)) {
      q <- q |> select(any_of(f$select_cols))
    }

    df <- q |>
      collect() |>
      mutate(
        fecha = as.Date(fecha),
        usuario_num = as.character(usuario_num),
        origen_datos = dplyr::coalesce(f$origen, f$tabla)
      )

    # Coerción de columna geográfica: solo si existe.
    # Proyectos con sección (e.g. Lorenia) tienen `seccion`;
    # proyectos con AGEB (e.g. GM) no la tienen y no deben fallar aquí.
    if ("seccion" %in% names(df)) {
      df <- df |> mutate(seccion = as.character(seccion))
    }

    # 4) Normalizador por fuente (R)
    if (!is.null(normalizador)) {
      df <- normalizador(df, f)
    }

    df
  })
}

# NOTA: filtro_minimo_actividad fue eliminado de este módulo.
# Cada proyecto debe inyectar su propio filtro vía el parámetro
# `filtro_minimo_actividad` de cargar_insumos(). No existe columna
# geográfica universal: Lorenia usa `seccion`, GM usa `ageb`.

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
