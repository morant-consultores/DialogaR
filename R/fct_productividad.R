# ============================================================
# REPORTE: PRODUCTIVIDAD (CORE)
#
# Este módulo refactoriza únicamente EL REPORTE (cálculo, ensamble y excel).
# Asume que los INSUMOS ya están cargados en memoria:
#   - bd_aux: dimensión operativa (foto al corte o a la ventana)
#   - actividad_dia: hechos de actividad ya filtrados a la ventana (día/semana)
#   - pl_main: pase de lista principal ya parseado (data.frame/tibble)
#   - pl_extra: pase de lista adicional (opcional; EXCEPCIÓN)
#
# Principios:
#   - Excepciones explícitas: dos cuestionarios / múltiples pases por día
#   - Conversión numérica sin hardcode: heurística conservadora
#   - Coordinadores como filas: configurable (scope + grain)
#   - Manejo de faltantes: política strict/lenient
#   - Ordenamiento robusto: no depende de distrito
# ============================================================

# ---- 0) Helpers ------------------------------------------------------------

normalizar_faltantes_reporte <- function(
  df,
  char_default = "-",
  num_default = 0
) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric),
        ~ tidyr::replace_na(.x, num_default)
      ),
      dplyr::across(
        dplyr::where(is.character),
        ~ tidyr::replace_na(.x, char_default)
      )
    )
}

ordenar_reporte <- function(df) {
  has_distrito <- "distrito" %in% names(df)
  if (has_distrito) {
    df |>
      dplyr::arrange(
        suppressWarnings(as.numeric(distrito)),
        municipio,
        nombre_brigada,
        nombre_vocero
      )
  } else {
    df |>
      dplyr::arrange(
        municipio,
        nombre_brigada,
        nombre_vocero
      )
  }
}

# Helper: coerción numérica heurística (sin hardcodear columnas).
# Regla: convertir columnas character que "parezcan numéricas" en >= threshold de sus valores.
# Se excluyen llaves y columnas cualitativas típicas.
coerce_numeric_candidates <- function(
  df,
  exclude = c(
    "id",
    "fecha",
    "obtener_usuario",
    "usuario",
    "vocero",
    "supervisor",
    "asistencia",
    "finalizar",
    "observaciones",
    "motivo_no",
    "motivo_noasistio",
    "motivo_nopudo",
    "othertext_motivo_noasistio",
    "othertext_motivo_nopudo"
  ),
  threshold = 0.95
) {
  df |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::where(is.character) & !dplyr::any_of(exclude),
      .fns = ~ {
        x <- stringr::str_squish(.x)
        if (all(is.na(x))) {
          return(.x)
        }

        # Normalización mínima: quitar comas de miles, si existieran
        x2 <- stringr::str_replace_all(x, ",", "")
        ok <- is.na(x2) |
          x2 == "" |
          stringr::str_detect(x2, "^[+-]?[0-9]+(\\.[0-9]+)?$")

        if (mean(ok) >= threshold) {
          suppressWarnings(as.numeric(dplyr::na_if(x2, "")))
        } else {
          .x
        }
      }
    ))
}

# Helper: suma robusta de campos tipo "1,2,3" (con NA) en columnas candidatas
sum_csv_numeric <- function(...) {
  vals <- c(...)
  txt <- stringr::str_remove_all(as.character(vals), "\\s+")
  if (length(txt) == 0 || all(is.na(txt))) {
    return(NA_real_)
  }
  nums <- suppressWarnings(as.numeric(unlist(stringr::str_split(txt, ","))))
  sum(nums, na.rm = TRUE)
}


# ---- 1) Pase de lista (transformación para el reporte) ---------------------

preparar_pase_lista <- function(
  pl_main,
  corte,
  pl_extra = NULL,
  allow_multiple_sources = FALSE,
  allow_multiple_per_day = TRUE,
  missing_policy = c("lenient", "strict"),
  numeric_threshold = 0.95
) {
  missing_policy <- match.arg(missing_policy)

  # EXCEPCIÓN OPERATIVA (explícita):
  # En proyectos estándar existe un solo pase (un cuestionario).
  # Si se provee pl_extra, debe habilitarse allow_multiple_sources.
  if (!is.null(pl_extra) && !isTRUE(allow_multiple_sources)) {
    stop(
      "pl_extra fue provisto pero allow_multiple_sources = FALSE. Hacer explícita la excepción."
    )
  }

  pl <- dplyr::bind_rows(
    pl_main |>
      dplyr::mutate(vocero = as.character(usuario), fecha = as.Date(fecha)),
    if (!is.null(pl_extra)) {
      pl_extra |>
        dplyr::mutate(vocero = as.character(usuario), fecha = as.Date(fecha))
    } else {
      NULL
    }
  ) |>
    dplyr::filter(fecha == as.Date(corte)) |>
    dplyr::rename(supervisor = obtener_usuario) |>
    dplyr::mutate(
      supervisor = as.character(supervisor),
      vocero = as.character(vocero)
    )

  # Si no hay pase de lista para la fecha, decide según política
  if (nrow(pl) == 0) {
    if (missing_policy == "strict") {
      stop("No hay pase de lista para la fecha solicitada.")
    }
    # Retorno vacío pero con columnas mínimas para joins posteriores
    return(tibble::tibble(
      supervisor = character(),
      vocero = character(),
      fecha = as.Date(character()),
      numero_pases_lista = integer()
    ))
  }

  # Conteo de pases por supervisor-vocero (auditoría)
  pl <- pl |>
    dplyr::mutate(numero_pases_lista = dplyr::n(), .by = c(supervisor, vocero))

  # Regla del core: si no permitimos múltiples pases, fallar explícitamente
  if (
    !isTRUE(allow_multiple_per_day) &&
      any(pl$numero_pases_lista > 1, na.rm = TRUE)
  ) {
    stop(
      "Se detectaron múltiples pases por día para supervisor-vocero; allow_multiple_per_day = FALSE."
    )
  }

  # Consolidación para replicar reporte vigente cuando hay múltiples pases:
  # - concatenar valores para preservar trazabilidad
  if (any(pl$numero_pases_lista > 1, na.rm = TRUE)) {
    pl_2 <- pl |>
      dplyr::filter(numero_pases_lista > 1) |>
      dplyr::arrange(supervisor, vocero) |>
      dplyr::group_by(supervisor, vocero) |>
      dplyr::summarise(
        numero_pases_lista = as.character(dplyr::n()),
        dplyr::across(
          .cols = -numero_pases_lista,
          .fns = ~ paste(
            tidyr::replace_na(as.character(.x), "-"),
            collapse = ", "
          )
        ),
        .groups = "drop"
      )

    pl <- pl |>
      dplyr::filter(numero_pases_lista <= 1) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::bind_rows(pl_2)
  }

  # Conversión numérica no-hardcodeada (heurística conservadora)
  pl <- pl |> coerce_numeric_candidates()

  # Derivado: afiliados_total (si existen columnas compatibles)
  # En algunos instrumentos podría llamarse afiliados / numafiliacion / num_afiliacion
  cols_af <- intersect(
    names(pl),
    c("afiliados", "numafiliacion", "num_afiliacion")
  )
  if (length(cols_af) > 0) {
    pl <- pl |>
      dplyr::rowwise() |>
      dplyr::mutate(
        afiliados_total = sum_csv_numeric(dplyr::c_across(dplyr::all_of(
          cols_af
        )))
      ) |>
      dplyr::ungroup()
  }

  pl
}


# ---- 2) Métricas de actividad (agregación para el reporte) -----------------
resumir_actividad <- function(
  actividad,
  missing_policy = c("lenient", "strict"),
  col_usuario = "usuario_num",
  col_seccion = "seccion", # Parámetro que define el nombre de la columna geográfica
  col_desglose = "desglose",
  col_duracion = "duracion_minutos",
  col_inicio = "fecha_inicio",
  col_fin = "fecha_fin"
) {
  missing_policy <- match.arg(missing_policy)

  if (is.null(actividad) || nrow(actividad) == 0) {
    if (missing_policy == "strict") {
      stop("No hay actividad para la ventana solicitada.")
    }
    # Usamos !! (bang-bang) y := para inyectar dinámicamente el nombre de la columna
    return(tibble::tibble(
      usuario_num = character(),
      !!col_seccion := character(),
      viviendas_visitadas_nube = integer(),
      dialogos_efectivos_nube = integer(),
      no_abrieron_nube = integer(),
      rechazaron_nube = integer(),
      cancelado_nube = integer(),
      sin_informacion_nube = integer(),
      afiliados_nube = integer(),
      duracion_promedio_min = numeric(),
      total_horas_trabajadas = numeric()
    ))
  }

  actividad |>
    dplyr::group_by(.data[[col_usuario]]) |>
    dplyr::summarise(
      # Usamos "{var}" := para nombrar la columna de salida dinámicamente
      "{col_seccion}" := dplyr::case_when(
        dplyr::n_distinct(.data[[col_seccion]]) > 1 ~ paste(
          unique(.data[[col_seccion]]),
          collapse = ", "
        ),
        TRUE ~ as.character(unique(.data[[col_seccion]])[1])
      ),
      viviendas_visitadas_nube = dplyr::n(),
      dialogos_efectivos_nube = sum(
        .data[[col_desglose]] == "Efectivo",
        na.rm = TRUE
      ),
      no_abrieron_nube = sum(
        .data[[col_desglose]] == "No abrieron",
        na.rm = TRUE
      ),
      rechazaron_nube = sum(
        .data[[col_desglose]] == "Sí, rechazaron",
        na.rm = TRUE
      ),
      cancelado_nube = sum(.data[[col_desglose]] == "Cancelado", na.rm = TRUE),
      sin_informacion_nube = sum(
        .data[[col_desglose]] == "ERROR",
        na.rm = TRUE
      ),
      #afiliados_nube = sum(.data[[col_afiliacion]] == "Sí", na.rm = TRUE),
      duracion_promedio_min = mean(
        .data[[col_duracion]][.data[[col_desglose]] == "Efectivo"],
        na.rm = TRUE
      ),
      total_horas_trabajadas = as.numeric(difftime(
        max(.data[[col_fin]]),
        min(.data[[col_inicio]]),
        units = "hours"
      )),
      .groups = "drop"
    ) |>
    dplyr::rename(usuario_num = dplyr::all_of(col_usuario))
}


# ---- 3) Ensamble (voceros + coordinadores) --------------------------------

ensamblar_productividad <- function(
  bd_aux,
  registros,
  pl = NULL, # Valor por defecto explícito
  include_coordinators = TRUE,
  coordinator_scope = c("from_structure", "from_pl"),
  coordinator_grain = c("coordinator_only", "coordinator_by_brigade")
) {
  coordinator_scope <- match.arg(coordinator_scope)
  coordinator_grain <- match.arg(coordinator_grain)

  # FAIL-FAST (ISO 27000): Prevenir estados inconsistentes
  if (is.null(pl) && coordinator_scope == "from_pl") {
    stop(
      "Conflicto de parámetros: No se puede usar coordinator_scope = 'from_pl' si 'pl' es NULL."
    )
  }

  # Voceros: estructura + métricas + pase (condicional)
  bd_prod_voc <- bd_aux |>
    dplyr::full_join(registros, dplyr::join_by(vocero == usuario_num)) |>
    # Inyección condicional de PL
    (\(data) {
      if (!is.null(pl)) {
        dplyr::left_join(data, pl, dplyr::join_by(supervisor, vocero))
      } else {
        data
      }
    })() |>
    dplyr::distinct() |>
    dplyr::select(dplyr::any_of(names(bd_aux)), dplyr::everything())

  bd_prod_coord <- NULL

  if (isTRUE(include_coordinators)) {
    # Universo de coordinadores
    if (coordinator_scope == "from_structure") {
      coord_base <- bd_aux |>
        dplyr::filter(!is.na(supervisor), supervisor != "-", status_vocero == TRUE) |>
        dplyr::distinct(supervisor, .keep_all = TRUE)
    } else {
      # Si llega aquí, está garantizado que 'pl' NO es NULL gracias a la validación inicial
      coord_ids <- pl |>
        dplyr::filter(!is.na(supervisor), supervisor != "-") |>
        dplyr::distinct(supervisor)

      coord_base <- bd_aux |>
        dplyr::filter(status_vocero == TRUE) |>
        dplyr::semi_join(coord_ids, by = "supervisor") |>
        dplyr::distinct(supervisor, .keep_all = TRUE)
    }

    bd_prod_coord <- coord_base |>
      dplyr::transmute(
        distrito,
        municipio,
        nombre_brigada,
        nombre_coordinador,
        supervisor,
        status_coord,
        nombre_vocero = nombre_coordinador,
        vocero = supervisor,
        status_vocero = status_coord
      )

    # Grano: una fila por coordinador, o por coordinador-brigada
    if (coordinator_grain == "coordinator_only") {
      bd_prod_coord <- bd_prod_coord |>
        dplyr::distinct(vocero, .keep_all = TRUE)
    } else {
      bd_prod_coord <- bd_prod_coord |>
        dplyr::distinct(vocero, nombre_brigada, .keep_all = TRUE)
    }

    # Métricas del coordinador: join por su num + pase (condicional)
    bd_prod_coord <- bd_prod_coord |>
      dplyr::left_join(registros, dplyr::join_by(supervisor == usuario_num)) |>
      # Inyección condicional de PL
      (\(data) {
        if (!is.null(pl)) {
          dplyr::left_join(data, pl, dplyr::join_by(supervisor, vocero))
        } else {
          data
        }
      })() |>
      dplyr::distinct()
  }

  bd_prod <- if (is.null(bd_prod_coord)) {
    bd_prod_voc
  } else {
    bd_prod_coord |>
      dplyr::bind_rows(
        bd_prod_voc |>
          dplyr::filter(!vocero %in% unique(bd_prod_coord$vocero))
      )
  }

  bd_prod |>
    dplyr::select(dplyr::any_of(names(bd_aux)), dplyr::everything())
}


# ---- 4) Post-procesamiento del output (faltantes, banderas, orden) --------

postprocesar_output <- function(bd_prod, char_default = "-", num_default = 0) {
  out <- bd_prod |>
    normalizar_faltantes_reporte(
      char_default = char_default,
      num_default = num_default
    )

  # Banderas de auditoría (robustas a columnas faltantes)
  if ("numero_pases_lista" %in% names(out)) {
    out <- out |>
      dplyr::mutate(
        tiene_pase_lista = !is.na(numero_pases_lista) & numero_pases_lista != 0
      )
  } else {
    out <- out |>
      dplyr::mutate(tiene_pase_lista = FALSE)
  }

  if ("viviendas_visitadas_nube" %in% names(out)) {
    out <- out |>
      dplyr::mutate(tiene_actividad = viviendas_visitadas_nube > 0)
  } else {
    out <- out |>
      dplyr::mutate(tiene_actividad = FALSE)
  }

  out_ordenado <- out |>
    ordenar_reporte() |>
    dplyr::filter(vocero != "-")

  # Detectar voceros dados de baja con actividad antes de filtrarlos
  excluidos_vocero <- out_ordenado |>
    dplyr::filter(!is.na(status_vocero) & !status_vocero)

  if (nrow(excluidos_vocero) > 0) {
    tiene_actividad_col <- "viviendas_visitadas_nube" %in% names(excluidos_vocero)

    con_datos_voc <- if (tiene_actividad_col) {
      excluidos_vocero |> dplyr::filter(viviendas_visitadas_nube > 0)
    } else {
      excluidos_vocero[0, ]
    }

    if (nrow(con_datos_voc) > 0) {
      detalle_voc <- paste(
        apply(con_datos_voc, 1, function(r) {
          viviendas <- if (tiene_actividad_col) r[["viviendas_visitadas_nube"]] else "?"
          efectivos  <- if ("dialogos_efectivos_nube" %in% names(con_datos_voc)) r[["dialogos_efectivos_nube"]] else "?"
          sprintf(
            "  - Vocero %s (%s) | Viviendas: %s | Efectivos: %s",
            r[["vocero"]], r[["nombre_vocero"]], viviendas, efectivos
          )
        }),
        collapse = "\n"
      )
      cli::cli_warn(c(
        "!" = "{nrow(con_datos_voc)} vocero(s) dado(s) de baja con actividad registrada. Se incluyen en el reporte.",
        " " = "Verifica el status del vocero en la base de datos.",
        " " = detalle_voc
      ))
    }
  }

  # Nunca excluir un vocero con actividad registrada, aunque esté dado de baja.
  out_ordenado <- if ("viviendas_visitadas_nube" %in% names(out_ordenado)) {
    out_ordenado |>
      dplyr::filter(is.na(status_vocero) | status_vocero | viviendas_visitadas_nube > 0)
  } else {
    out_ordenado |>
      dplyr::filter(is.na(status_vocero) | status_vocero)
  }

  # Detectar voceros activos con coordinador inactivo antes de filtrarlos
  excluidos_coord <- out_ordenado |>
    dplyr::filter(!is.na(status_coord) & !status_coord)

  if (nrow(excluidos_coord) > 0) {
    tiene_actividad_col <- "viviendas_visitadas_nube" %in% names(excluidos_coord)

    con_datos <- if (tiene_actividad_col) {
      excluidos_coord |> dplyr::filter(viviendas_visitadas_nube > 0)
    } else {
      excluidos_coord[0, ]
    }

    sin_datos <- if (tiene_actividad_col) {
      excluidos_coord |> dplyr::filter(viviendas_visitadas_nube == 0)
    } else {
      excluidos_coord
    }

    if (nrow(con_datos) > 0) {
      detalle <- paste(
        apply(con_datos, 1, function(r) {
          viviendas <- if (tiene_actividad_col) r[["viviendas_visitadas_nube"]] else "?"
          efectivos  <- if ("dialogos_efectivos_nube" %in% names(con_datos)) r[["dialogos_efectivos_nube"]] else "?"
          sprintf(
            "  - Vocero %s (%s) | Coord. inactivo: %s | Viviendas: %s | Efectivos: %s",
            r[["vocero"]], r[["nombre_vocero"]], r[["nombre_coordinador"]],
            viviendas, efectivos
          )
        }),
        collapse = "\n"
      )
      cli::cli_warn(c(
        "!" = "{nrow(con_datos)} vocero(s) con actividad tienen coordinador inactivo (baja reciente o reasignacion pendiente). Se incluyen en el reporte.",
        " " = "Verifica el estado del coordinador en la base de datos.",
        " " = detalle
      ))
    }

    if (nrow(sin_datos) > 0) {
      detalle_sin <- paste(
        apply(sin_datos, 1, function(r) {
          sprintf(
            "  - Vocero %s (%s) | Coord. inactivo: %s",
            r[["vocero"]], r[["nombre_vocero"]], r[["nombre_coordinador"]]
          )
        }),
        collapse = "\n"
      )
      cli::cli_warn(c(
        "i" = "{nrow(sin_datos)} vocero(s) activo(s) excluido(s) del reporte por tener coordinador inactivo (sin actividad registrada).",
        " " = detalle_sin
      ))
    }
  }

  # Excluir solo coordinadores inactivos sin actividad.
  # Si hay actividad (viviendas_visitadas_nube > 0), se conserva la fila aunque
  # el coordinador este inactivo (baja reciente / reasignacion pendiente).
  if ("viviendas_visitadas_nube" %in% names(out_ordenado)) {
    out_ordenado |>
      dplyr::filter(
        is.na(status_coord) | status_coord | viviendas_visitadas_nube > 0
      )
  } else {
    out_ordenado |>
      dplyr::filter(is.na(status_coord) | status_coord)
  }
}


# ---- 5) Exportación a Excel ------------------------------------------------

exportar_productividad_excel <- function(
  bd_prod,
  sheet_name,
  path,
  header_fill = "#7030A0"
) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = sheet_name)
  openxlsx::writeData(wb, sheet = sheet_name, x = bd_prod)

  headerStyle <- openxlsx::createStyle(
    fontColour = "white",
    fgFill = header_fill,
    halign = "center",
    textDecoration = "bold"
  )

  openxlsx::addStyle(
    wb,
    sheet = sheet_name,
    style = headerStyle,
    rows = 1,
    cols = 1:ncol(bd_prod),
    gridExpand = TRUE
  )

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}


# ---- 6) Función orquestadora del reporte ----------------------------------
#' Generar reporte de productividad operativa
#'
#' @description
#' Construye el **reporte de productividad** para una ventana temporal (típicamente un día),
#' combinando:
#' - métricas de actividad (hechos operativos),
#' - información de pase de lista (asistencia y variables reportadas),
#' - y una dimensión administrativa previamente construida (estructura operativa).
#'
#' La función **NO carga datos desde la base de datos**. Asume que todos los insumos
#' ya fueron preparados aguas arriba (ETL o scripts de carga).
#'
#' @details
#' ## Alcance del reporte
#' El reporte:
#' - calcula métricas agregadas por vocero,
#' - opcionalmente incluye coordinadores como filas adicionales,
#' - maneja de forma explícita excepciones operativas (múltiples pases, múltiples cuestionarios),
#' - exporta el resultado a Excel con formato estándar.
#'
#' El reporte **no decide**:
#' - a qué brigada pertenece una persona,
#' - cuál es el municipio correcto históricamente,
#' - ni cómo se resuelven cambios administrativos.
#'
#' Todas esas decisiones pertenecen a la construcción de insumos (`bd_aux`).
#'
#' ## Excepciones explícitas
#' Por defecto, el reporte asume:
#' - un solo pase de lista (un cuestionario),
#' - un solo pase por día y persona.
#'
#' Si estas condiciones no se cumplen, el usuario debe habilitar explícitamente:
#' - `allow_multiple_sources = TRUE` para usar más de un cuestionario,
#' - `allow_multiple_per_day = TRUE` para consolidar múltiples pases diarios.
#'
#' ## Política de datos faltantes
#' - `"lenient"` (default): el reporte se genera aunque falte actividad o pase de lista.
#' - `"strict"`: el reporte falla si no existen datos para la ventana solicitada.
#'
#' @param bd_aux Tibble con la **estructura operativa** (dimensión administrativa),
#'   incluyendo voceros, coordinadores, brigadas y municipio.
#'
#' @param actividad_dia Tibble con los **hechos de actividad** ya filtrados
#'   a la ventana temporal del reporte.
#'
#' @param corte Fecha de corte del reporte (`Date`).
#'
#' @param pl_main Tibble con el **pase de lista principal**, ya parseado.
#'
#' @param pl_extra Tibble opcional con un **pase de lista adicional**.
#'   Su uso requiere `allow_multiple_sources = TRUE`.
#'
#' @param allow_multiple_sources Logical. Permite explícitamente
#'   usar más de un cuestionario de pase de lista.
#'
#' @param allow_multiple_per_day Logical. Permite consolidar múltiples
#'   pases de lista del mismo día para el mismo supervisor–vocero.
#'
#' @param missing_policy Política para manejar ausencia de datos:
#'   `"lenient"` o `"strict"`.
#'
#' @param include_coordinators Logical. Indica si el reporte debe
#'   incluir coordinadores como filas adicionales.
#'
#' @param coordinator_scope Define de dónde se obtiene el universo
#'   de coordinadores:
#'   - `"from_structure"`: desde la estructura administrativa (`bd_aux`)
#'   - `"from_pl"`: desde el pase de lista.
#'
#' @param coordinator_grain Define el nivel de agregación de coordinadores:
#'   - `"coordinator_only"`: una fila por coordinador.
#'   - `"coordinator_by_brigade"`: una fila por coordinador y brigada.
#'
#' @param numeric_threshold Proporción mínima (0–1) para convertir una columna
#'   character a numérica en el pase de lista.
#'
#' @param subir Objeto o ruta a subir a Google Drive. Si `NULL`, no se sube nada.
#'
#' @param carpeta_drive ID de la carpeta de Google Drive destino.
#'
#' @param sheet_name Nombre de la hoja de Excel.
#'
#' @param header_fill Color de fondo del encabezado en el Excel.
#'
#' @param char_default Valor por defecto para campos character faltantes.
#'
#' @param num_default Valor por defecto para campos numéricos faltantes.
#'
#' @param col_geo_actividad Nombre de la columna geográfica en `actividad_dia`
#'   que se usará como columna de agrupación (por defecto `"seccion"`).
#'
#' @return
#' Una lista con:
#' - `corte`: fecha del reporte,
#' - `pase_lista`: pase de lista procesado,
#' - `registros`: métricas de actividad agregadas,
#' - `bd_prod`: tabla final del reporte.
#'
#' @export
generar_reporte_productividad <- function(
  bd_aux,
  actividad_dia,
  corte,
  pl_main,
  pl_extra = NULL,

  # Excepciones controladas (defaults estándar)
  allow_multiple_sources = FALSE,
  allow_multiple_per_day = TRUE,

  # Política de faltantes
  missing_policy = c("lenient", "strict"),

  # Coordinadores como filas
  include_coordinators = TRUE,
  coordinator_scope = c("from_structure", "from_pl"),
  coordinator_grain = c("coordinator_only", "coordinator_by_brigade"),

  # Coerción numérica del pase
  numeric_threshold = 0.95,

  # Output
  subir = NULL,
  carpeta_drive = NULL,
  sheet_name = as.character(corte),
  header_fill = "#7030A0",
  char_default = "-",
  num_default = 0,
  col_geo_actividad = "seccion"
) {
  missing_policy <- match.arg(missing_policy)
  coordinator_scope <- match.arg(coordinator_scope)
  coordinator_grain <- match.arg(coordinator_grain)

  # 1) Pase de lista (día)
  if (!is.null(pl_main)) {
    pl <- preparar_pase_lista(
      pl_main = pl_main,
      pl_extra = pl_extra,
      corte = corte,
      allow_multiple_sources = allow_multiple_sources,
      allow_multiple_per_day = allow_multiple_per_day,
      missing_policy = missing_policy,
      numeric_threshold = numeric_threshold
    )
  } else {
    pl <- NULL
  }

  # 2) Métricas actividad (día)
  registros <- resumir_actividad(
    actividad = actividad_dia,
    missing_policy = missing_policy,
    col_seccion = col_geo_actividad
  )

  # 3) Ensamble
  bd_prod_raw <- ensamblar_productividad(
    bd_aux = bd_aux,
    registros = registros,
    pl = pl,
    include_coordinators = include_coordinators,
    coordinator_scope = coordinator_scope,
    coordinator_grain = coordinator_grain
  )
  # 4) Post-proceso (imputación + banderas + orden)
  bd_prod <- postprocesar_output(
    bd_prod_raw,
    char_default = char_default,
    num_default = num_default
  ) |>
    filter(nombre_coordinador != "-" | dialogos_efectivos_nube > 0)

  list(
    corte = corte,
    pase_lista = pl,
    registros = registros,
    bd_prod = bd_prod
  )
}



#' Generar tablas de resumen para el reporte de productividad
#'
#' @param bd_prod Data frame producido por \code{generar_reporte_productividad()},
#'   con columnas \code{nombre_brigada} y métricas de actividad por vocero.
#'
#' @return Lista de tablas (flextable / data frame) listas para insertar en la
#'   presentación PPTX.
#'
#' @section Seguridad y Privacidad:
#' Nivel de datos: INTERNO (métricas agregadas por brigada y coordinador).
#' No contiene PII de encuestados. Los nombres de coordinador son datos
#' operativos internos. Control ISO 27001: A.8.2 (Clasificación de información).
#'
#' @importFrom flextable hrule padding
#' @export
generar_tablas_reporte <- function(bd_prod) {
  # --- 1. PROCESAMIENTO TABLA ACUMULADA (RESUMEN) ---
  nombres <- bd_prod |> 
    filter(!nombre_brigada == "-", !stringr::str_detect(nombre_brigada, "CAPACITACIONES")) |>
    distinct(nombre_brigada) |> 
    pull(nombre_brigada) |> 
    sort()
  
  base_resumen <- bd_prod |>
    filter(nombre_brigada %in% nombres)

  general <- base_resumen |>
    summarise(
      nivel   = "General",
      nombre  = "TOTAL",
      usuarios = sum(dialogos_efectivos_nube > 0, na.rm = TRUE),
      dialogos = sum(dialogos_efectivos_nube, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(promedio = round(dialogos / if_else(usuarios == 0, NA_real_, usuarios), 2))

  por_brigada <- base_resumen |>
    group_by(nombre_brigada) |>
    summarise(
      nivel   = "Brigada",
      nombre  = first(nombre_brigada),
      usuarios = sum(dialogos_efectivos_nube > 0, na.rm = TRUE),
      dialogos = sum(dialogos_efectivos_nube, na.rm = TRUE),
      .groups = "drop"
    )

  brigadas_huerfanas <- por_brigada |>
    filter(usuarios == 0, dialogos > 0)

  if (nrow(brigadas_huerfanas) > 0) {
    detalle <- paste(
      sprintf("  - %s: %d diálogo(s)", brigadas_huerfanas$nombre, brigadas_huerfanas$dialogos),
      collapse = "\n"
    )
    stop(sprintf(
      paste0(
        "generar_tablas_reporte: %d brigada(s) tienen diálogos registrados pero ningún",
        " usuario con actividad. Esto indica una inconsistencia entre bd_prod y la",
        " estructura operativa. Brigadas afectadas:\n%s"
      ),
      nrow(brigadas_huerfanas), detalle
    ))
  }

  por_brigada <- por_brigada |>
    filter(usuarios > 0) |>
    mutate(promedio = round(dialogos / usuarios, 2))

  tabla_acumulada <- bind_rows(
    general     |> mutate(.ord0 = 0, .ord2 = 0),
    por_brigada |> mutate(.ord0 = 2, .ord2 = as.numeric(factor(nombre)))
  ) |>
    arrange(.ord0, .ord2) |>
    select(-starts_with(".ord")) |>
    mutate(
      promedio = replace_na(promedio, 0),
      color = case_when(
        promedio >= 14.5 ~ "verde",
        promedio >= 10   ~ "amarillo",
        TRUE             ~ "naranja"
      )
    )

  # --- 2. CREACIÓN FLEXTABLE RESUMEN ---
  # 1. Asegúrate de que las columnas de referencia existan en col_keys
columnas_visibles <- c("nivel", "nombre", "usuarios", "dialogos", "promedio")

ft_resumen <- tabla_acumulada |>
    flextable(col_keys = columnas_visibles) |>
    # 2. Etiquetas visuales (esto no afecta a las fórmulas posteriores)
    set_header_labels(
      nivel    = "Nivel",
      nombre   = "Brigada",
      usuarios = "Usuarios con actividad",
      dialogos = "Diálogos efectivos",
      promedio = "Promedio de diálogos por vocero"
    ) |>
    # 3. Formato condicional
    # ¡OJO! Aquí usamos 'color' porque es la columna que calculaste en el mutate
    # Si 'color' no está en col_keys, flextable no la encontrará.
    bg(i = ~ color == "verde",    j = "promedio", bg = "#92D050", part = "body") |>
    bg(i = ~ color == "amarillo", j = "promedio", bg = "#FFFF00", part = "body") |>
    bg(i = ~ color == "naranja",  j = "promedio", bg = "#FFC000", part = "body") |>
    bold(i = ~ nivel == "General", part = "body") |>
    fontsize(size = 11, part = "all") |>
    padding(padding.top = 1, padding.bottom = 1, part = "body") |>
    height(height = 0.2, part = "body") |>
    hrule(rule = "exact", part = "body") |>
    # USAR NOMBRES ORIGINALES AQUÍ:
    width(j = "nivel",    width = 1.0,  unit = "in") |>
    width(j = "nombre",   width = 4.0,  unit = "in") |> 
    width(j = "usuarios", width = 0.8,  unit = "in") |>
    width(j = "dialogos", width = 0.8,  unit = "in") |>
    width(j = "promedio", width = 1.5,  unit = "in") |>
    align(j = 3:5, align = "right", part = "all") |>
    border_inner(border = fp_border(color = "black", width = 1), part = "all") |>
    border_outer(border = fp_border(color = "black", width = 1), part = "all")
  # --- 3. PROCESAMIENTO TABLA DETALLE (JA) BLINDADO ---
  ja <- bd_prod |>
    filter(!nombre_coordinador == nombre_vocero) |>
    filter(!is.na(nombre_coordinador) & nombre_coordinador != "-") |>
    # Inyección defensiva: Garantizamos que la columna exista para el summarise
    (\(data) {
      if (!"numero_pases_lista" %in% names(data)) {
        mutate(data, numero_pases_lista = 0)
      } else {
        data
      }
    })() |>
    mutate(
      # Utilizamos la bandera robusta creada aguas arriba en postprocesar_output()
      tiene_pase = tiene_pase_lista,
      estatus_individual = if_else(
        tiene_pase,
        paste0("✅ ", nombre_coordinador),
        paste0("❌ ", nombre_coordinador)
      )
    ) |>
    arrange(desc(tiene_pase), nombre_coordinador) |>
    group_by(nombre_coordinador) |>
    summarise(
      total_coord = n_distinct(nombre_coordinador),
      lista_estatus = paste(unique(estatus_individual), collapse = "\n"),
      total_pases_distrito = sum(numero_pases_lista, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 4. CREACIÓN FLEXTABLE DETALLE ---
  ft_detalle <- flextable(
    ja,
    col_keys = c("total_coord", "lista_estatus", "total_pases_distrito")
  ) |>
    set_header_labels(
      total_coord   = "Total Coordinadores",
      lista_estatus = "Estatus de Coordinadores (Pase de Lista)",
      total_pases_distrito = "Total pases por coordinador"
    ) |>
    valign(j = "lista_estatus", valign = "top") |>
    fontsize(size = 12, part = "all") |>
    bold(part = "header") |>
    width(j = 1, width = 1.2, unit = "in") |>
    width(j = 2, width = 4.8, unit = "in") |>
    width(j = 3, width = 1.2, unit = "in") |>
    align(j = c(1, 3), align = "center", part = "all") |>
    align(j = 2, align = "left", part = "all") |>
    border_inner(border = fp_border(color = "black", width = 1), part = "all") |>
    border_outer(border = fp_border(color = "black", width = 1), part = "all") |>
    bg(part = "header", bg = "#F2F2F2") |>
    line_spacing(space = 1.2, part = "body")

  # Retornar ambas tablas como una lista
  return(list(resumen = ft_resumen, detalle = ft_detalle, cruda = tabla_acumulada))
}
