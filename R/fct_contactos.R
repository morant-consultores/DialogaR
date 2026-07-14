#' Construir base de contactos con clasificación estratégica opcional
#'
#' Integra los registros de encuesta con la jerarquía operativa resuelta por
#' `cargar_insumos()`, filtra los registros que cuentan con al menos un medio
#' de contacto (correo o celular) y, opcionalmente, clasifica a cada contacto
#' según su simpatía de partido y su opinión sobre un personaje político.
#'
#' @param personaje Nombre (sin comillas) de la variable que identifica al
#'   personaje político. La función busca las columnas `conoce_<personaje>`
#'   y `opinion_<personaje>` en `bd_completa`; si no las encuentra, cae a las
#'   columnas genéricas `personaje_conocimiento` y `personaje_opinion`.
#' @param partido Nombre (sin comillas) de la columna en `bd_completa` que
#'   indica la simpatía de partido del entrevistado. Puede ser `NULL`
#'   (predeterminado) cuando no se requiere esa columna ni la clasificación.
#' @param bd_completa Data frame con los registros crudos de encuesta. Debe
#'   contener al menos: `id`, `fecha`, `seccion`, `usuario_num`, columnas de
#'   nombre (`nombre_entrevistado`, `apellido_paterno`, `apellido_materno`) y
#'   domicilio, y las columnas de conocimiento/opinión del personaje.
#' @param corte Fecha de corte (`Date` o cadena interpretable por R). Solo se
#'   incluyen registros con `fecha <= corte`.
#' @param bd_aux Data frame producido por `cargar_insumos()$bd_aux` con la
#'   jerarquía vocero → brigada → coordinador resuelta al corte. Columnas
#'   requeridas: `vocero`, `nombre_vocero`, `nombre_coordinador`.
#' @param na_chr Cadena usada para rellenar `NA` en columnas de carácter.
#'   Predeterminado: `"-"`.
#' @param clasificacion Lógico. Si `TRUE`, añade las columnas `grupo` y
#'   `categoria` con la clasificación estratégica del contacto. Requiere que
#'   `partido` no sea `NULL`. Predeterminado: `FALSE`.
#'
#' @return Data frame con una fila por contacto que tenga correo o celular
#'   registrado. Incluye columnas de identificación, ubicación, datos de
#'   contacto, conocimiento y opinión del personaje, y —cuando
#'   `clasificacion = TRUE`— las columnas `grupo` (carácter `"1"`–`"6"`) y
#'   `categoria` (etiqueta descriptiva).
#'
#' @export
#'
#' @section Seguridad y Privacidad:
#'   Esta función procesa datos personales de los entrevistados (nombre
#'   completo, domicilio, celular, correo). No persiste datos en disco ni los
#'   transmite a servicios externos. Asegúrese de que los datos de entrada
#'   provengan de fuentes autorizadas y de que los resultados se compartan
#'   únicamente con destinatarios con acceso aprobado.
construir_base_contactos <- function(
  personaje,
  partido = NULL,
  bd_completa,
  corte,
  bd_aux,
  na_chr = "-",
  clasificacion = FALSE,
  voceros = NULL,
  brigadas = NULL,
  coordinadores = NULL,
  aux_zonas = NULL
) {
  if (!is.null(voceros) || !is.null(brigadas) || !is.null(coordinadores) || !is.null(aux_zonas)) {
    cli::cli_abort(c(
      "x" = "Los parámetros {.arg voceros}, {.arg brigadas}, {.arg coordinadores} y {.arg aux_zonas} fueron eliminados en v0.3.0.",
      "i" = "Usa {.arg bd_aux} (salida de {.fn cargar_insumos}$bd_aux) en su lugar."
    ))
  }

  # ------------------------------------------------------------
  # Helpers
  # ------------------------------------------------------------
  norm_ascii <- function(x) stringi::stri_trans_general(x, id = "latin-ascii")

  # Captura flexible: texto o símbolo
  personaje_chr <- rlang::as_name(rlang::ensym(personaje))

  # partido puede ser NULL: capturar la expresión sin forzar evaluación del promise
  partido_expr <- rlang::enexpr(partido)
  partido_chr  <- if (!rlang::is_null(partido_expr)) {
    rlang::as_name(partido_expr)
  } else {
    NULL
  }

  # Columnas dinámicas esperadas
  col_con_dyn <- paste0("conoce_", personaje_chr)
  col_opi_dyn <- paste0("opinion_", personaje_chr)

  # Resolver columnas reales en bd_completa
  col_con <- dplyr::case_when(
    col_con_dyn %in% names(bd_completa) ~ col_con_dyn,
    "personaje_conocimiento" %in% names(bd_completa) ~ "personaje_conocimiento",
    TRUE ~ NA_character_
  )

  col_opi <- dplyr::case_when(
    col_opi_dyn %in% names(bd_completa) ~ col_opi_dyn,
    "personaje_opinion" %in% names(bd_completa) ~ "personaje_opinion",
    TRUE ~ NA_character_
  )

  if (is.na(col_con) || is.na(col_opi)) {
    stop(
      sprintf(
        "No encontré columnas de conocimiento/opinión para personaje='%s'. Busqué '%s'/'%s' o versiones genéricas.",
        personaje_chr,
        col_con_dyn,
        col_opi_dyn
      ),
      call. = FALSE
    )
  }

  # Si piden clasificación, partido es requerido
  if (isTRUE(clasificacion) && rlang::is_null(partido_expr)) {
    stop(
      "Si clasificacion = TRUE, debes proporcionar el argumento `partido`.",
      call. = FALSE
    )
  }

  lbl_partido <- partido_chr
  lbl_person  <- personaje_chr

  # ------------------------------------------------------------
  # Catálogo de voceros desde bd_aux (jerarquía ya resuelta)
  # ------------------------------------------------------------

  cat_voceros <- bd_aux |>
    dplyr::filter(!is.na(vocero)) |>
    dplyr::distinct(vocero, .keep_all = TRUE) |>
    dplyr::transmute(
      vocero,
      vocero_nombre = dplyr::coalesce(
        norm_ascii(nombre_vocero),
        norm_ascii(nombre_coordinador)
      )
    )

  # ------------------------------------------------------------
  # Procesamiento base
  # ------------------------------------------------------------

  bd_proc <- bd_completa |>
    dplyr::filter(fecha <= corte) |>
    dplyr::left_join(cat_voceros, dplyr::join_by(usuario_num == vocero))

  # grupo_whats: no todos los cuestionarios lo capturan (p.ej. Cuauhtémoc/304)
  if (!"grupo_whats" %in% names(bd_proc)) {
    bd_proc$grupo_whats <- NA_character_
  }

  res <- bd_proc |>
    dplyr::transmute(
      id,
      fecha,
      vocero             = vocero_nombre,
      usuario_num,
      seccion,
      direccion_calle,
      direccion_num_ext,
      direccion_num_int,
      nombre             = toupper(norm_ascii(paste(
        nombre_entrevistado,
        apellido_paterno,
        apellido_materno
      ))),
      sexo,
      edad,
      celular,
      correo,
      grupo_whats,
      personaje_conocimiento = .data[[col_con]],
      personaje_opinion      = .data[[col_opi]],
    )

  # Agregar partido por nombre de columna (string) para evitar evaluación prematura del símbolo
  if (!is.null(partido_chr)) {
    res <- dplyr::mutate(res, partido = bd_proc[[partido_chr]])
  }

  # Post-proceso común
  res <- res |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, na_chr))
    ) |>
    dplyr::arrange(fecha, seccion, vocero) |>
    dplyr::rename_with(
      ~ gsub("^direccion_", "", .x),
      .cols = dplyr::starts_with("direccion_")
    ) |>
    dplyr::filter(correo != na_chr | celular != na_chr)

  # ------------------------------------------------------------
  # Clasificación estratégica (opcional)
  # ------------------------------------------------------------
  if (isTRUE(clasificacion)) {
    res <- res |>
      dplyr::mutate(
        grupo = dplyr::case_when(
          partido == lbl_partido &
            personaje_conocimiento == "Sí" &
            personaje_opinion == "Buena" ~ "1",

          partido == lbl_partido &
            personaje_conocimiento == "No" ~ "2",

          partido == lbl_partido &
            personaje_conocimiento == "Sí" &
            personaje_opinion != "Buena" ~ "3",

          partido != lbl_partido &
            personaje_conocimiento == "Sí" &
            personaje_opinion == "Buena" ~ "4",

          partido != lbl_partido &
            personaje_conocimiento == "Sí" &
            personaje_opinion != "Buena" ~ "5",

          partido != lbl_partido &
            personaje_conocimiento == "No" ~ "6",

          TRUE ~ NA_character_
        ),
        categoria = dplyr::case_match(
          grupo,
          "1" ~ sprintf("Simpatizantes de %s y %s", lbl_partido, lbl_person),
          "2" ~ sprintf("Simpatizantes %s", lbl_partido),
          "3" ~ sprintf("Simp %s sí, %s no", lbl_partido, lbl_person),
          "4" ~ sprintf("Simp %s, %s no", lbl_person, lbl_partido),
          "5" ~ sprintf("%s no, %s no", lbl_partido, lbl_person),
          "6" ~ sprintf("Otros partidos no conocen %s", lbl_person),
          .default = NA_character_
        )
      )
  }

  return(res)
}
