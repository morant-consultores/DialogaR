# -------------------------------------------------------------------------
# PROYECTO: DialogaR
# SCRIPT:   fct_paseLista.R
# OBJETIVO: Reconstrucción dinámica del JSON de SurveyJS para el pase de lista.
# AUTOR:    Rafael López / Equipo de Análisis
# FECHA:    2026-03-24
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno / Crítico
# -------------------------------------------------------------------------
# NOTAS DE SEGURIDAD:
# - Esta función modifica directamente el esquema del instrumento en la BD.
# - Escapa el JSON con glue::glue_sql() para evitar inyección y corrupción
#   por comillas internas. No usa binding de parámetros TDS porque FreeTDS
#   trunca NVARCHAR(MAX) grandes al cruzar el protocolo de parámetros.
# -------------------------------------------------------------------------

# ---- Helpers Internos (No exportados) -----------------------------------

# 1. Construir la base operativa (Extrae y cruza la jerarquía)
construir_base_operativa_pl <- function(
  pool,
  id_proyecto,
  ids_encuestas_dialogo,
  id_cargo_supervisor
) {
  usuarios <- dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::collect()
  usuarios_encuesta <- dplyr::tbl(pool, "UsuariosEncuesta") |> dplyr::collect()
  brigadas <- dplyr::tbl(pool, "Brigadas") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::collect()
  usuario_log <- dplyr::tbl(pool, "UsuarioLog") |>
    dplyr::filter(IdProyecto == !!id_proyecto) |>
    dplyr::collect()

  base_aux <- usuario_log |>
    dplyr::arrange(dplyr::desc(FechaInsert)) |>
    dplyr::distinct(IdUsuario, .keep_all = TRUE) |>
    dplyr::select(IdSupervisor, IdUsuario, IdBrigada) |>
    dplyr::filter(!is.na(IdSupervisor)) |>
    dplyr::left_join(
      brigadas |> dplyr::select(Id, NombreBrigada),
      by = dplyr::join_by(IdBrigada == Id)
    ) |>
    dplyr::inner_join(
      usuarios |>
        dplyr::transmute(
          Id,
          nombre_coordinador = paste(Nombre, APaterno, AMaterno),
          supervisor = Num,
          status_supervisor = Status,
          IdCargoSupervisor = IdCargo
        ),
      by = dplyr::join_by(IdSupervisor == Id)
    ) |>
    dplyr::inner_join(
      usuarios |>
        dplyr::transmute(
          Id,
          nombre_vocero = paste(Nombre, APaterno, AMaterno),
          vocero = Num,
          status_vocero = Status,
          IdCargoVocero = IdCargo
        ),
      by = dplyr::join_by(IdUsuario == Id)
    ) |>
    dplyr::mutate(dplyr::across(
      dplyr::contains("nombre"),
      ~ gsub("  ", " ", stringr::str_to_upper(stringr::str_squish(.x)))
    )) |>
    dplyr::filter(as.logical(status_vocero), IdCargoSupervisor == !!id_cargo_supervisor)

  coordinadores_voceros <- base_aux |>
    dplyr::distinct(IdSupervisor, .keep_all = TRUE) |>
    dplyr::filter(status_supervisor == TRUE) |>
    dplyr::left_join(
      usuarios_encuesta |> dplyr::select(UsuarioId, EncuestaId),
      by = dplyr::join_by(IdSupervisor == UsuarioId)
    ) |>
    dplyr::filter(EncuestaId %in% ids_encuestas_dialogo) |>
    dplyr::distinct(IdSupervisor, .keep_all = TRUE) |>
    dplyr::mutate(
      IdUsuario = IdSupervisor,
      nombre_vocero = nombre_coordinador,
      vocero = supervisor,
      status_vocero = status_supervisor
    )

  dplyr::bind_rows(base_aux, coordinadores_voceros) |>
    dplyr::arrange(nombre_coordinador, nombre_vocero)
}

# 2. Extraer y parsear el JSON original del cuestionario
extraer_json_molde <- function(pool, id_pase_lista) {
  json_completo <- dplyr::tbl(pool, "Encuesta") |>
    dplyr::filter(Id == !!id_pase_lista) |>
    dplyr::pull(JsonData)

  todo <- json_completo |>
    jsonlite::fromJSON() |>
    purrr::pluck("pages") |>
    dplyr::as_tibble() |>
    tibble::rownames_to_column("id") |>
    dplyr::mutate(id = as.numeric(id) - 1)

  list(json_crudo = json_completo, tabla_paginas = todo)
}

# 3. Generar las páginas dinámicas para cada vocero
generar_paginas_dinamicas <- function(base_operativa, pagina_cero) {
  purrr::pmap(
    base_operativa,
    function(vocero, nombre_vocero, nombre_coordinador, supervisor, ...) {
      elementos <- pagina_cero$elements |>
        purrr::pluck(1) |>
        dplyr::mutate(
          dplyr::across(c(name, title, visibleIf), ~ gsub("0", vocero, .x)),
          visibleIf = gsub("'", "''", visibleIf)
        ) |>
        dplyr::as_tibble()

      tibble::tibble(
        name = vocero,
        elements = list(elementos),
        title = glue::glue("Vocero {nombre_vocero}"),
        visibleIf = glue::glue(
          "{Obtener_usuario} = ''[supervisor]''",
          .open = "[",
          .close = "]"
        )
      ) |>
        jsonlite::toJSON(pretty = TRUE) |>
        (\(.) substr(., 2, nchar(.) - 1))()
    }
  )
}

# 4. Ensamblar todas las piezas en un solo JSON válido
ensamblar_json_final <- function(
  json_crudo,
  tabla_paginas,
  paginas_voceros,
  base_operativa
) {
  relacion <- base_operativa |>
    dplyr::distinct(nombre_coordinador, supervisor) |>
    dplyr::transmute(value = supervisor, text = nombre_coordinador)

  primera <- json_crudo |>
    jsonlite::fromJSON() |>
    (\(.) .[-match("pages", names(.))])() |>
    jsonlite::toJSON() |>
    (\(.) gsub("\\[|\\]", "", .))() |>
    (\(.) gsub("}", ",", .))() |>
    paste0("\"pages\": [")

  segundo <- tabla_paginas |>
    dplyr::filter(name %in% c("Inicial", 0)) |>
    (\(.) split(., .$id))() |>
    purrr::map(
      ~ {
        tabla <- .x$elements |> purrr::pluck(1) |> dplyr::as_tibble()
        if ("visibleIf" %in% names(tabla)) {
          elements <- tabla |>
            dplyr::mutate(visibleIf = gsub("'", "''", visibleIf))
        } else {
          tabla <- tabla |> dplyr::mutate(choices = list(relacion))
          choices <- tabla |>
            dplyr::pull(choices) |>
            purrr::map(
              ~ if ("visibleIf" %in% names(.x)) {
                dplyr::mutate(.x, visibleIf = gsub("'", "''", visibleIf))
              } else {
                .x
              }
            )
          elements <- .x$elements |>
            purrr::pluck(1) |>
            dplyr::as_tibble() |>
            dplyr::mutate(choices = choices)
        }
        .x$elements <- list(elements)
        .x |>
          dplyr::select(-id) |>
          jsonlite::toJSON() |>
          (\(.) substr(., 2, nchar(.) - 1))()
      }
    )

  final <- tabla_paginas |>
    dplyr::filter(name == "Final") |>
    dplyr::select(name, elements) |>
    jsonlite::toJSON() |>
    (\(.) substr(., 2, nchar(.) - 1))()

  paste(
    primera,
    paste(
      append(append(segundo, paginas_voceros), list(final)) |>
        paste(collapse = ", "),
      "\n ]\n}"
    )
  )
}

# ---- Orquestador Principal (Exportado) ----------------------------------

#' Actualizar Estructura del Pase de Lista
#'
#' @description
#' Orquesta el proceso de reconstrucción dinámica del JSON de SurveyJS para el pase de lista.
#' Incluye un mecanismo de Backup automático del JSON original antes de cualquier modificación
#' para garantizar la recuperación ante desastres (ISO 27000).
#'
#' @param pool Conexión activa a la base de datos (DBI).
#' @param id_proyecto Numeric. ID del proyecto.
#' @param id_pase_lista Numeric. ID del cuestionario de pase de lista a modificar.
#' @param ids_encuestas_dialogo Numeric vector. IDs de los cuestionarios operativos.
#' @param id_cargo_supervisor Numeric. ID del cargo que funge como coordinador. Por defecto 37.
#' @param dir_backup Character. Carpeta donde se guardarán los respaldos del JSON. Por defecto "backups_pl".
#'
#' @return Invisible TRUE si la actualización en BD es exitosa.
#' @export
actualizar_pase_lista <- function(
  pool,
  id_proyecto,
  id_pase_lista,
  ids_encuestas_dialogo,
  id_cargo_supervisor = 37,
  dir_backup = "backups_pl"
) {
  # 1. Construir jerarquía operativa
  base_operativa <- construir_base_operativa_pl(
    pool,
    id_proyecto,
    ids_encuestas_dialogo,
    id_cargo_supervisor
  )

  # 2. Extraer JSON molde de la BD
  moldes <- extraer_json_molde(pool, id_pase_lista)
  pagina_cero <- moldes$tabla_paginas |>
    dplyr::filter(name == "0") |>
    dplyr::select(-c(id, visible))

  # -----------------------------------------------------------------------
  # 3. BACKUP DE SEGURIDAD (Disaster Recovery)
  # -----------------------------------------------------------------------
  if (!dir.exists(dir_backup)) {
    dir.create(dir_backup, recursive = TRUE)
  }

  # Generar un timestamp seguro para el nombre del archivo
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  archivo_backup <- file.path(
    dir_backup,
    paste0("backup_pl_", id_pase_lista, "_", timestamp, ".json")
  )

  # Escribir el JSON original intacto en el disco local
  writeLines(moldes$json_crudo, archivo_backup)

  # Alerta en consola para trazabilidad
  cli::cli_alert_success(
    "Backup de seguridad creado exitosamente en: {archivo_backup}"
  )
  # -----------------------------------------------------------------------

  # 4. Generar páginas dinámicas para los voceros
  paginas_voceros <- generar_paginas_dinamicas(base_operativa, pagina_cero)

  # 5. Ensamblar JSON final
  json_actualizado <- ensamblar_json_final(
    json_crudo = moldes$json_crudo,
    tabla_paginas = moldes$tabla_paginas,
    paginas_voceros = paginas_voceros,
    base_operativa = base_operativa
  )

  # 6. Validación y persistencia
  if (!jsonlite::validate(json_actualizado)) {
    cli::cli_abort(c(
      "JSON ensamblado inválido para el cuestionario {id_pase_lista}.",
      "i" = "Backup intacto en: {archivo_backup}",
      "i" = "No se ejecutó UPDATE."
    ))
  }

  fecha_mod <- as.character(lubridate::now(tzone = 'America/Mexico_City'))

  # glue_sql escapa comillas internas correctamente. El incremento de Version
  # se hace en el servidor (COALESCE es ANSI: válido en SQL Server y SQLite)
  # para evitar la condición de carrera del patrón read-modify-write.
  query_update <- glue::glue_sql(
    "UPDATE Encuesta
        SET JsonData = {json_actualizado},
            Version = COALESCE(Version, 0) + 1,
            FechaModificacion = {fecha_mod}
      WHERE Id = {id_pase_lista}",
    .con = pool
  )

  DBI::dbExecute(pool, query_update)
  cli::cli_alert_success(
    "Cuestionario {id_pase_lista} actualizado correctamente en la base de datos."
  )

  invisible(TRUE)
}
