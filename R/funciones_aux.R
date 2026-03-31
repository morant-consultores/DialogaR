#' Obtiene la lista de brigadas de un proyecto
#'
#' @param pool Conexión a la base de datos.
#' @param id_proyecto Identificador del proyecto.
#'
#' @return Un tibble con la lista de brigadas, incluyendo id, distrito y nombre de brigada.
obtener_brigadas <- function(pool, id_proyecto) {
  tbl(pool, "Brigadas") |>
    filter(IdProyecto == !!id_proyecto) |>
    collect() |>
    janitor::clean_names() |>
    mutate(distrito = stringr::str_sub(nombre_brigada, 1, 2)) |>
    transmute(
      id,
      distrito,
      nombre_brigada = gsub("_", " ", nombre_brigada),
      id_usuario
    ) |>
    distinct()
}

#' Obtiene la lista de coordinadores de un proyecto
#'
#' @param pool Conexión a la base de datos.
#' @param id_proyecto Identificador del proyecto.
#' @param brigadas Tibble con la lista de brigadas.
#'
#' @return Un tibble con la lista de coordinadores, incluyendo id, municipio, nombre completo, cargo y otros detalles.
obtener_coordinadores <- function(pool, id_proyecto, brigadas) {
  id_coord <- brigadas |>
    filter(!is.na(id_usuario)) |>
    pull(id_usuario)

  tbl(pool, "Usuarios") |>
    filter(IdProyecto == !!id_proyecto, Id %in% id_coord) |>
    janitor::clean_names() |>
    collect() |>
    mutate(across(nombre:a_materno, ~ tidyr::replace_na(.x, ""))) |>
    transmute(
      id,
      municipio,
      nombre_completo = toupper(stringr::str_squish(paste(
        nombre,
        a_paterno,
        a_materno
      ))),
      cargo,
      num,
      fecha_update,
      id_brigada,
      id_usuario,
      status_coord = status
    ) |>
    left_join(
      brigadas |>
        select(-id_usuario),
      join_by(id_brigada == id)
    )
}

#' Obtiene la lista de voceros de un proyecto
#'
#' @param pool Conexión a la base de datos.
#' @param id_proyecto Identificador del proyecto.
#'
#' @return Un tibble con la lista de voceros, incluyendo id, municipio, nombre completo, número y otros detalles.
obtener_voceros <- function(pool, id_proyecto) {
  tbl(pool, "Usuarios") |>
    filter(IdProyecto == !!id_proyecto) |>
    filter(Cargo == "Vocero") |>
    janitor::clean_names() |>
    collect() |>
    transmute(
      id,
      municipio,
      nombre_completo = toupper(stringr::str_squish(paste(
        nombre,
        a_paterno,
        a_materno
      ))),
      num,
      fecha_update,
      id_usuario,
      status_vocero = status,
      id_brigada
    )
}

crear_relacion_voceros <- function(pool, id_proyecto, usuarios_num) {
  usuarios_voceros <- tbl(pool, "Usuarios") |>
    filter(id_proyecto == !!id_proyecto) |>
    filter(Num %in% !!usuarios_num, Cargo == "Voceros", Status == TRUE) |>
    collect() |>
    transmute(
      nombre_vocero = toupper(stringr::str_squish(paste(
        Nombre,
        APaterno,
        AMaterno
      ))),
      usuario_num = Num,
      IdUsuario,
      IdBrigada
    )

  usuarios_coordinadores <- tbl(pool, "Usuarios") |>
    filter(id_proyecto == !!id_proyecto) |>
    filter(Num %in% !!usuarios_num, Cargo != "Voceros", Status == TRUE) |>
    collect() |>
    transmute(
      nombre_vocero = toupper(stringr::str_squish(paste(
        Nombre,
        APaterno,
        AMaterno
      ))),
      usuario_num = Num,
      IdUsuario,
      IdBrigada
    )

  coordinadores <- tbl(pool, "Usuarios") |>
    filter(id_proyecto == !!id_proyecto, Cargo != "Voceros", Status == TRUE) |>
    collect() |>
    transmute(
      Id,
      nombre_coordinador = toupper(stringr::str_squish(paste(
        Nombre,
        APaterno,
        AMaterno
      ))),
      usuario_num = Num
    )

  brigadas <- tbl(pool, "Brigadas") |>
    filter(IdProyecto == !!id_proyecto) |>
    collect() |>
    arrange(desc(FechaInsert)) |>
    distinct(Id, NombreBrigada)

  tabla_voceros_coordinadores <- bind_rows(
    usuarios_voceros |>
      left_join(coordinadores, join_by(IdUsuario == Id)) |>
      left_join(brigadas, join_by(IdBrigada == Id)) |>
      transmute(
        nombre_brigada = NombreBrigada,
        nombre_coordinador,
        nombre_vocero,
        usuario_num = usuario_num.x,
      ),
    usuarios_coordinadores |>
      left_join(brigadas, join_by(IdBrigada == Id)) |>
      transmute(
        nombre_brigada = NombreBrigada,
        nombre_coordinador = nombre_vocero,
        nombre_vocero,
        usuario_num = usuario_num
      )
  ) |>
    mutate(zona = stringr::str_sub(nombre_brigada, 1, 2)) |>
    relocate(zona, .before = nombre_brigada)

  return(tabla_voceros_coordinadores)
}

verificar_longitudes_columnas <- function(df, pool, nombre_tabla) {
  estructura_tabla <- DBI::dbGetQuery(
    pool,
    paste0(
      "
    SELECT
      COLUMN_NAME,
      DATA_TYPE,
      CHARACTER_MAXIMUM_LENGTH
    FROM INFORMATION_SCHEMA.COLUMNS
    WHERE TABLE_NAME = '",
      nombre_tabla,
      "'
    ORDER BY ORDINAL_POSITION
  "
    )
  )

  # Identifica columnas de texto en el dataframe
  columnas_texto <- sapply(df, is.character)
  df_texto <- df[, columnas_texto, drop = FALSE]

  resultados <- data.frame(
    columna = character(),
    max_longitud_datos = numeric(),
    max_longitud_tabla = numeric(),
    excede_limite = logical(),
    stringsAsFactors = FALSE
  )

  for (col in names(df_texto)) {
    max_longitud <- max(nchar(df_texto[[col]], keepNA = FALSE), na.rm = TRUE)
    limite_tabla <- estructura_tabla$CHARACTER_MAXIMUM_LENGTH[
      estructura_tabla$COLUMN_NAME == col
    ]

    if (length(limite_tabla) == 0 || is.na(limite_tabla)) {
      limite_tabla <- Inf
    }

    excede <- max_longitud > limite_tabla

    resultados <- rbind(
      resultados,
      data.frame(
        columna = col,
        max_longitud_datos = max_longitud,
        max_longitud_tabla = if (is.infinite(limite_tabla)) {
          "Ilimitado"
        } else {
          limite_tabla
        },
        excede_limite = excede,
        stringsAsFactors = FALSE
      )
    )
  }

  return(resultados)
}

escribir_xlsx_formato <- function(bd, file_path, fecha) {
  wb <- createWorkbook()

  addWorksheet(wb, "Sheet 1")

  writeData(wb, "Sheet 1", bd)

  headerStyle <- createStyle(
    fgFill = "#800080",
    fontColour = "white",
    halign = "center",
    valign = "center",
    textDecoration = "bold",
    wrapText = TRUE
  )

  addStyle(
    wb,
    "Sheet 1",
    style = headerStyle,
    rows = 1,
    cols = 1:ncol(bd),
    gridExpand = TRUE
  )

  for (i in 1:ncol(bd)) {
    setColWidths(wb, "Sheet 1", cols = i, widths = "auto")
  }

  saveWorkbook(wb, file_path, overwrite = TRUE)
  print("Se ha guardado el archivo xlsx de forma exitosa")
}
