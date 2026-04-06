categorizar_ds <- function(
  pool,
  encuesta_id,
  numeros_prueba,
  fecha_inicio,
  puerta = T,
  ids,
  remover_columnas = c()
) {
  conn <- pool::poolCheckout(pool)

  # Usamos on.exit para asegurar que la conexión regrese al pool
  # incluso si la función falla o se interrumpe
  on.exit(pool::poolReturn(conn))

  # 2. Subir los IDs a la tabla temporal usando esa conexión específica
  ids_temp <- copy_to(
    conn,
    data.frame(Id = ids),
    name = "temp_ids",
    temporary = TRUE,
    overwrite = TRUE
  )

  aux <- tbl(conn, "Registros") |>
    filter(
      EncuestaId == !!encuesta_id,
      !UsuarioNum %in% numeros_prueba
    ) |>
    anti_join(ids_temp, by = "Id")

  ja <- aux |>
    filter(n() > 1, .by = IdCliente) |>
    tally() |>
    pull()

  if (ja > 0) {
    print(paste("Registros repetidos", ja))
  } else {
    print("Sin registros repetidos")
  }

  if (tally(aux) |> pull(n) > 0) {
    # 3. Realizar la consulta
    aux <- aux |>
      distinct(IdCliente, .keep_all = T) |>
      collect() |>
      filter(floor_date(FechaInicio) >= fecha_inicio) |>
      pmap_df(function(
        Id,
        FechaInicio,
        FechaFin,
        UsuarioNum,
        Resultado,
        isComplete,
        TipoRegistro,
        ...
      ) {
        list_respuestas <- list()

        list_respuestas$id = Id
        list_respuestas$fecha_inicio = as_datetime(lubridate::with_tz(
          FechaInicio,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$fecha_fin = as_datetime(lubridate::with_tz(
          FechaFin,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$fecha = as_date(lubridate::with_tz(
          FechaInicio,
          tzone = "America/Mexico_City"
        ))
        list_respuestas$usuario_num = UsuarioNum
        list_respuestas$isComplete = isComplete
        list_respuestas$duracion_minutos = round(
          as.numeric(FechaFin - FechaInicio),
          digits = 2
        )

        list_respuestas$TipoRegistro = TipoRegistro

        list_respuestas <- list_respuestas |>
          append(
            Resultado |>
              jsonlite::fromJSON()
          )

        bd_respuestas <- do.call(
          c,
          lapply(seq_along(list_respuestas), function(i) {
            nombre <- names(list_respuestas)[i]
            valores <- list_respuestas[[i]]
            if (length(valores) > 1) {
              setNames(
                as.list(valores),
                paste0(nombre, "_O", seq_along(valores))
              )
            } else {
              setNames(list(valores), nombre)
            }
          })
        )
        return(bd_respuestas)
      }) |>
      select(-any_of(remover_columnas))

    if (puerta == T & nrow(aux) > 0) {
      aux <- aux |>
        mutate(
          desglose = case_when(
            TipoRegistro == "Efectivo" ~ "Efectivo",
            TipoRegistro == "Cancelado" ~ "Cancelado",
            TipoRegistro %in%
              c("Vivienda visitada", "No aplica") &
              puerta == "Sí, rechazaron" ~ "Sí, rechazaron",
            TipoRegistro %in%
              c("Vivienda visitada", "No aplica") &
              puerta == "No abrieron" ~ "No abrieron",
            T ~ "ERROR"
          ),
          edad = if_else(as.numeric(edad) > 120, NA, as.numeric(edad))
        ) |>
        select(c(id, fecha, usuario_num, isComplete), everything())
    }
    return(
      aux |>
        janitor::clean_names()
    )
  } else {
    tibble::tibble()
  }
}
