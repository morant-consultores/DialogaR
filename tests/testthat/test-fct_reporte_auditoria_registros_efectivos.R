# tests/testthat/test-fct_reporte_auditoria_registros_efectivos.R

# Registros.FechaInicio se almacena en UTC en producción; el mock guarda el mismo
# formato ISO-8601 con sufijo "Z" que dbplyr usa al traducir un literal POSIXct para el
# filtro de fecha, para que la comparación lexicográfica en SQLite (solo en pruebas)
# coincida con el formato real.
fecha_utc_cdmx <- function(fecha_hora_cdmx) {
  instante_utc <- lubridate::with_tz(as.POSIXct(fecha_hora_cdmx, tz = "America/Mexico_City"), "UTC")
  format(instante_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

crear_con_registros <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Registros.FechaInicio se almacena en UTC en producción; RegistroId 1 ocurrió a las
  # 23:30 CDMX del 1 de junio (06-02 05:30 UTC) — cruza la medianoche al convertir a UTC.
  # RegistroId 2 y 3 ocurrieron el 2 de junio CDMX. RegistroId 4 es "Cancelado" (no debe
  # contarse como diálogo efectivo, sin importar su fecha).
  DBI::dbWriteTable(con, "Registros", data.frame(
    Id = c(1L, 2L, 3L, 4L),
    EncuestaId = c(1L, 1L, 1L, 1L),
    UsuarioNum = c("101", "102", "103", "104"),
    TipoRegistro = c("Efectivo", "Efectivo", "Efectivo", "Cancelado"),
    FechaInicio = fecha_utc_cdmx(c(
      "2026-06-01 23:30:00", "2026-06-02 08:00:00", "2026-06-02 08:00:00", "2026-06-02 08:00:00"
    )),
    stringsAsFactors = FALSE
  ))

  con
}

test_that("registros_efectivos filtra TipoRegistro == 'Efectivo'", {
  con <- crear_con_registros()
  on.exit(DBI::dbDisconnect(con))

  res <- registros_efectivos(con, encuesta_id = 1L)

  expect_setequal(res$RegistroId, c(1, 2, 3))
  expect_false(4 %in% res$RegistroId)
})

test_that("registros_efectivos asigna 'fecha' según el día natural CDMX, no UTC", {
  con <- crear_con_registros()
  on.exit(DBI::dbDisconnect(con))

  res <- registros_efectivos(con, encuesta_id = 1L)

  # RegistroId 1 ocurrió a las 23:30 CDMX del 1 de junio, aunque en UTC ya era madrugada
  # del 2 de junio. Si se tomara la fecha UTC directamente se le asignaría por error el 2.
  expect_equal(res$fecha[res$RegistroId == 1], as.Date("2026-06-01"))
  expect_equal(res$fecha[res$RegistroId == 2], as.Date("2026-06-02"))
})

test_that("registros_efectivos empuja el filtro de fechas usando el día natural CDMX", {
  con <- crear_con_registros()
  on.exit(DBI::dbDisconnect(con))

  res_1jun <- registros_efectivos(con, encuesta_id = 1L,
                                  fecha_inicio = as.Date("2026-06-01"), fecha_fin = as.Date("2026-06-01"))
  expect_setequal(res_1jun$RegistroId, 1)

  res_2jun <- registros_efectivos(con, encuesta_id = 1L,
                                  fecha_inicio = as.Date("2026-06-02"), fecha_fin = as.Date("2026-06-02"))
  expect_setequal(res_2jun$RegistroId, c(2, 3))
})
