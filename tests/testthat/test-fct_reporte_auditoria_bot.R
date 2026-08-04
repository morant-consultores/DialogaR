# tests/testthat/test-fct_reporte_auditoria_bot.R

veredicto_json <- function(dictamen, total, obs = "ok") {
  jsonlite::toJSON(list(
    dictamenFinal = dictamen,
    totalEvaluacion = as.character(total),
    observaciones = obs
  ), auto_unbox = TRUE)
}

# ResultadoAuditoriaBot.Fecha y EvaluacionRegistro.Fecha se almacenan en UTC en producción
# (SQL Server); el mock reproduce ese formato ISO-8601 con sufijo "Z" porque es la forma
# exacta en la que dbplyr traduce un literal POSIXct para el filtro de fecha, y así la
# comparación lexicográfica en SQLite (usada solo en pruebas) coincide con el formato real.
fecha_utc_cdmx <- function(fecha_hora_cdmx) {
  instante_utc <- lubridate::with_tz(
    as.POSIXct(fecha_hora_cdmx, tz = "America/Mexico_City"), "UTC"
  )
  format(instante_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

crear_con_bot <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "Registros", data.frame(
    Id = c(1L, 2L, 3L),
    EncuestaId = c(1L, 1L, 1L),
    UsuarioNum = c("101", "102", "103"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "LoteAuditoria", data.frame(
    Id = 1L,
    EncuestaId = 1L,
    LoteIdExterno = "L1",
    FechaInsert = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  ))

  # RegistroId 1 se audita dos veces en días CDMX distintos, cruzando la medianoche:
  # Id = 1 ocurre 2026-06-01 23:30 CDMX (06-02 05:30 UTC) -> día CDMX = 01 de junio.
  # Id = 2 (re-auditoría, gana sobre Id = 1) ocurre 2026-06-02 08:00 CDMX -> día CDMX = 02 de junio.
  # RegistroId 2 (Id = 3) también ocurre 2026-06-02 08:00 CDMX.
  DBI::dbWriteTable(con, "ResultadoAuditoriaBot", data.frame(
    Id = c(1L, 2L, 3L),
    RegistroId = c(1L, 1L, 2L),
    LoteAuditoriaId = c(1L, 1L, 1L),
    Fecha = c(
      fecha_utc_cdmx("2026-06-01 23:30:00"),
      fecha_utc_cdmx("2026-06-02 08:00:00"),
      fecha_utc_cdmx("2026-06-02 08:00:00")
    ),
    VeredictoJson = c(
      as.character(veredicto_json("Diálogo Deficiente", 1)),
      as.character(veredicto_json("Diálogo Óptimo", 5)),
      as.character(veredicto_json("Diálogo Aceptable", 3))
    ),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "RevisionAuditoriaBot", data.frame(
    Id = 1L,
    ResultadoAuditoriaBotId = 3L,
    CalificacionVeredicto = 2L,
    VeredictoCorregido = as.character(veredicto_json("Diálogo Deficiente", 1, "corregido por humano")),
    stringsAsFactors = FALSE
  ))

  con
}

test_that("fetch_auditoria_bot colapsa re-auditorías quedándose con la última", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, encuesta_id = 1L)

  fila_1 <- res[res$RegistroId == 1, ]
  expect_equal(nrow(fila_1), 1)
  expect_equal(fila_1$dictamenFinal, "Diálogo Óptimo")
})

test_that("fetch_auditoria_bot aplica la corrección humana cuando existe", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, encuesta_id = 1L)

  fila_2 <- res[res$RegistroId == 2, ]
  expect_equal(nrow(fila_2), 1)
  expect_equal(fila_2$dictamenFinal, "Diálogo Deficiente")
  expect_equal(fila_2$observaciones, "corregido por humano")
})

test_that("fetch_auditoria_bot asigna 'fecha' según el día natural en CDMX, no en UTC", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, encuesta_id = 1L)

  # La auditoría Id = 1 ocurrió a las 23:30 CDMX del 1 de junio, aunque en UTC ya era
  # madrugada del 2 de junio. Si se tomara la fecha UTC directamente (sin convertir a
  # CDMX), se le asignaría por error el 2 de junio.
  fila_2 <- res[res$RegistroId == 2, ]
  expect_equal(fila_2$fecha, as.Date("2026-06-02"))
})

test_that("fetch_auditoria_bot empuja el filtro de fechas usando el día natural CDMX", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  # Ventana = 2 de junio (CDMX): incluye la re-auditoría de RegistroId 1 (Id = 2) y
  # RegistroId 2 (Id = 3); excluye la primera pasada de RegistroId 1 (Id = 1, día CDMX = 1 de junio).
  res <- fetch_auditoria_bot(con, encuesta_id = 1L,
                             fecha_inicio = as.Date("2026-06-02"),
                             fecha_fin = as.Date("2026-06-02"))
  expect_setequal(res$RegistroId, c(1, 2))
  expect_equal(res$dictamenFinal[res$RegistroId == 1], "Diálogo Óptimo")

  # Ventana = 1 de junio (CDMX): solo la primera pasada de RegistroId 1 (Id = 1) cae aquí,
  # aunque su timestamp UTC ya sea 2 de junio.
  res_previo <- fetch_auditoria_bot(con, encuesta_id = 1L,
                                    fecha_inicio = as.Date("2026-06-01"),
                                    fecha_fin = as.Date("2026-06-01"))
  expect_setequal(res_previo$RegistroId, 1)
  expect_equal(res_previo$dictamenFinal[res_previo$RegistroId == 1], "Diálogo Deficiente")
})

test_that("obtener_evaluaciones('combinar') privilegia el bot sobre el legado", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "EvaluacionRegistro", data.frame(
    RegistroId = c(1L, 3L),
    Fecha = as.POSIXct(c("2026-06-01 12:00:00", "2026-06-01 12:00:00"), tz = "America/Mexico_City"),
    Resultado = c(
      as.character(veredicto_json("Diálogo Deficiente", 1, "legado, debe perder frente al bot")),
      as.character(veredicto_json("Diálogo Aceptable", 3, "legado, no tiene equivalente en bot"))
    ),
    stringsAsFactors = FALSE
  ))

  res <- obtener_evaluaciones(con, encuesta_id = 1L, fuente_auditoria = "combinar",
                              fecha_inicio_au = as.Date("2026-06-01"), fecha_fin_au = as.Date("2026-06-02"))

  # RegistroId 1 existe en ambas fuentes: debe ganar el bot (Óptimo), no el legado (Deficiente).
  fila_1 <- res[res$RegistroId == 1, ]
  expect_equal(fila_1$dictamenFinal, "Diálogo Óptimo")

  # RegistroId 3 solo existe en el legado: debe conservarse.
  fila_3 <- res[res$RegistroId == 3, ]
  expect_equal(nrow(fila_3), 1)
  expect_equal(fila_3$dictamenFinal, "Diálogo Aceptable")

  # RegistroId 2 solo existe en el bot (con corrección humana aplicada).
  fila_2 <- res[res$RegistroId == 2, ]
  expect_equal(fila_2$dictamenFinal, "Diálogo Deficiente")
  expect_equal(fila_2$observaciones, "corregido por humano")
})
