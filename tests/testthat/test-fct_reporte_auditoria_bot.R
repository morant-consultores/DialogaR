# tests/testthat/test-fct_reporte_auditoria_bot.R

veredicto_json <- function(dictamen, total, obs = "ok") {
  as.character(jsonlite::toJSON(list(
    dictamenFinal = dictamen,
    totalEvaluacion = as.character(total),
    observaciones = obs
  ), auto_unbox = TRUE))
}

make_registros <- function() {
  # RegistroId 1 se audita dos veces (re-auditoría); RegistroId 2 tiene una corrección
  # humana posterior. 'fecha' proviene de Registros.FechaInicio (día del diálogo), no de
  # cuándo se realizó la auditoría — el bot audita en lotes, frecuentemente días después.
  tibble::tibble(
    RegistroId = c(1L, 2L),
    usuario_num = c("101", "102"),
    fecha = as.Date(c("2026-06-01", "2026-06-02"))
  )
}

crear_con_bot <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "ResultadoAuditoriaBot", data.frame(
    Id = c(1L, 2L, 3L),
    RegistroId = c(1L, 1L, 2L),
    VeredictoJson = c(
      veredicto_json("Diálogo Deficiente", 1),
      veredicto_json("Diálogo Óptimo", 5),
      veredicto_json("Diálogo Aceptable", 3)
    ),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "RevisionAuditoriaBot", data.frame(
    Id = 1L,
    ResultadoAuditoriaBotId = 3L,
    CalificacionVeredicto = 2L,
    VeredictoCorregido = veredicto_json("Diálogo Deficiente", 1, "corregido por humano"),
    stringsAsFactors = FALSE
  ))

  con
}

test_that("fetch_auditoria_bot colapsa re-auditorías quedándose con la última", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, make_registros())

  fila_1 <- res[res$RegistroId == 1, ]
  expect_equal(nrow(fila_1), 1)
  expect_equal(fila_1$dictamenFinal, "Diálogo Óptimo")
})

test_that("fetch_auditoria_bot aplica la corrección humana cuando existe", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, make_registros())

  fila_2 <- res[res$RegistroId == 2, ]
  expect_equal(nrow(fila_2), 1)
  expect_equal(fila_2$dictamenFinal, "Diálogo Deficiente")
  expect_equal(fila_2$observaciones, "corregido por humano")
})

test_that("fetch_auditoria_bot hereda 'fecha' y 'usuario_num' de registros, no del propio veredicto", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, make_registros())

  expect_equal(res$fecha[res$RegistroId == 1], as.Date("2026-06-01"))
  expect_equal(res$usuario_num[res$RegistroId == 1], "101")
})

test_that("fetch_auditoria_bot retorna tibble vacío cuando no hay registros", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  res <- fetch_auditoria_bot(con, tibble::tibble(RegistroId = integer(), usuario_num = character(), fecha = as.Date(character())))
  expect_equal(nrow(res), 0)
})

test_that("obtener_evaluaciones('combinar') privilegia el bot sobre el legado, usando la fecha del diálogo", {
  con <- crear_con_bot()
  on.exit(DBI::dbDisconnect(con))

  # Registros: RegistroId 1 y 2 auditados por el bot (arriba); RegistroId 3 solo tiene
  # auditoría legado. Los tres son diálogos "Efectivo" ocurridos el 1-2 de junio.
  DBI::dbWriteTable(con, "Registros", data.frame(
    Id = c(1L, 2L, 3L),
    EncuestaId = c(1L, 1L, 1L),
    UsuarioNum = c("101", "102", "103"),
    TipoRegistro = c("Efectivo", "Efectivo", "Efectivo"),
    FechaInicio = c("2026-06-01 18:00:00", "2026-06-02 14:00:00", "2026-06-01 18:00:00"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "EvaluacionRegistro", data.frame(
    RegistroId = c(1L, 3L),
    Resultado = c(
      veredicto_json("Diálogo Deficiente", 1, "legado, debe perder frente al bot"),
      veredicto_json("Diálogo Aceptable", 3, "legado, no tiene equivalente en bot")
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
