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
    Id = c(1L, 2L),
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

test_that("fetch_auditoria_legacy colapsa re-capturas del mismo RegistroId quedándose con la última (Id más alto)", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "EvaluacionRegistro", data.frame(
    Id = c(1L, 2L),
    RegistroId = c(1L, 1L),
    Resultado = c(
      veredicto_json("Diálogo Deficiente", 1, "primera captura"),
      veredicto_json("Diálogo Óptimo", 5, "recaptura, debe ganar")
    ),
    stringsAsFactors = FALSE
  ))
  registros <- tibble::tibble(RegistroId = 1L, usuario_num = "101", fecha = as.Date("2026-06-01"))
  res <- fetch_auditoria_legacy(con, registros)
  expect_equal(nrow(res), 1)
  expect_equal(res$dictamenFinal, "Diálogo Óptimo")
  expect_equal(res$observaciones, "recaptura, debe ganar")
})

# --- Cruce en la base (semi_join) en vez de lista IN: error 8632 de SQL Server ----------

crear_con_bot_y_registros <- function() {
  con <- crear_con_bot()
  # RegistroId 9 tiene auditoría de bot pero NO es diálogo efectivo (Cancelado): no debe entrar.
  DBI::dbExecute(con, "INSERT INTO ResultadoAuditoriaBot VALUES (4, 9, '{}')")
  DBI::dbWriteTable(con, "Registros", data.frame(
    Id = c(1L, 2L, 9L),
    EncuestaId = c(1L, 1L, 1L),
    UsuarioNum = c("101", "102", "109"),
    TipoRegistro = c("Efectivo", "Efectivo", "Cancelado"),
    FechaInicio = c("2026-06-01 18:00:00", "2026-06-02 14:00:00", "2026-06-02 14:00:00"),
    stringsAsFactors = FALSE
  ))
  con
}

test_that("filtrar_por_registros con registros_tbl no genera lista IN (evita el error 8632)", {
  con <- crear_con_bot_y_registros()
  on.exit(DBI::dbDisconnect(con))

  # `registros` local enorme: con IN iría completo en el SQL; con semi_join no debe aparecer.
  registros_grande <- tibble::tibble(RegistroId = seq_len(50000L))
  sql <- dplyr::tbl(con, "ResultadoAuditoriaBot") |>
    filtrar_por_registros(registros_grande, registros_efectivos_tbl(con, 1L)) |>
    dbplyr::sql_render() |>
    as.character()

  expect_false(grepl("49999", sql)) # ningún RegistroId viaja en el SQL
  expect_true(grepl("EXISTS", sql))
})

test_that("fetch_auditoria_bot con registros_tbl da lo mismo que con lista IN y excluye no efectivos", {
  con <- crear_con_bot_y_registros()
  on.exit(DBI::dbDisconnect(con))

  registros <- registros_efectivos(con, 1L)
  con_in  <- fetch_auditoria_bot(con, registros)
  con_sql <- fetch_auditoria_bot(con, registros, registros_efectivos_tbl(con, 1L))

  expect_equal(con_sql, con_in)
  expect_setequal(con_sql$RegistroId, c(1L, 2L))
  expect_equal(con_sql[con_sql$RegistroId == 2L, ]$observaciones, "corregido por humano")
})

test_that("fetch_auditoria_legacy con registros_tbl da lo mismo que con lista IN", {
  con <- crear_con_bot_y_registros()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbWriteTable(con, "EvaluacionRegistro", data.frame(
    Id = c(1L, 2L, 3L),
    RegistroId = c(1L, 2L, 9L),
    Resultado = c(veredicto_json("Diálogo Óptimo", 5), veredicto_json("Diálogo Aceptable", 3),
                  veredicto_json("Diálogo Deficiente", 1)),
    stringsAsFactors = FALSE
  ))

  registros <- registros_efectivos(con, 1L)
  con_in  <- fetch_auditoria_legacy(con, registros)
  con_sql <- fetch_auditoria_legacy(con, registros, registros_efectivos_tbl(con, 1L))

  expect_equal(con_sql, con_in)
  expect_setequal(con_sql$RegistroId, c(1L, 2L))
})
