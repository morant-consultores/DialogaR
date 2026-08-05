# tests/testthat/test-fct_reporte_auditoria_rango_manual.R

crear_con_legacy <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # 'fecha' para el reporte se deriva de Registros.FechaInicio (cuándo ocurrió el diálogo),
  # no de EvaluacionRegistro.Fecha (cuándo se auditó) — ver registros_efectivos().
  # Id = 1 cae fuera del rango manual (24 de julio); Id = 2 y 3 caen dentro (25 y 31 de julio).
  DBI::dbWriteTable(con, "Registros", data.frame(
    Id = c(1L, 2L, 3L),
    EncuestaId = c(1L, 1L, 1L),
    UsuarioNum = c("001", "001", "001"),
    TipoRegistro = c("Efectivo", "Efectivo", "Efectivo"),
    FechaInicio = as.POSIXct(c("2026-07-24 18:00:00", "2026-07-25 18:00:00", "2026-07-31 18:00:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  ))

  veredicto <- function(dictamen, total) {
    as.character(jsonlite::toJSON(list(
      dictamenFinal = dictamen, totalEvaluacion = as.character(total), observaciones = "ok"
    ), auto_unbox = TRUE))
  }

  DBI::dbWriteTable(con, "EvaluacionRegistro", data.frame(
    RegistroId = c(1L, 2L, 3L),
    Resultado = c(veredicto("Diálogo Óptimo", 5), veredicto("Diálogo Óptimo", 5), veredicto("Diálogo Aceptable", 3)),
    stringsAsFactors = FALSE
  ))

  con
}

make_bd_aux <- function() {
  tibble::tibble(
    distrito = "01", municipio = "Centro", nombre_brigada = "BRIGADA 1",
    nombre_coordinador = "COORD 1", supervisor = "002", status_coord = TRUE,
    nombre_vocero = "VOCERO 1", vocero = "001", status_vocero = TRUE
  )
}

make_insumos <- function() {
  list(cat = list(usuarios = tibble::tibble(num = "002", cargo = "Coordinador de Brigada")))
}

make_bd_completa <- function() {
  tibble::tibble(usuario_num = "001", fecha = as.Date("2026-07-25"), desglose = "Efectivo")
}

test_that("fecha_inicio_auditoria/fecha_fin_auditoria reemplazan el rango calculado desde corte", {
  con <- crear_con_legacy()
  on.exit(DBI::dbDisconnect(con))

  resultado <- generar_reporte_metricas(
    pool = con,
    insumos = make_insumos(),
    bd_completa = make_bd_completa(),
    bd_aux = make_bd_aux(),
    encuesta_id = 1L,
    corte = "2026-07-31",
    fecha_inicio_auditoria = as.Date("2026-07-25"),
    fecha_fin_auditoria = as.Date("2026-07-31")
  )

  # Solo RegistroId 2 y 3 (25 y 31 de julio) deben entrar al cálculo semanal;
  # RegistroId 1 (24 de julio) queda fuera del rango manual.
  expect_equal(resultado$res_auditoria$dialogos_auditados[resultado$res_auditoria$vocero == "001"], 2)
})

test_that("indicar solo uno de los dos límites del rango manual de auditoría produce un error", {
  con <- crear_con_legacy()
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    generar_reporte_metricas(
      pool = con,
      insumos = make_insumos(),
      bd_completa = make_bd_completa(),
      bd_aux = make_bd_aux(),
      encuesta_id = 1L,
      corte = "2026-07-31",
      fecha_inicio_auditoria = as.Date("2026-07-25")
    ),
    "deben indicarse juntos"
  )
})

test_that("por defecto la ventana de efectivos es la misma semana que la de auditoría", {
  con <- crear_con_legacy()
  on.exit(DBI::dbDisconnect(con))

  # bd_completa solo tiene un registro efectivo el 25 de julio, dentro del rango de
  # auditoría manual; si la ventana de efectivos coincide (comportamiento por defecto),
  # ese registro debe contarse en 'efectivos'.
  resultado <- generar_reporte_metricas(
    pool = con,
    insumos = make_insumos(),
    bd_completa = make_bd_completa(),
    bd_aux = make_bd_aux(),
    encuesta_id = 1L,
    corte = "2026-07-31",
    fecha_inicio_auditoria = as.Date("2026-07-25"),
    fecha_fin_auditoria = as.Date("2026-07-31")
  )

  expect_equal(resultado$res_auditoria$efectivos[resultado$res_auditoria$vocero == "001"], 1)
})

test_that("fecha_inicio_efectivos/fecha_fin_efectivos fijan una ventana de producción independiente", {
  con <- crear_con_legacy()
  on.exit(DBI::dbDisconnect(con))

  # Se pide una ventana de efectivos que NO incluye el 25 de julio (único registro en
  # bd_completa): el conteo de efectivos debe caer a 0 aunque la auditoría siga siendo 25-31.
  resultado <- generar_reporte_metricas(
    pool = con,
    insumos = make_insumos(),
    bd_completa = make_bd_completa(),
    bd_aux = make_bd_aux(),
    encuesta_id = 1L,
    corte = "2026-07-31",
    fecha_inicio_auditoria = as.Date("2026-07-25"),
    fecha_fin_auditoria = as.Date("2026-07-31"),
    fecha_inicio_efectivos = as.Date("2026-07-13"),
    fecha_fin_efectivos = as.Date("2026-07-19")
  )

  expect_equal(resultado$res_auditoria$efectivos[resultado$res_auditoria$vocero == "001"], 0)
})

test_that("indicar solo uno de los dos límites del rango manual de efectivos produce un error", {
  con <- crear_con_legacy()
  on.exit(DBI::dbDisconnect(con))

  expect_error(
    generar_reporte_metricas(
      pool = con,
      insumos = make_insumos(),
      bd_completa = make_bd_completa(),
      bd_aux = make_bd_aux(),
      encuesta_id = 1L,
      corte = "2026-07-31",
      fecha_inicio_efectivos = as.Date("2026-07-13")
    ),
    "deben indicarse juntos"
  )
})
