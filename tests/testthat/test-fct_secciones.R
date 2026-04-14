# tests/testthat/test-fct_secciones.R

test_that("meta_usuario_condicional calcula avance correctamente sin coordinadores", {
  mock_actividad <- tibble::tibble(
    seccion = c("0001", "0002", "0002"),
    fecha = as.Date("2026-03-26"),
    desglose = c("Efectivo", "Efectivo", "No Efectivo")
  )

  mock_metas <- tibble::tibble(
    seccion = c("0001", "0003"),
    meta = c(10, 5)
  )

  resultado <- meta_usuario_condicional(
    bd_actividad = mock_actividad,
    metas = mock_metas,
    corte = as.Date("2026-03-26")
  )

  avance_0001 <- resultado$`AVANCE META`[resultado$SECCION == "0001"]
  expect_equal(avance_0001, 0.1)

  avance_0002 <- resultado$`AVANCE META`[resultado$SECCION == "0002"]
  expect_equal(avance_0002, 0)
})

test_that("meta_usuario_condicional no pierde secciones de bd_actividad", {
  mock_actividad <- tibble::tibble(
    seccion = c("0001", "0002", "0003"),
    fecha = as.Date("2026-03-26"),
    desglose = "Efectivo"
  )

  mock_metas <- tibble::tibble(
    seccion = "0001",
    meta = 10
  )

  resultado <- meta_usuario_condicional(
    bd_actividad = mock_actividad,
    metas = mock_metas,
    corte = as.Date("2026-03-26")
  )

  expect_equal(nrow(resultado), 3)
  expect_setequal(resultado$SECCION, c("0001", "0002", "0003"))
})

test_that("meta_usuario_condicional no produce NA en AVANCE META", {
  mock_actividad <- tibble::tibble(
    seccion = c("0001", "0002", "0003"),
    fecha = as.Date("2026-03-26"),
    desglose = c("Efectivo", "Efectivo", "No Efectivo")
  )

  mock_metas <- tibble::tibble(
    seccion = c("0001", "0002"),
    meta = c(10, 0)
  )

  resultado <- meta_usuario_condicional(
    bd_actividad = mock_actividad,
    metas = mock_metas,
    corte = as.Date("2026-03-26")
  )

  expect_false(any(is.na(resultado$`AVANCE META`)))
})

test_that("meta_usuario_condicional agrupa por coordinador cuando se provee base_coordinadores", {
  mock_actividad <- tibble::tibble(
    seccion    = c("0001", "0001", "0002"),
    fecha      = as.Date("2026-03-26"),
    desglose   = c("Efectivo", "No Efectivo", "Efectivo"),
    usuario_num = c(1L, 1L, 2L)
  )

  mock_coordinadores <- tibble::tibble(
    vocero             = c(1L, 2L),
    nombre_coordinador = c("Coord A", "Coord B"),
    nombre_brigada     = c("Brigada 1", "Brigada 2")
  )

  mock_metas <- tibble::tibble(
    seccion = c("0001", "0002"),
    meta    = c(10, 5)
  )

  resultado <- meta_usuario_condicional(
    bd_actividad       = mock_actividad,
    metas              = mock_metas,
    corte              = as.Date("2026-03-26"),
    base_coordinadores = mock_coordinadores
  )

  expect_true("NOMBRE COORDINADOR" %in% names(resultado))
  expect_true("NOMBRE BRIGADA" %in% names(resultado))
  expect_equal(nrow(resultado), 2)
  expect_equal(
    resultado$`AVANCE META`[resultado$SECCION == "0001"],
    0.1
  )
  expect_equal(
    resultado$`AVANCE META`[resultado$SECCION == "0002"],
    0.2
  )
})
