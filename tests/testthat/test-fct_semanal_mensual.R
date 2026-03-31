# tests/testthat/test-fct_semanal_mensual.R

# =========================================================================
# HELPER: build minimal fixture data
# =========================================================================

make_bd_actividad <- function(corte, usuario = "001", n_efectivos = 5) {
  tibble::tibble(
    fecha = corte,
    usuario_num = usuario,
    seccion = "0001",
    desglose = c(rep("Efectivo", n_efectivos), "No abrieron"),
    duracion_minutos = c(rep(10, n_efectivos), 2),
    fecha_inicio = as.POSIXct(paste(corte, "08:00:00")),
    fecha_fin   = as.POSIXct(paste(corte, "14:00:00"))
  )
}

make_bd_aux <- function() {
  tibble::tibble(
    distrito         = "06",
    municipio        = "Centro",
    nombre_brigada   = "06_BRIGADA NORTE",
    nombre_coordinador = "CARLOS SOTO",
    supervisor       = "002",
    status_coord     = TRUE,
    nombre_vocero    = "JUAN PEREZ",
    vocero           = "001",
    status_vocero    = TRUE
  )
}

make_pl <- function(corte, supervisor = "002", vocero = "001") {
  tibble::tibble(
    obtener_usuario = supervisor,
    usuario         = vocero,
    fecha           = corte,
    asistencia      = "Si",
    observaciones   = NA_character_
  )
}

# =========================================================================
# TESTS: construir_productividad_diaria
# =========================================================================

test_that("construir_productividad_diaria returns list with 9 expected elements", {
  corte <- as.Date("2026-03-25")

  resultado <- construir_productividad_diaria(
    bd_actividad = make_bd_actividad(corte),
    bd_aux       = make_bd_aux(),
    pl           = make_pl(corte),
    corte        = corte
  )

  expect_type(resultado, "list")
  expected_names <- c(
    "registros", "pl", "bd_prod_voc", "bd_prod_coord",
    "bd_prod", "tabla_acumulada", "ft_resumen",
    "resumen_detalle", "ft_detalle"
  )
  expect_true(all(expected_names %in% names(resultado)))
})

test_that("construir_productividad_diaria produces flextable objects", {
  corte <- as.Date("2026-03-25")

  resultado <- construir_productividad_diaria(
    bd_actividad = make_bd_actividad(corte),
    bd_aux       = make_bd_aux(),
    pl           = make_pl(corte),
    corte        = corte
  )

  expect_s3_class(resultado$ft_resumen, "flextable")
  expect_s3_class(resultado$ft_detalle, "flextable")
})

test_that("construir_productividad_diaria assigns KPI color verde when promedio >= 14.5", {
  corte <- as.Date("2026-03-25")
  # 15 efectivos / 1 usuario = promedio 15 → verde
  bd_act <- make_bd_actividad(corte, n_efectivos = 15)

  resultado <- construir_productividad_diaria(
    bd_actividad = bd_act,
    bd_aux       = make_bd_aux(),
    pl           = make_pl(corte),
    corte        = corte
  )

  tabla <- resultado$tabla_acumulada
  expect_true("color" %in% names(tabla))
  # General row (first) should be verde
  expect_equal(tabla$color[tabla$nivel == "General"], "verde")
})

test_that("construir_productividad_diaria assigns KPI color naranja when promedio < 10", {
  corte <- as.Date("2026-03-25")
  # 3 efectivos / 1 usuario = promedio 3 → naranja
  bd_act <- make_bd_actividad(corte, n_efectivos = 3)

  resultado <- construir_productividad_diaria(
    bd_actividad = bd_act,
    bd_aux       = make_bd_aux(),
    pl           = make_pl(corte),
    corte        = corte
  )

  tabla <- resultado$tabla_acumulada
  expect_equal(tabla$color[tabla$nivel == "General"], "naranja")
})

test_that("construir_productividad_diaria tabla_acumulada has required columns", {
  corte <- as.Date("2026-03-25")

  resultado <- construir_productividad_diaria(
    bd_actividad = make_bd_actividad(corte),
    bd_aux       = make_bd_aux(),
    pl           = make_pl(corte),
    corte        = corte
  )

  tabla <- resultado$tabla_acumulada
  expect_true(all(c("nivel", "nombre", "usuarios", "dialogos", "promedio", "color") %in% names(tabla)))
  expect_true("General" %in% tabla$nivel)
})
