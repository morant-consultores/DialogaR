# tests/testthat/test-fct_secciones.R

test_that("meta_usuario_condicional returns NA when meta is missing", {
  # 1. Setup mock data (Synthetic data, strictly ISO compliant)
  mock_actividad <- tibble::tibble(
    seccion = c("0001", "0002", "0002"),
    fecha = as.Date("2026-03-26"),
    desglose = c("Efectivo", "Efectivo", "No Efectivo")
  )

  mock_metas <- tibble::tibble(
    seccion = c("0001", "0003"),
    meta = c(10, 5)
  ) # Notice "0002" is missing from the metas catalog

  # 2. Execute the function
  resultado <- meta_usuario_condicional(
    bd_actividad = mock_actividad,
    metas = mock_metas,
    corte = as.Date("2026-03-26")
  )

  # 3. Assertions (The actual tests)
  # Section 0001 should have a 10% advance (1 effective / 10 meta)
  avance_0001 <- resultado$`AVANCE META`[resultado$SECCION == "0001"] # Added backticks
  expect_equal(avance_0001, 0.1)

  # Section 0002 should be NA because it's not in the mock_metas
  avance_0002 <- resultado$`AVANCE META`[resultado$SECCION == "0002"] # Added backticks
  expect_true(is.na(avance_0002))
})
