# tests/testthat/test-fct_reporte_auditoria.R

test_that("crear_workbook_auditoria handles valid inputs and creates workbook", {
  # Setup mock data reflecting our required structure
  mock_datos <- list(
    res_auditoria = tibble::tibble(
      nombre_brigada = "BRIGADA 1",
      nombre_completo = "JUAN PEREZ",
      usuario_num = "101",
      status = TRUE,
      `Promedio de evaluaciones` = 25.5,
      dialogos_auditados = 5,
      eliminados = 0,
      efectivos = 20,
      fecha_ultimo_registro = as.Date("2026-03-26")
    ),
    observaciones = tibble::tibble(
      nombre_brigada = "BRIGADA 1",
      nombre_completo = "JUAN PEREZ",
      usuario_num = "101",
      status = TRUE,
      RegistroId = 999,
      fecha = as.Date("2026-03-26"),
      observaciones = "Todo bien",
      dictamenFinal = "Aprobada"
    )
  )

  # Execute
  wb <- crear_workbook_auditoria(mock_datos)

  # Assertions
  # Assertions
  expect_true(inherits(wb, "Workbook")) # Changed this line
  expect_true("res_auditoria" %in% names(wb))
  expect_true("observaciones" %in% names(wb))
})

test_that("crear_workbook_auditoria fails-fast if structure is wrong", {
  mock_bad_data <- list(
    tabla_equivocada = tibble::tibble(x = 1)
  )

  # Expect an error to be thrown by our cli::cli_abort
  expect_error(crear_workbook_auditoria(mock_bad_data))
})
