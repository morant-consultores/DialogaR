# tests/testthat/test-fct_capacitacion.R

# =========================================================================
# HELPER: Setup In-Memory Database for Testing
# =========================================================================
# This runs once before the tests and creates a fake SQLite database
setup_mock_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Mock "Usuarios" table
  mock_usuarios <- tibble::tibble(
    Id = 1:3,
    IdProyecto = c(10, 10, 20), # ID 3 belongs to another project
    Num = c("001", "002", "003"), # "002" will be our test exclusion
    Nombre = c("Juan", "Maria", "Pedro"),
    APaterno = c("Perez", "Gomez", "Ruiz"),
    AMaterno = c("Lopez", "Diaz", "Soto"),
    Municipio = "Centro",
    Cargo = "Vocero",
    Status = TRUE,
    FechaInsert = "2026-03-20",
    IdBrigada = c(100, 100, 200),
    Entrevista = "Aprobada",
    Resolucion = "Aprobada",
    Capacitacion = "Pendiente"
  )
  DBI::dbWriteTable(con, "Usuarios", mock_usuarios)

  # Mock "Brigadas" table
  mock_brigadas <- tibble::tibble(
    Id = c(100, 200),
    IdProyecto = c(10, 20),
    NombreBrigada = c("Brigada Norte", "Brigada Sur")
  )
  DBI::dbWriteTable(con, "Brigadas", mock_brigadas)

  # Mock "RegistrosCuestionarios" table
  mock_respuestas <- tibble::tibble(
    IdUsuario = c(1, 1),
    IdProyecto = c(10, 10),
    JsonData = c(
      '{"P1": "Si", "P2": 10}',
      '{"P1": "No", "P2": 5}'
    )
  )
  DBI::dbWriteTable(con, "RegistrosCuestionarios", mock_respuestas)

  return(con)
}

# =========================================================================
# TESTS: crear_tabla_capacitacion
# =========================================================================

test_that("crear_tabla_capacitacion filtra proyectos y números de prueba correctamente", {
  con <- setup_mock_db()
  on.exit(DBI::dbDisconnect(con)) # Ensure DB is closed when test finishes

  # Mock parameters
  brigadas_mock <- dplyr::tbl(con, "Brigadas") |> dplyr::collect()

  # Execute
  resultado <- crear_tabla_capacitacion(
    pool = con,
    id_proyecto = 10,
    numeros_prueba = c("002"), # Exclude Maria
    brigadas = brigadas_mock
  )

  # Assertions
  expect_equal(nrow(resultado), 1) # Only Juan should remain (Maria excluded, Pedro wrong project)
  expect_equal(resultado$nombre[1], "Juan Perez Lopez")
  expect_true("NombreBrigada" %in% names(resultado))
})

# =========================================================================
# TESTS: obtener_respuestas_capacitacion
# =========================================================================

test_that("obtener_respuestas_capacitacion expande JSON correctamente", {
  con <- setup_mock_db()
  on.exit(DBI::dbDisconnect(con))

  # Execute
  resultado <- obtener_respuestas_capacitacion(
    pool = con,
    ids = c(1),
    id_proyecto = 10
  )

  # Assertions
  expect_equal(nrow(resultado), 2) # Two JSON records for Id 1
  expect_true("P1" %in% names(resultado))
  expect_true("P2" %in% names(resultado))
  expect_equal(resultado$Id[1], 1)
})

# =========================================================================
# TESTS: generar_reporte_capacitacion (Orchestrator)
# =========================================================================

test_that("generar_reporte_capacitacion detiene la ejecución si Drive falla", {
  con <- setup_mock_db()
  on.exit(DBI::dbDisconnect(con))

  # MOCK: Force drive_get to fail to test the tryCatch block
  testthat::local_mocked_bindings(
    drive_get = function(...) stop("API Error Simulated"),
    .package = "googledrive"
  )

  # The orchestrator should catch the error and stop gracefully
  expect_error(
    generar_reporte_capacitacion(
      pool = con,
      id_proyecto = 10,
      numeros_prueba = NULL,
      drive_folder = "fake_id"
    ),
    "No pude acceder a `drive_folder`"
  )
})

test_that("generar_reporte_capacitacion ejecuta el flujo completo y sube archivo", {
  con <- setup_mock_db()
  on.exit(DBI::dbDisconnect(con))

  # MOCK: Pretend Drive works perfectly and intercept subida
  testthat::local_mocked_bindings(
    drive_get = function(...) return(TRUE),
    as_id = function(x) return(x),
    .package = "googledrive"
  )

  testthat::local_mocked_bindings(
    subida = function(carpeta_drive, objeto, nombre, corte) {
      expect_true(is.data.frame(objeto)) # Ensure it's sending the final joined table
      expect_equal(nombre, "capacitacion") # Ensure prefix was cleaned
      return(data.frame(name = paste0(nombre, "_", corte, ".xlsx")))
    },
    .package = "DialogaR" # Change this if your package is named differently!
  )

  # Execute full orchestrator
  resultado <- generar_reporte_capacitacion(
    pool = con,
    id_proyecto = 10,
    numeros_prueba = c("002"),
    drive_folder = "fake_folder",
    corte = as.Date("2026-03-27"),
    verbose = FALSE # Turn off messages for tests
  )

  # Assertions
  expect_type(resultado, "list")
  expect_true("tabla_final" %in% names(resultado))
  expect_equal(nrow(resultado$tabla_final), 2) # 1 User x 2 JSON Responses
})
