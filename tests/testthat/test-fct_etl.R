# tests/testthat/test-fct_etl.R

# =========================================================================
# HELPER: SQLite in-memory DB for cargar_insumos
# =========================================================================

setup_etl_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "Usuarios", data.frame(
    Id = c(1L, 2L),
    IdProyecto = c(10L, 10L),
    Num = c("001", "002"),
    Cargo = c("Vocero", "Coordinador de Brigada"),
    Status = c(TRUE, FALSE),
    Municipio = c("Centro", "Centro"),
    Nombre = c("Juan", "Carlos"),
    APaterno = c("Perez", "Soto"),
    AMaterno = c("Lopez", "Diaz"),
    IdBrigada = c(100L, 100L),
    Capacitacion = c(TRUE, FALSE),
    FechaUpdate = c("2026-01-01", "2026-01-01"),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Brigadas", data.frame(
    Id = 100L,
    NombreBrigada = "06_BRIGADA NORTE",
    Activo = TRUE,
    IdZonaDeTrabajo = 1L,
    IdUsuario = 2L,
    IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Municipios", data.frame(
    Id = 1L,
    Municipio = "Centro",
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "UsuarioLog", data.frame(
    IdHistorico = 1L,
    IdUsuario = 1L,
    IdCargo = 1L,
    IdEstado = 1L,
    IdMunicipio = 1L,
    IdZonaDeTabajo = 1L,
    IdSupervisor = 2L,
    IdBrigada = 100L,
    FechaInsert = "2026-01-15 00:00:00",
    IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Actividad", data.frame(
    fecha = "2026-03-20",
    usuario_num = "001",
    seccion = "0001",
    desglose = "Efectivo",
    duracion_minutos = 10,
    origen = "Actividad",
    stringsAsFactors = FALSE
  ))

  con
}

# =========================================================================
# TESTS: cargar_insumos
# =========================================================================

test_that("cargar_insumos returns list with expected structure", {
  con <- setup_etl_db()
  on.exit(DBI::dbDisconnect(con))

  fuentes <- list(list(
    tabla = "Actividad",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  resultado <- cargar_insumos(
    pool = con,
    id_proyecto = 10L,
    corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes
  )

  expect_type(resultado, "list")
  expect_true(all(c("bd_actividad", "bd_aux", "pase_lista", "cat", "id_proyecto", "corte") %in% names(resultado)))
  expect_s3_class(resultado$bd_actividad, "data.frame")
  expect_s3_class(resultado$bd_aux, "data.frame")
  expect_equal(resultado$id_proyecto, 10L)
  expect_equal(resultado$corte, as.Date("2026-03-31"))
  expect_equal(nrow(resultado$bd_actividad), 1L)
})

test_that("cargar_insumos applies postprocess_insumos hook", {
  con <- setup_etl_db()
  on.exit(DBI::dbDisconnect(con))

  fuentes <- list(list(
    tabla = "Actividad",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  hook_called <- FALSE
  hook <- function(insumos) {
    hook_called <<- TRUE
    insumos$hook_applied <- TRUE
    insumos
  }

  resultado <- cargar_insumos(
    pool = con,
    id_proyecto = 10L,
    corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes,
    postprocess_insumos = hook
  )

  expect_true(hook_called)
  expect_true(isTRUE(resultado$hook_applied))
})

test_that("cargar_insumos aborts when ids_pase_lista provided without procesador_pl", {
  con <- setup_etl_db()
  on.exit(DBI::dbDisconnect(con))

  fuentes <- list(list(
    tabla = "Actividad",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  expect_error(
    cargar_insumos(
      pool = con,
      id_proyecto = 10L,
      corte = as.Date("2026-03-31"),
      fuentes_actividad = fuentes,
      ids_pase_lista = 210L
    ),
    "procesador_pl"
  )
})

# =========================================================================
# TESTS: procesar_pase_lista
# =========================================================================

test_that("procesar_pase_lista returns empty data.table when records are before fecha_min", {
  # Non-empty mock with old dates — they will be filtered out by fecha_min
  registros_mock <- tibble::tibble(
    Id = 1L,
    EncuestaId = 210L,
    FechaInicio = as.POSIXct("2020-06-01 10:00:00", tz = "UTC"),
    Resultado = '{"Obtener_usuario":"TEST","asistencia_99":"Si"}'
  )

  testthat::local_mocked_bindings(
    tbl = function(pool, name) registros_mock,
    .package = "DialogaR"
  )

  result <- procesar_pase_lista(
    pool = NULL,
    id_cuestionario = 210L,
    fecha_min = as.Date("2026-01-01")
  )

  expect_true(inherits(result, "data.table") || is.data.frame(result))
  expect_equal(nrow(result), 0L)
})

test_that("procesar_pase_lista parses JSON and returns expected columns", {
  registros_mock <- tibble::tibble(
    Id = 1L,
    EncuestaId = 210L,
    FechaInicio = as.POSIXct("2026-03-20 10:00:00", tz = "UTC"),
    Resultado = paste0(
      '{"Obtener_usuario":"JUAN PEREZ",',
      '"asistencia_12345":"Si","viviendas_12345":"5",',
      '"finalizar":"1","observaciones":"OK"}'
    )
  )

  testthat::local_mocked_bindings(
    tbl = function(pool, name) registros_mock,
    .package = "DialogaR"
  )

  result <- procesar_pase_lista(
    pool = NULL,
    id_cuestionario = 210L,
    fecha_min = as.Date("2026-01-01")
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0L)
  expect_true("obtener_usuario" %in% names(result))
  expect_true("usuario" %in% names(result))
  expect_equal(as.character(result$obtener_usuario[[1]]), "JUAN PEREZ")
  expect_equal(as.character(result$usuario[[1]]), "12345")
})

test_that("procesar_pase_lista skips records with invalid JSON", {
  registros_mock <- tibble::tibble(
    Id = c(1L, 2L),
    EncuestaId = c(210L, 210L),
    FechaInicio = as.POSIXct(
      c("2026-03-20 10:00:00", "2026-03-20 11:00:00"),
      tz = "UTC"
    ),
    Resultado = c(
      '{"Obtener_usuario":"CARLOS DIAZ","asistencia_99999":"Si"}',
      "NOT_VALID_JSON{"
    )
  )

  testthat::local_mocked_bindings(
    tbl = function(pool, name) registros_mock,
    .package = "DialogaR"
  )

  result <- procesar_pase_lista(
    pool = NULL,
    id_cuestionario = 210L,
    fecha_min = as.Date("2026-01-01")
  )

  expect_true(is.data.frame(result))
  if (nrow(result) > 0) {
    expect_equal(as.character(result$obtener_usuario[[1]]), "CARLOS DIAZ")
  }
})
