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

test_that("cargar_insumos respeta cargo_coordinador personalizado", {
  # DB con cargo "Supervisor" en lugar de "Coordinador de Brigada"
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Usuarios", data.frame(
    Id = c(1L, 2L),
    IdProyecto = c(10L, 10L),
    Num = c("001", "002"),
    Cargo = c("Vocero", "Supervisor"),       # ← cargo distinto
    Status = c(TRUE, TRUE),
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
    Id = 100L, NombreBrigada = "BRIGADA NORTE", Activo = TRUE,
    IdZonaDeTrabajo = 1L, IdUsuario = 2L, IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "Municipios", data.frame(Id = 1L, Municipio = "Centro", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "UsuarioLog", data.frame(
    IdHistorico = 1L, IdUsuario = 1L, IdCargo = 1L, IdEstado = 1L,
    IdMunicipio = 1L, IdZonaDeTabajo = 1L, IdSupervisor = 2L, IdBrigada = 100L,
    FechaInsert = "2026-01-15 00:00:00", IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "Actividad", data.frame(
    fecha = "2026-03-20", usuario_num = "001", seccion = "0001",
    desglose = "Efectivo", duracion_minutos = 10, origen = "Actividad",
    stringsAsFactors = FALSE
  ))

  fuentes <- list(list(
    tabla = "Actividad",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  # Sin cargo_coordinador correcto → coordinadores vacío
  res_default <- cargar_insumos(
    pool = con, id_proyecto = 10L, corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes
  )
  expect_equal(nrow(res_default$bd_aux |> dplyr::filter(!is.na(supervisor))), 0L)

  # Con cargo_coordinador = "Supervisor" → coordinador reconocido
  res_custom <- cargar_insumos(
    pool = con, id_proyecto = 10L, corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes, cargo_coordinador = "Supervisor"
  )
  expect_true(any(!is.na(res_custom$bd_aux$supervisor)))
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

# =========================================================================
# TESTS: resolver_brigada_en_fecha
# =========================================================================

# Fixture helpers
make_usuario_log <- function() {
  dplyr::tibble(
    IdHistorico   = c(10L, 20L, 30L, 40L),
    IdUsuario     = c(1L,  1L,  1L,  2L),
    IdCargo       = c(1L,  1L,  NA_integer_, 1L),
    IdEstado      = c(1L,  1L,  1L,  1L),
    IdMunicipio   = c(1L,  1L,  1L,  1L),
    IdZonaDeTabajo = c(1L, 1L,  1L,  1L),
    IdSupervisor  = c(99L, 99L, NA_integer_, 99L),
    IdBrigada     = c(100L, 200L, NA_integer_, 300L),
    FechaInsert   = as.POSIXct(c(
      "2026-01-01 00:00:00",
      "2026-02-15 00:00:00",
      "2026-04-01 00:00:00",
      "2026-01-01 00:00:00"
    ), tz = "UTC"),
    ts_evento     = as.POSIXct(c(
      "2026-01-01 00:00:00",
      "2026-02-15 00:00:00",
      "2026-04-01 00:00:00",
      "2026-01-01 00:00:00"
    ), tz = "America/Mexico_City"),
    fecha_evento  = as.Date(c(
      "2026-01-01",
      "2026-02-15",
      "2026-04-01",
      "2026-01-01"
    ))
  )
}

make_usuarios_cat <- function() {
  dplyr::tibble(
    id_usuario = c(1L, 2L),
    num        = c("001", "002"),
    cargo      = c("Vocero", "Vocero"),
    status     = c(TRUE, TRUE),
    municipio_usuario = c("Centro", "Centro"),
    nombre_completo   = c("JUAN PEREZ", "CARLOS DIAZ"),
    id_brigada        = c(200L, 300L)
  )
}

test_that("resolver_brigada_en_fecha: resolves brigada from first assignment", {
  actividad <- dplyr::tibble(
    usuario_num = "001",
    fecha       = as.Date("2026-01-20")
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 1L)
  expect_equal(result$id_brigada, 100L)
})

test_that("resolver_brigada_en_fecha: resolves brigada after a brigade change", {
  actividad <- dplyr::tibble(
    usuario_num = "001",
    fecha       = as.Date("2026-03-01")
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(result$id_brigada, 200L)
})

test_that("resolver_brigada_en_fecha: fills forward through deactivation (NA brigada row)", {
  # On 2026-04-01 the user has IdBrigada = NA (deactivated).
  # Activity on 2026-04-05 should inherit the last known brigada (200).
  actividad <- dplyr::tibble(
    usuario_num = "001",
    fecha       = as.Date("2026-04-05")
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(result$id_brigada, 200L)
})

test_that("resolver_brigada_en_fecha: activity before first log entry returns NA", {
  actividad <- dplyr::tibble(
    usuario_num = "001",
    fecha       = as.Date("2025-12-31")
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$id_brigada))
})

test_that("resolver_brigada_en_fecha: handles multiple users independently", {
  actividad <- dplyr::tibble(
    usuario_num = c("001", "002"),
    fecha       = as.Date(c("2026-02-01", "2026-02-01"))
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 2L)
  expect_equal(result$id_brigada[result$usuario_num == "001"], 100L)
  expect_equal(result$id_brigada[result$usuario_num == "002"], 300L)
})

test_that("resolver_brigada_en_fecha: replaces existing id_brigada column", {
  actividad <- dplyr::tibble(
    usuario_num = "001",
    fecha       = as.Date("2026-02-01"),
    id_brigada  = 999L  # stale value that should be overwritten
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(result$id_brigada, 100L)
})

test_that("resolver_brigada_en_fecha: empty actividad returns empty data frame", {
  actividad <- dplyr::tibble(
    usuario_num = character(),
    fecha       = as.Date(character())
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 0L)
  expect_true("id_brigada" %in% names(result))
})

test_that("resolver_brigada_en_fecha: multiple log entries on same date do not duplicate activity rows", {
  # Two log events on 2026-02-15 for the same user (e.g. status change + brigade
  # change recorded on the same day). This is the exact scenario that caused row
  # multiplication before the slice_tail(n=1) dedup was added.
  log_same_day <- dplyr::tibble(
    IdHistorico  = c(10L, 11L, 20L),
    IdUsuario    = c(1L,  1L,  1L),
    IdCargo      = c(1L,  1L,  1L),
    IdEstado     = c(1L,  1L,  1L),
    IdMunicipio  = c(1L,  1L,  1L),
    IdZonaDeTabajo = c(1L, 1L, 1L),
    IdSupervisor = c(99L, 99L, 99L),
    IdBrigada    = c(100L, 150L, 200L),
    FechaInsert  = as.POSIXct(c(
      "2026-01-01 08:00:00",
      "2026-01-01 09:00:00",  # same calendar date as row 1
      "2026-02-15 00:00:00"
    ), tz = "UTC"),
    ts_evento    = as.POSIXct(c(
      "2026-01-01 08:00:00",
      "2026-01-01 09:00:00",
      "2026-02-15 00:00:00"
    ), tz = "America/Mexico_City"),
    fecha_evento = as.Date(c("2026-01-01", "2026-01-01", "2026-02-15"))
  )

  actividad <- dplyr::tibble(
    usuario_num = c("001", "001", "001"),
    fecha       = as.Date(c("2026-01-15", "2026-01-15", "2026-03-01"))
  )

  result <- resolver_brigada_en_fecha(actividad, log_same_day, make_usuarios_cat())

  # Must not expand: output rows == input rows
  expect_equal(nrow(result), nrow(actividad))
  # On 2026-01-15: last entry on 2026-01-01 is IdHistorico=11 → IdBrigada=150
  expect_equal(result$id_brigada[result$fecha == as.Date("2026-01-15")], c(150L, 150L))
  # On 2026-03-01: last entry overall is IdHistorico=20 → IdBrigada=200
  expect_equal(result$id_brigada[result$fecha == as.Date("2026-03-01")], 200L)
})

test_that("resolver_brigada_en_fecha: output row count always equals input row count", {
  # Property-style check: regardless of log density, nrow(result) == nrow(actividad)
  actividad <- dplyr::tibble(
    usuario_num = rep(c("001", "002"), each = 5),
    fecha       = rep(seq(as.Date("2026-01-01"), by = "month", length.out = 5), 2)
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), nrow(actividad))
})

test_that("resolver_brigada_en_fecha: num_map amplio resuelve usuario ausente de usuarios_cat", {
  # Escenario: usuario "003" (id=3) tiene actividad y aparece en usuario_log,
  # pero NO está en usuarios_cat (ej. Activo=FALSE en UsuariosEncuesta).
  # Sin num_map → id_brigada=NA. Con num_map que incluye id=3 → brigada resuelta.
  log_con_inactivo <- dplyr::bind_rows(
    make_usuario_log(),
    dplyr::tibble(
      IdHistorico    = 50L,
      IdUsuario      = 3L,
      IdCargo        = 1L,
      IdEstado       = 1L,
      IdMunicipio    = 1L,
      IdZonaDeTabajo = 1L,
      IdSupervisor   = 99L,
      IdBrigada      = 400L,
      FechaInsert    = as.POSIXct("2026-01-10 00:00:00", tz = "UTC"),
      ts_evento      = as.POSIXct("2026-01-10 00:00:00", tz = "America/Mexico_City"),
      fecha_evento   = as.Date("2026-01-10")
    )
  )

  actividad <- dplyr::tibble(
    usuario_num = "003",
    fecha       = as.Date("2026-02-01")
  )

  # Sin num_map: usuario no está en usuarios_cat → NA
  result_sin <- resolver_brigada_en_fecha(actividad, log_con_inactivo, make_usuarios_cat())
  expect_true(is.na(result_sin$id_brigada))

  # Con num_map amplio que incluye id=3 → brigada resuelta
  num_map_amplio <- dplyr::tibble(
    id_usuario = c(1L, 2L, 3L),
    num        = c("001", "002", "003")
  )
  result_con <- resolver_brigada_en_fecha(
    actividad, log_con_inactivo, make_usuarios_cat(),
    num_map = num_map_amplio
  )
  expect_equal(result_con$id_brigada, 400L)
})

# =========================================================================
# TESTS: construir_bd_aux — validación relación coordinador–brigada
# =========================================================================

make_ec <- function(id_brigada_vocero, id_brigada_coord) {
  dplyr::tibble(
    id_usuario   = c(1L, 2L),
    id_cargo     = c(1L, 2L),
    id_estado    = c(1L, 1L),
    id_municipio = c(1L, 1L),
    id_zona      = c(1L, 1L),
    id_supervisor = c(2L, NA_integer_),
    id_brigada   = c(id_brigada_vocero, id_brigada_coord),
    ts_evento    = as.POSIXct(c("2026-01-15", "2026-01-15"), tz = "UTC"),
    fecha_evento = as.Date(c("2026-01-15", "2026-01-15"))
  )
}

ucat_bd <- dplyr::tibble(
  id_usuario       = c(1L, 2L),
  num              = c("001", "002"),
  cargo            = c("Vocero", "Coordinador de Brigada"),
  status           = c(TRUE, TRUE),
  municipio_usuario = c("Centro", "Centro"),
  nombre_completo  = c("JUAN PEREZ", "CARLOS SOTO"),
  id_brigada       = c(100L, 200L)
)

bcat_bd <- dplyr::tibble(
  id_brigada            = c(100L, 200L),
  nombre_brigada        = c("BRIGADA NORTE", "BRIGADA SUR"),
  activo_brigada        = c(TRUE, TRUE),
  id_zona_trabajo_brigada = c(1L, 1L),
  id_usuario_brigada    = c(2L, NA_integer_)
)

ccat_bd <- dplyr::tibble(
  id_supervisor      = 2L,
  supervisor         = "002",
  nombre_coordinador = "CARLOS SOTO",
  status_coord       = TRUE
)

mcat_bd <- dplyr::tibble(id_municipio = 1L, municipio_log = "Centro")

test_that("construir_bd_aux: produce 1 fila por usuario de estructura_corte", {
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  result <- construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_bd, mcat_bd)
  expect_equal(nrow(result), 2L)
})

test_that("construir_bd_aux: vocero queda vinculado a su brigada y coordinador", {
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  result <- construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_bd, mcat_bd)
  vocero_row <- result[result$vocero == "001", ]
  expect_equal(vocero_row$nombre_brigada, "BRIGADA NORTE")
  expect_equal(vocero_row$nombre_coordinador, "CARLOS SOTO")
})

test_that("construir_bd_aux: usuario sin match en usuarios_cat tiene vocero NA", {
  ec_desconocido <- dplyr::tibble(
    id_usuario    = c(99L, 2L),
    id_cargo      = c(1L, 2L),
    id_estado     = c(1L, 1L),
    id_municipio  = c(1L, 1L),
    id_zona       = c(1L, 1L),
    id_supervisor = c(2L, NA_integer_),
    id_brigada    = c(200L, 200L),  # misma brigada que coordinador → no espuria
    ts_evento     = as.POSIXct(c("2026-01-15", "2026-01-15"), tz = "UTC"),
    fecha_evento  = as.Date(c("2026-01-15", "2026-01-15"))
  )
  result <- construir_bd_aux(ec_desconocido, ucat_bd, ccat_bd, bcat_bd, mcat_bd)
  expect_equal(nrow(result), 2L)
  expect_true(any(is.na(result$vocero)))
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
