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

  DBI::dbWriteTable(con, "ZonaDeTrabajo", data.frame(
    IdZona = 1L,
    Descripcion = "6B",
    IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Grupos", data.frame(
    Id = 1L,
    Descripcion = "Cots",
    IdProyecto = 10L,
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

  DBI::dbWriteTable(con, "BrigadasLog", data.frame(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("06_BRIGADA NORTE", "06_BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, 2L),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = c("2026-01-01 00:00:00", "2026-01-15 00:00:00"),
    IdProyecto      = c(10L, 10L),
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

  DBI::dbWriteTable(con, "UsuariosEncuesta", data.frame(
    UsuarioId  = c(1L, 2L),
    EncuestaId = c(1L, 1L),
    Activo     = c(TRUE, TRUE),
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

test_that("cargar_insumos no excluye de bd_aux a usuarios con actividad real pero sin UsuariosEncuesta", {
  # Reproduce el caso real: un vocero (Ana) fue dado de alta en Usuarios/UsuarioLog
  # y registró actividad real, pero nunca se creó su fila en UsuariosEncuesta.
  # Un segundo usuario (Beto) tampoco tiene UsuariosEncuesta, pero NO tiene
  # actividad real: debe seguir excluido (protege el filtro original contra
  # usuarios de otras encuestas del mismo proyecto).
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Usuarios", data.frame(
    Id = c(1L, 2L, 3L, 4L),
    IdProyecto = c(10L, 10L, 10L, 10L),
    Num = c("001", "002", "003", "004"),
    Cargo = c("Vocero", "Coordinador de Brigada", "Vocero", "Vocero"),
    Status = c(TRUE, TRUE, TRUE, TRUE),
    Municipio = c("Centro", "Centro", "Centro", "Centro"),
    Nombre = c("Juan", "Carlos", "Ana", "Beto"),
    APaterno = c("Perez", "Soto", "Ruiz", "Diaz"),
    AMaterno = c("Lopez", "Diaz", "Cruz", "Ortiz"),
    IdBrigada = c(100L, 100L, 100L, 100L),
    Capacitacion = c(TRUE, FALSE, TRUE, TRUE),
    FechaUpdate = rep("2026-01-01", 4),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Brigadas", data.frame(
    Id = 100L, NombreBrigada = "06_BRIGADA NORTE", Activo = TRUE,
    IdZonaDeTrabajo = 1L, IdUsuario = 2L, IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "Municipios", data.frame(Id = 1L, Municipio = "Centro", stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "ZonaDeTrabajo", data.frame(IdZona = 1L, Descripcion = "6B", IdProyecto = 10L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "Grupos", data.frame(Id = 1L, Descripcion = "Cots", IdProyecto = 10L, stringsAsFactors = FALSE))

  DBI::dbWriteTable(con, "UsuarioLog", data.frame(
    IdHistorico = c(1L, 2L, 3L),
    IdUsuario = c(1L, 3L, 4L),
    IdCargo = c(1L, 1L, 1L),
    IdEstado = c(1L, 1L, 1L),
    IdMunicipio = c(1L, 1L, 1L),
    IdZonaDeTabajo = c(1L, 1L, 1L),
    IdSupervisor = c(2L, 2L, 2L),
    IdBrigada = c(100L, 100L, 100L),
    FechaInsert = rep("2026-01-15 00:00:00", 3),
    IdProyecto = c(10L, 10L, 10L),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "BrigadasLog", data.frame(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("06_BRIGADA NORTE", "06_BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, 2L),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = c("2026-01-01 00:00:00", "2026-01-15 00:00:00"),
    IdProyecto      = c(10L, 10L),
    stringsAsFactors = FALSE
  ))

  # Nombrada "snapshot_id_1" para que cargar_usuarios_asignados() la reconozca
  # como fuente ligada a EncuestaId = 1 (mismo patrón que produccion, p.ej.
  # "snapshot_id_304"). Solo Ana (usuario_num "003") tiene actividad real;
  # Beto ("004") no aparece aqui.
  DBI::dbWriteTable(con, "snapshot_id_1", data.frame(
    fecha = "2026-03-20",
    usuario_num = "003",
    seccion = "0001",
    desglose = "Efectivo",
    duracion_minutos = 10,
    origen = "Actividad",
    stringsAsFactors = FALSE
  ))

  # UsuariosEncuesta solo cubre a Juan y Carlos; Ana y Beto quedan sin fila.
  DBI::dbWriteTable(con, "UsuariosEncuesta", data.frame(
    UsuarioId  = c(1L, 2L),
    EncuestaId = c(1L, 1L),
    Activo     = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  ))

  fuentes <- list(list(
    tabla = "snapshot_id_1",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  resultado <- suppressWarnings(cargar_insumos(
    pool = con,
    id_proyecto = 10L,
    corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes
  ))

  bd_aux <- resultado$bd_aux

  # Ana tiene actividad real -> debe quedar en bd_aux con su brigada/nombre,
  # aunque nunca tuvo fila en UsuariosEncuesta.
  fila_ana <- bd_aux[bd_aux$vocero == "003", ]
  expect_equal(nrow(fila_ana), 1L)
  expect_equal(fila_ana$nombre_vocero, "ANA RUIZ CRUZ")
  expect_equal(fila_ana$nombre_brigada, "06_BRIGADA NORTE")

  # Beto no tiene actividad real ni UsuariosEncuesta -> sigue excluido
  # (se preserva la protección original contra usuarios de otras encuestas).
  fila_beto <- bd_aux[bd_aux$vocero == "004", ]
  expect_equal(nrow(fila_beto), 0L)

  # Debe emitir advertencia mencionando al usuario rescatado (IdUsuario 3 = Ana).
  expect_warning(
    cargar_insumos(
      pool = con, id_proyecto = 10L, corte = as.Date("2026-03-31"),
      fuentes_actividad = fuentes
    ),
    "actividad real pero sin UsuariosEncuesta"
  )
})

test_that("cargar_insumos resuelve nombre de zona de trabajo y grupo en bd_aux", {
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

  bd_aux <- resultado$bd_aux
  expect_true(all(c("nombre_zona_trabajo", "nombre_grupo") %in% names(bd_aux)))
  # ZonaDeTrabajo IdZona=1 → "6B"; Grupos Id=1 → "Cots" (vía BrigadasLog LOCF).
  vocero_row <- bd_aux[bd_aux$vocero == "001", ]
  expect_equal(vocero_row$nombre_zona_trabajo, "6B")
  expect_equal(vocero_row$nombre_grupo, "Cots")
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
  DBI::dbWriteTable(con, "ZonaDeTrabajo", data.frame(IdZona = 1L, Descripcion = "6B", IdProyecto = 10L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "Grupos", data.frame(Id = 1L, Descripcion = "Cots", IdProyecto = 10L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "UsuarioLog", data.frame(
    IdHistorico = 1L, IdUsuario = 1L, IdCargo = 1L, IdEstado = 1L,
    IdMunicipio = 1L, IdZonaDeTabajo = 1L, IdSupervisor = 2L, IdBrigada = 100L,
    FechaInsert = "2026-01-15 00:00:00", IdProyecto = 10L,
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "BrigadasLog", data.frame(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("BRIGADA NORTE", "BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, 2L),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = c("2026-01-01 00:00:00", "2026-01-15 00:00:00"),
    IdProyecto      = c(10L, 10L),
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "Actividad", data.frame(
    fecha = "2026-03-20", usuario_num = "001", seccion = "0001",
    desglose = "Efectivo", duracion_minutos = 10, origen = "Actividad",
    stringsAsFactors = FALSE
  ))
  DBI::dbWriteTable(con, "UsuariosEncuesta", data.frame(
    UsuarioId = c(1L, 2L), EncuestaId = c(1L, 1L), Activo = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  ))

  fuentes <- list(list(
    tabla = "Actividad",
    select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
    origen = "Actividad"
  ))

  # Sin cargo_coordinador correcto → coordinadores vacío; Brigadas.IdUsuario apunta
  # a un cargo distinto → advertencia esperada de coordinador no resuelto.
  res_default <- suppressWarnings(cargar_insumos(
    pool = con, id_proyecto = 10L, corte = as.Date("2026-03-31"),
    fuentes_actividad = fuentes
  ))
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

test_that("resolver_brigada_en_fecha: num con dos id_usuario distintos no duplica actividad ni produce NA si uno tiene brigada", {
  # Escenario: el mismo num ("003") aparece con dos id_usuario (3 y 4) en num_map
  # (usuario re-registrado). En la misma fecha, uno tiene IdBrigada=84 y el otro NA.
  # La actividad debe resolverse a 84, sin duplicar filas.
  log_duplicado <- dplyr::bind_rows(
    make_usuario_log(),
    dplyr::tibble(
      IdHistorico    = c(50L, 51L),
      IdUsuario      = c(3L, 4L),
      IdCargo        = c(1L, 1L),
      IdEstado       = c(1L, 1L),
      IdMunicipio    = c(1L, 1L),
      IdZonaDeTabajo = c(1L, 1L),
      IdSupervisor   = c(99L, 99L),
      IdBrigada      = c(84L, NA_integer_),
      FechaInsert    = as.POSIXct(c("2026-04-12 08:00:00", "2026-04-12 09:00:00"), tz = "UTC"),
      ts_evento      = as.POSIXct(c("2026-04-12 08:00:00", "2026-04-12 09:00:00"), tz = "America/Mexico_City"),
      fecha_evento   = as.Date(c("2026-04-12", "2026-04-12"))
    )
  )

  actividad <- dplyr::tibble(
    usuario_num = "003",
    fecha       = as.Date("2026-04-13")
  )

  num_map_dup <- dplyr::tibble(
    id_usuario = c(1L, 2L, 3L, 4L),
    num        = c("001", "002", "003", "003")  # id 3 e id 4 comparten num
  )

  result <- resolver_brigada_en_fecha(actividad, log_duplicado, make_usuarios_cat(), num_map = num_map_dup)

  expect_equal(nrow(result), 1L)
  expect_equal(result$id_brigada, 84L)
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
  id_brigada         = c(100L, 200L),
  nombre_brigada_log = c("BRIGADA NORTE", "BRIGADA SUR"),
  id_coordinador_log = c(2L, NA_integer_),
  id_zona_trabajo    = c(1L, 1L),
  id_grupo           = c(1L, 1L),
  activo_brigada     = c(TRUE, TRUE)
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

test_that("construir_bd_aux: nombre_zona_trabajo/nombre_grupo NA sin catálogos", {
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  result <- construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_bd, mcat_bd)
  expect_true(all(c("nombre_zona_trabajo", "nombre_grupo") %in% names(result)))
  expect_true(all(is.na(result$nombre_zona_trabajo)))
  expect_true(all(is.na(result$nombre_grupo)))
})

test_that("construir_bd_aux: resuelve nombres desde zonas_cat/grupos_cat", {
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  zonas_cat  <- dplyr::tibble(id_zona_trabajo = 1L, nombre_zona_trabajo = "6B")
  grupos_cat <- dplyr::tibble(id_grupo = 1L, nombre_grupo = "Cots")
  result <- construir_bd_aux(
    ec, ucat_bd, ccat_bd, bcat_bd, mcat_bd,
    zonas_cat = zonas_cat, grupos_cat = grupos_cat
  )
  vocero_row <- result[result$vocero == "001", ]
  expect_equal(vocero_row$nombre_zona_trabajo, "6B")
  expect_equal(vocero_row$nombre_grupo, "Cots")
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

test_that("construir_bd_aux: distrito NA cuando nombres de brigada no siguen patron DD", {
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  result <- construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_bd, mcat_bd)
  # bcat_bd tiene "BRIGADA NORTE" / "BRIGADA SUR" — sin prefijo numérico
  expect_true(all(is.na(result$distrito)))
})

test_that("construir_bd_aux: distrito extraido cuando todos los nombres de brigada empiezan con DD", {
  bcat_con_distrito <- dplyr::tibble(
    id_brigada         = c(100L, 200L),
    nombre_brigada_log = c("01 BRIGADA NORTE", "02 BRIGADA SUR"),
    id_coordinador_log = c(2L, NA_integer_),
    id_zona_trabajo    = c(1L, 1L),
    id_grupo           = c(1L, 1L),
    activo_brigada     = c(TRUE, TRUE)
  )
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 100L)
  result <- construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_con_distrito, mcat_bd)
  expect_false(any(is.na(result$distrito)))
  expect_true(all(result$distrito %in% c("01", "02")))
})

test_that("construir_bd_aux: distrito parcial cuando solo algunas brigadas siguen el patron DD", {
  bcat_mixto <- dplyr::tibble(
    id_brigada         = c(100L, 200L),
    nombre_brigada_log = c("01 BRIGADA NORTE", "BRIGADA SUR"),  # mixto
    id_coordinador_log = c(2L, NA_integer_),
    id_zona_trabajo    = c(1L, 1L),
    id_grupo           = c(1L, 1L),
    activo_brigada     = c(TRUE, TRUE)
  )
  # Al menos una brigada tiene prefijo DD → usar_distrito TRUE.
  # "01 BRIGADA NORTE" → distrito "01"; "BRIGADA SUR" → NA.
  ec <- make_ec(id_brigada_vocero = 100L, id_brigada_coord = 200L)
  result <- suppressWarnings(construir_bd_aux(ec, ucat_bd, ccat_bd, bcat_mixto, mcat_bd))
  expect_true(any(!is.na(result$distrito)))
  expect_true(any(is.na(result$distrito)))
  expect_equal(result$distrito[result$nombre_brigada == "01 BRIGADA NORTE"], "01")
  expect_true(is.na(result$distrito[result$nombre_brigada == "BRIGADA SUR"]))
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

# =========================================================================
# TESTS: Coordinador-Vocero — sin pérdida de información ni duplicados
# =========================================================================
#
# Escenario raíz: un coordinador (id=2, num="002") aparece en UsuariosEncuesta
# con EncuestaId=288, pero el snapshot activo es 287. Por eso no queda en
# usuarios_asignados. Sin el fix de ETL su actividad desaparecería del reporte.
# Con el fix, debe aparecer en usuarios_cat (vía Cargo) y en bd_aux con
# vocero poblado, sin filas duplicadas aunque se auto-supervise en UsuarioLog.
# =========================================================================

setup_coord_removido_db <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(con, "Usuarios", data.frame(
    Id           = c(1L, 2L),
    IdProyecto   = c(10L, 10L),
    Num          = c("001", "002"),
    Cargo        = c("Vocero", "Coordinador de Brigada"),
    Status       = c(TRUE, TRUE),
    Municipio    = c("Centro", "Centro"),
    Nombre       = c("Juan", "Carlos"),
    APaterno     = c("Perez", "Soto"),
    AMaterno     = c("Lopez", "Diaz"),
    IdBrigada    = c(100L, 100L),
    Capacitacion = c(TRUE, FALSE),
    FechaUpdate  = c("2026-01-01", "2026-01-01"),
    stringsAsFactors = FALSE
  ))

  # Vocero (id=1) → EncuestaId 287 (matches snapshot activo).
  # Coordinador (id=2) → EncuestaId 288 (distinto): queda fuera de usuarios_asignados.
  DBI::dbWriteTable(con, "UsuariosEncuesta", data.frame(
    Id         = c(1L, 2L),
    EncuestaId = c(287L, 288L),
    UsuarioId  = c(1L, 2L),
    Activo     = c(1L, 1L),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Brigadas", data.frame(
    Id              = 100L,
    NombreBrigada   = "01 BRIGADA NORTE",
    Activo          = TRUE,
    IdZonaDeTrabajo = 1L,
    IdUsuario       = 2L,
    IdProyecto      = 10L,
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "Municipios", data.frame(
    Id        = 1L,
    Municipio = "Centro",
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "ZonaDeTrabajo", data.frame(IdZona = 1L, Descripcion = "6B", IdProyecto = 10L, stringsAsFactors = FALSE))
  DBI::dbWriteTable(con, "Grupos", data.frame(Id = 1L, Descripcion = "Cots", IdProyecto = 10L, stringsAsFactors = FALSE))

  # Coordinador (id=2) se auto-supervisa (IdSupervisor = 2).
  DBI::dbWriteTable(con, "UsuarioLog", data.frame(
    IdHistorico    = c(1L, 2L),
    IdUsuario      = c(1L, 2L),
    IdCargo        = c(1L, 2L),
    IdEstado       = c(1L, 1L),
    IdMunicipio    = c(1L, 1L),
    IdZonaDeTabajo = c(1L, 1L),
    IdSupervisor   = c(2L, 2L),
    IdBrigada      = c(100L, 100L),
    FechaInsert    = c("2026-01-15 00:00:00", "2026-01-15 00:00:00"),
    IdProyecto     = c(10L, 10L),
    stringsAsFactors = FALSE
  ))

  DBI::dbWriteTable(con, "BrigadasLog", data.frame(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("01 BRIGADA NORTE", "01 BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, 2L),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = c("2026-01-01 00:00:00", "2026-01-15 00:00:00"),
    IdProyecto      = c(10L, 10L),
    stringsAsFactors = FALSE
  ))

  # Ambos usuarios tienen actividad en el snapshot activo (287).
  DBI::dbWriteTable(con, "snapshot_id_287", data.frame(
    fecha            = c("2026-03-20", "2026-03-20"),
    usuario_num      = c("001", "002"),
    seccion          = c("0001", "0002"),
    desglose         = c("Efectivo", "Efectivo"),
    duracion_minutos = c(10L, 15L),
    stringsAsFactors = FALSE
  ))

  con
}

fuentes_snapshot_287 <- list(list(
  tabla       = "snapshot_id_287",
  select_cols = c("fecha", "usuario_num", "seccion", "desglose", "duracion_minutos"),
  origen      = "snapshot_id_287"
))

test_that("cargar_usuarios_cat: coordinador fuera de usuarios_asignados sigue en el catálogo", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Usuarios", data.frame(
    Id = c(1L, 2L), IdProyecto = c(10L, 10L),
    Num = c("001", "002"),
    Cargo = c("Vocero", "Coordinador de Brigada"),
    Status = c(TRUE, TRUE),
    Municipio = c("Centro", "Centro"),
    Nombre = c("Juan", "Carlos"), APaterno = c("Perez", "Soto"), AMaterno = c("Lopez", "Diaz"),
    IdBrigada = c(100L, 100L), Capacitacion = c(TRUE, FALSE),
    FechaUpdate = c("2026-01-01", "2026-01-01"),
    stringsAsFactors = FALSE
  ))

  result <- cargar_usuarios_cat(
    con, 10L,
    usuarios_asignados  = 1L,
    cargo_coordinador   = "Coordinador de Brigada"
  )

  expect_true(1L %in% result$id_usuario, label = "vocero presente")
  expect_true(2L %in% result$id_usuario, label = "coordinador presente aunque no esté en usuarios_asignados")
  expect_equal(result$cargo[result$id_usuario == 2L], "Coordinador de Brigada")
})

test_that("cargar_insumos: coordinador con EncuestaId distinto al snapshot está en insumos$cat$usuarios", {
  con <- setup_coord_removido_db()
  on.exit(DBI::dbDisconnect(con))

  resultado <- suppressWarnings(cargar_insumos(
    pool              = con,
    id_proyecto       = 10L,
    corte             = as.Date("2026-03-31"),
    fuentes_actividad = fuentes_snapshot_287
  ))

  expect_true("002" %in% resultado$cat$usuarios$num)
  expect_equal(
    resultado$cat$usuarios$cargo[resultado$cat$usuarios$num == "002"],
    "Coordinador de Brigada"
  )
})

test_that("cargar_insumos: bd_aux tiene vocero poblado para coordinador excluido de UsuariosEncuesta", {
  con <- setup_coord_removido_db()
  on.exit(DBI::dbDisconnect(con))

  resultado <- suppressWarnings(cargar_insumos(
    pool              = con,
    id_proyecto       = 10L,
    corte             = as.Date("2026-03-31"),
    fuentes_actividad = fuentes_snapshot_287
  ))

  coord_row <- dplyr::filter(resultado$bd_aux, vocero == "002")
  expect_equal(nrow(coord_row), 1L)
  expect_false(is.na(coord_row$nombre_vocero))
})

test_that("cargar_insumos: bd_aux sin filas duplicadas cuando coordinador se auto-supervisa", {
  con <- setup_coord_removido_db()
  on.exit(DBI::dbDisconnect(con))

  resultado <- suppressWarnings(cargar_insumos(
    pool              = con,
    id_proyecto       = 10L,
    corte             = as.Date("2026-03-31"),
    fuentes_actividad = fuentes_snapshot_287
  ))

  # 2 usuarios en UsuarioLog → exactamente 2 filas, sin duplicados por auto-supervisión
  expect_equal(nrow(resultado$bd_aux), 2L)
  voceros_presentes <- resultado$bd_aux$vocero[!is.na(resultado$bd_aux$vocero)]
  expect_equal(length(voceros_presentes), dplyr::n_distinct(voceros_presentes))
})

# =========================================================================
# TESTS: resolver_coordinador_en_fecha
# =========================================================================

make_brigada_log <- function() {
  dplyr::tibble(
    IdHistorico     = c(1L, 2L, 3L),
    BrigadaId       = c(100L, 100L, 200L),
    NombreBrigada   = c("BRIGADA NORTE", "BRIGADA NORTE", "BRIGADA SUR"),
    IdUsuario       = c(NA_integer_, 2L, 5L),
    IdZonaDeTrabajo = c(1L, 1L, 2L),
    IdGrupo         = c(1L, 1L, 2L),
    Activo          = c(TRUE, TRUE, TRUE),
    FechaInsert     = as.POSIXct(c(
      "2026-01-01 00:00:00",
      "2026-01-15 00:00:00",
      "2026-01-01 00:00:00"
    ), tz = "UTC"),
    ts_evento       = as.POSIXct(c(
      "2026-01-01 00:00:00",
      "2026-01-15 00:00:00",
      "2026-01-01 00:00:00"
    ), tz = "America/Mexico_City"),
    fecha_evento    = as.Date(c("2026-01-01", "2026-01-15", "2026-01-01"))
  )
}

test_that("resolver_coordinador_en_fecha: LOCF resuelve coordinador correcto al corte", {
  result <- resolver_coordinador_en_fecha(make_brigada_log(), corte = as.Date("2026-03-31"))

  expect_equal(nrow(result), 2L)
  brigada_100 <- result[result$id_brigada == 100L, ]
  expect_equal(brigada_100$id_coordinador_log, 2L)
  expect_equal(brigada_100$nombre_brigada_log, "BRIGADA NORTE")
})

test_that("resolver_coordinador_en_fecha: primera entrada NULL se resuelve correctamente via LOCF", {
  # IdHistorico 1 es NULL, IdHistorico 2 tiene coordinador — LOCF propaga hacia adelante
  # pero el orden es por IdHistorico, así que slice_tail devuelve la última fila (con valor)
  log_con_null_inicial <- dplyr::tibble(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("BRIGADA NORTE", "BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, 2L),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = as.POSIXct(c("2026-01-01", "2026-01-15"), tz = "UTC"),
    ts_evento       = as.POSIXct(c("2026-01-01", "2026-01-15"), tz = "America/Mexico_City"),
    fecha_evento    = as.Date(c("2026-01-01", "2026-01-15"))
  )

  result <- resolver_coordinador_en_fecha(log_con_null_inicial, corte = as.Date("2026-03-31"))

  expect_equal(nrow(result), 1L)
  expect_equal(result$id_coordinador_log, 2L)
})

test_that("resolver_coordinador_en_fecha: brigada con todos IdUsuario NULL retorna NA sin error", {
  log_null <- dplyr::tibble(
    IdHistorico     = c(1L, 2L),
    BrigadaId       = c(100L, 100L),
    NombreBrigada   = c("BRIGADA NORTE", "BRIGADA NORTE"),
    IdUsuario       = c(NA_integer_, NA_integer_),
    IdZonaDeTrabajo = c(1L, 1L),
    IdGrupo         = c(1L, 1L),
    Activo          = c(TRUE, TRUE),
    FechaInsert     = as.POSIXct(c("2026-01-01", "2026-01-15"), tz = "UTC"),
    ts_evento       = as.POSIXct(c("2026-01-01", "2026-01-15"), tz = "America/Mexico_City"),
    fecha_evento    = as.Date(c("2026-01-01", "2026-01-15"))
  )

  result <- resolver_coordinador_en_fecha(log_null, corte = as.Date("2026-03-31"))

  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$id_coordinador_log))
})

test_that("resolver_coordinador_en_fecha: todos los eventos después del corte retorna 0 filas", {
  result <- resolver_coordinador_en_fecha(make_brigada_log(), corte = as.Date("2025-12-31"))

  expect_equal(nrow(result), 0L)
})

test_that("resolver_coordinador_en_fecha: incluye columnas id_zona_trabajo e id_grupo", {
  result <- resolver_coordinador_en_fecha(make_brigada_log(), corte = as.Date("2026-03-31"))

  expect_true("id_zona_trabajo" %in% names(result))
  expect_true("id_grupo" %in% names(result))
  brigada_200 <- result[result$id_brigada == 200L, ]
  expect_equal(brigada_200$id_zona_trabajo, 2L)
  expect_equal(brigada_200$id_grupo, 2L)
})

# =========================================================================
# TESTS: resolver_brigada_en_fecha — ruta directa via id_usuario_actividad
# =========================================================================

test_that("resolver_brigada_en_fecha: usa ruta directa cuando id_usuario_actividad está presente", {
  actividad <- dplyr::tibble(
    usuario_num          = NA_character_,
    fecha                = as.Date("2026-02-01"),
    id_usuario_actividad = 1L
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 1L)
  expect_equal(result$id_brigada, 100L)
})

test_that("resolver_brigada_en_fecha: ruta directa devuelve NA cuando usuario sin brigada en log", {
  actividad <- dplyr::tibble(
    usuario_num          = NA_character_,
    fecha                = as.Date("2025-12-31"),  # antes de cualquier log
    id_usuario_actividad = 1L
  )

  result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat())

  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$id_brigada))
})

test_that("resolver_brigada_en_fecha: un solo NA en id_usuario_actividad avisa y cae a ruta legacy", {
  # La columna existe pero está parcialmente poblada: debe usar usuario_num
  # para TODAS las filas y emitir una advertencia con el conteo de NAs.
  actividad <- dplyr::tibble(
    usuario_num          = c("1", "2"),
    fecha                = as.Date(c("2026-02-01", "2026-02-01")),
    id_usuario_actividad = c(1L, NA_integer_)
  )

  expect_warning(
    result <- resolver_brigada_en_fecha(actividad, make_usuario_log(), make_usuarios_cat()),
    "1 de 2"
  )
  expect_equal(nrow(result), 2L)
})

# =========================================================================
# TESTS: excluir_brigadas_prueba
# =========================================================================

test_that("excluir_brigadas_prueba excluye brigadas de prueba de bd_aux y bd_actividad", {
  bd_aux <- dplyr::tibble(
    nombre_brigada = c("06_BRIGADA NORTE", "00_PRUEBAS"),
    vocero         = c("001", "999"),
    supervisor     = c("002", NA_character_)
  )
  bd_act <- dplyr::tibble(
    usuario_num = c("001", "999"),
    desglose    = c("Efectivo", "Efectivo")
  )

  res <- suppressWarnings(excluir_brigadas_prueba(bd_aux, bd_act))

  expect_equal(nrow(res$bd_aux), 1L)
  expect_false(any(grepl("prueba", res$bd_aux$nombre_brigada, ignore.case = TRUE)))
  # La actividad del vocero de la brigada de prueba (999) también se elimina.
  expect_equal(nrow(res$bd_actividad), 1L)
  expect_false("999" %in% res$bd_actividad$usuario_num)
})

test_that("excluir_brigadas_prueba advierte cuando una brigada de prueba tiene diálogos", {
  bd_aux <- dplyr::tibble(
    nombre_brigada = "00_PRUEBAS",
    vocero         = "999",
    supervisor     = NA_character_
  )
  bd_act <- dplyr::tibble(usuario_num = "999", desglose = "Efectivo")

  expect_warning(excluir_brigadas_prueba(bd_aux, bd_act), "prueba")
})

test_that("excluir_brigadas_prueba no advierte si la brigada de prueba no tiene actividad", {
  bd_aux <- dplyr::tibble(
    nombre_brigada = "00_PRUEBAS",
    vocero         = "999",
    supervisor     = NA_character_
  )
  bd_act <- dplyr::tibble(usuario_num = "001", desglose = "Efectivo")

  expect_no_warning(res <- excluir_brigadas_prueba(bd_aux, bd_act))
  expect_equal(nrow(res$bd_aux), 0L)
  expect_equal(nrow(res$bd_actividad), 1L)  # actividad ajena intacta
})

test_that("excluir_brigadas_prueba sin coincidencias devuelve insumos intactos", {
  bd_aux <- dplyr::tibble(nombre_brigada = "06_BRIGADA NORTE", vocero = "001", supervisor = "002")
  bd_act <- dplyr::tibble(usuario_num = "001", desglose = "Efectivo")

  res <- excluir_brigadas_prueba(bd_aux, bd_act)

  expect_equal(nrow(res$bd_aux), 1L)
  expect_equal(nrow(res$bd_actividad), 1L)
})
