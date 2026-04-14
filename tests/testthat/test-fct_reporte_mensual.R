# tests/testthat/test-fct_reporte_mensual.R

# =========================================================================
# HELPERS
# =========================================================================

make_bd_completa_m <- function(fecha_ini, fecha_fin_arg) {
  fechas <- seq.Date(fecha_ini, fecha_fin_arg, by = "day")
  tibble::tibble(
    fecha            = fechas,
    usuario_num      = "001",
    desglose         = "Efectivo",
    duracion_minutos = 12,
    fecha_inicio     = as.POSIXct(paste(as.character(fechas), "08:00:00")),
    fecha_fin        = as.POSIXct(paste(as.character(fechas), "14:00:00"))
  )
}

make_bd_aux_m <- function() {
  tibble::tibble(
    distrito           = "06",
    municipio          = "Centro",
    nombre_brigada     = "06_BRIGADA NORTE",
    nombre_coordinador = "CARLOS SOTO",
    supervisor         = "002",
    status_coord       = TRUE,
    nombre_vocero      = "JUAN PEREZ",
    vocero             = "001",
    status_vocero      = TRUE
  )
}

make_insumos_cat <- function() {
  list(
    cat = list(
      usuario_log = tibble::tibble(
        IdHistorico    = 1L,
        IdUsuario      = 1L,
        IdCargo        = 1L,
        IdEstado       = 1L,
        IdMunicipio    = 1L,
        IdZonaDeTabajo = 1L,
        IdSupervisor   = 2L,
        IdBrigada      = 100L,
        FechaInsert    = "2026-01-15 00:00:00"
      )
    )
  )
}

mock_usuarios_tibble <- function() {
  tibble::tibble(
    Id         = c(1L, 2L),
    IdProyecto = c(10L, 10L),
    Num        = c("001", "002"),
    Status     = c(TRUE, TRUE),
    FechaUpdate = c("2026-01-01", "2026-01-01")
  )
}

# Fixture reutilizable: un registro efectivo por vocero en un rango dado
make_bd_completa_multi <- function(fechas, usuario_nums) {
  tibble::tibble(
    fecha            = fechas,
    usuario_num      = usuario_nums,
    desglose         = "Efectivo",
    duracion_minutos = 10,
    fecha_inicio     = as.POSIXct(paste(as.character(fechas), "08:00:00")),
    fecha_fin        = as.POSIXct(paste(as.character(fechas), "14:00:00"))
  )
}

# Fixture reutilizable: bd_aux con voceros y supervisor explícitos
make_bd_aux_rows <- function(voceros, supervisores, status_coord, status_vocero = NULL) {
  n <- length(voceros)
  if (is.null(status_vocero)) status_vocero <- rep(TRUE, n)
  tibble::tibble(
    distrito           = rep("01", n),
    municipio          = rep("Centro", n),
    nombre_brigada     = rep("BRIGADA TEST", n),
    nombre_coordinador = paste("COORD", supervisores),
    supervisor         = supervisores,
    status_coord       = status_coord,
    nombre_vocero      = paste("VOC", voceros),
    vocero             = voceros,
    status_vocero      = status_vocero
  )
}

# =========================================================================
# TESTS: generar_reporte_brigadas
# =========================================================================

test_that("generar_reporte_brigadas (semanal) returns list with expected keys", {
  # 2026-03-23 is a Monday; use week_start = 1 (Monday) for simplicity
  corte       <- as.Date("2026-03-25")
  fecha_ini   <- as.Date("2026-03-23")   # Monday of that week

  bd_completa <- make_bd_completa_m(fecha_ini, corte)

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = make_bd_aux_m(),
    insumos     = make_insumos_cat(),
    week_start  = 1L
  )

  expect_type(resultado, "list")
  expect_true(all(c("registros", "cortos", "rango_fechas") %in% names(resultado)))
})

test_that("generar_reporte_brigadas (mensual) returns registros with Total column", {
  # Use a date well inside a month to avoid edge-case floor/ceiling issues
  corte       <- as.Date("2026-02-15")
  fecha_ini   <- as.Date("2026-01-01")   # floor of 2026-01-14 to month

  bd_completa <- make_bd_completa_m(fecha_ini, corte)

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- generar_reporte_brigadas(
    reporte     = "mensual",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = make_bd_aux_m(),
    insumos     = make_insumos_cat()
  )

  expect_true("Total" %in% names(resultado$registros))
  expect_s3_class(resultado$registros, "data.frame")
})

test_that("generar_reporte_brigadas imputes SIN ASIGNAR for voceros with no coordinator in bd_aux", {
  corte     <- as.Date("2026-03-25")
  fecha_ini <- as.Date("2026-03-23")

  # bd_aux row with NA coordinator → imputed to "SIN ASIGNAR"
  bd_aux_sin_coord <- tibble::tibble(
    distrito           = "06",
    municipio          = "Centro",
    nombre_brigada     = NA_character_,
    nombre_coordinador = NA_character_,
    supervisor         = NA_character_,
    status_coord       = NA,
    nombre_vocero      = "VOCERO HUERFANO",
    vocero             = "999",
    status_vocero      = TRUE
  )

  bd_completa <- tibble::tibble(
    fecha            = corte,
    usuario_num      = "999",
    desglose         = "Efectivo",
    duracion_minutos = 10,
    fecha_inicio     = as.POSIXct(paste(as.character(corte), "08:00:00")),
    fecha_fin        = as.POSIXct(paste(as.character(corte), "14:00:00"))
  )

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- suppressWarnings(generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = bd_aux_sin_coord,
    insumos     = make_insumos_cat(),
    week_start  = 1L
  ))

  expect_true(
    any(resultado$registros$nombre_coordinador == "SIN ASIGNAR", na.rm = TRUE)
  )
})

# =========================================================================
# TESTS: integridad de registros
# =========================================================================

test_that("el Total del reporte coincide exactamente con los registros efectivos de bd_completa", {
  corte     <- as.Date("2026-03-25")
  fecha_ini <- as.Date("2026-03-23")
  fechas    <- seq.Date(fecha_ini, corte, by = "day")

  # 5 registros efectivos: vocero "001" tiene 3, "002" tiene 2
  bd_completa <- make_bd_completa_multi(
    fechas      = c(fechas[1], fechas[1], fechas[2], fechas[2], fechas[3]),
    usuario_nums = c("001", "001", "001", "002", "002")
  )
  # Añadir un registro NO efectivo que no debe contarse
  bd_completa <- dplyr::bind_rows(
    bd_completa,
    tibble::tibble(
      fecha = fechas[1], usuario_num = "001", desglose = "No efectivo",
      duracion_minutos = 5,
      fecha_inicio = as.POSIXct("2026-03-23 08:00:00"),
      fecha_fin    = as.POSIXct("2026-03-23 08:05:00")
    )
  )

  bd_aux <- make_bd_aux_rows(
    voceros     = c("001", "002"),
    supervisores = c("003", "003"),
    status_coord = c(TRUE, TRUE)
  )

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- suppressWarnings(generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = bd_aux,
    insumos     = make_insumos_cat(),
    week_start  = 1L
  ))

  n_efectivos_esperados <- sum(bd_completa$desglose == "Efectivo")
  expect_equal(
    sum(resultado$registros$Total, na.rm = TRUE),
    n_efectivos_esperados
  )
})

test_that("cada vocero aparece exactamente una vez cuando bd_aux tiene filas duplicadas por cambio de coordinador", {
  corte     <- as.Date("2026-03-25")
  fecha_ini <- as.Date("2026-03-23")

  bd_completa <- make_bd_completa_multi(
    fechas       = seq.Date(fecha_ini, corte, by = "day"),
    usuario_nums = rep("001", 3)
  )

  # Mismo vocero con dos coordinadores distintos (cambio histórico)
  bd_aux <- tibble::tibble(
    distrito           = "01",
    municipio          = "Centro",
    nombre_brigada     = "BRIGADA TEST",
    nombre_coordinador = c("COORD ACTUAL", "COORD ANTERIOR"),
    supervisor         = c("002", "003"),
    status_coord       = c(TRUE, FALSE),
    nombre_vocero      = c("VOC UNO", "VOC UNO"),
    vocero             = c("001", "001"),
    status_vocero      = c(TRUE, TRUE)
  )

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- suppressWarnings(generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = bd_aux,
    insumos     = make_insumos_cat(),
    week_start  = 1L
  ))

  voceros_reales <- resultado$registros |>
    dplyr::filter(!is.na(vocero), vocero != "SIN ASIGNAR")

  # Sin duplicados
  expect_equal(nrow(voceros_reales), dplyr::n_distinct(voceros_reales$vocero))

  # Se conservó el coordinador activo (status_coord = TRUE)
  fila_vocero <- dplyr::filter(voceros_reales, vocero == "001")
  expect_equal(fila_vocero$nombre_coordinador, "COORD ACTUAL")

  # Total no cambia por la deduplicación
  expect_equal(sum(resultado$registros$Total, na.rm = TRUE), 3L)
})

test_that("un coordinador que también es vocero aparece exactamente una vez en el reporte", {
  corte     <- as.Date("2026-03-25")
  fecha_ini <- as.Date("2026-03-23")
  fechas    <- seq.Date(fecha_ini, corte, by = "day")

  # Vocero "001" es además el supervisor de "002"
  bd_completa <- make_bd_completa_multi(
    fechas       = c(fechas[1], fechas[2], fechas[1]),
    usuario_nums = c("001", "001", "002")
  )

  bd_aux <- make_bd_aux_rows(
    voceros      = c("001", "002"),
    supervisores = c("001", "001"),   # "001" se coordina a sí mismo y a "002"
    status_coord = c(TRUE, TRUE)
  )

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_tibble(),
    .package = "dplyr"
  )

  resultado <- suppressWarnings(generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = bd_aux,
    insumos     = make_insumos_cat(),
    week_start  = 1L
  ))

  voceros_reales <- resultado$registros |>
    dplyr::filter(!is.na(vocero), vocero != "SIN ASIGNAR")

  # Sin duplicados
  expect_equal(nrow(voceros_reales), dplyr::n_distinct(voceros_reales$vocero))

  # Total correcto: 3 efectivos en total
  expect_equal(sum(resultado$registros$Total, na.rm = TRUE), 3L)
})

test_that("duplicados en la tabla Usuarios no generan filas duplicadas en el reporte", {
  corte     <- as.Date("2026-03-25")
  fecha_ini <- as.Date("2026-03-23")

  bd_completa <- make_bd_completa_m(fecha_ini, corte)   # vocero "001"
  bd_aux      <- make_bd_aux_m()                        # vocero "001", supervisor "002"

  # Simula que Usuarios tiene dos entradas para el mismo Id/Num (dato corrupto)
  mock_usuarios_dup <- tibble::tibble(
    Id          = c(1L, 1L),
    IdProyecto  = c(10L, 10L),
    Num         = c("001", "001"),
    Status      = c(TRUE, TRUE),
    FechaUpdate = c("2026-01-01", "2026-01-01")
  )

  testthat::local_mocked_bindings(
    tbl = function(src, ...) mock_usuarios_dup,
    .package = "dplyr"
  )

  resultado <- suppressWarnings(generar_reporte_brigadas(
    reporte     = "semanal",
    corte       = corte,
    id_proyecto = 10L,
    pool        = NULL,
    bd_completa = bd_completa,
    bd_aux      = bd_aux,
    insumos     = make_insumos_cat(),
    week_start  = 1L
  ))

  voceros_reales <- resultado$registros |>
    dplyr::filter(!is.na(vocero), vocero != "SIN ASIGNAR")

  expect_equal(nrow(voceros_reales), dplyr::n_distinct(voceros_reales$vocero))
})
