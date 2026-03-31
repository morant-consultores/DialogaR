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
