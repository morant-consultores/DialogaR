# tests/testthat/test-fct_productividad.R

# =========================================================================
# HELPERS
# =========================================================================

make_bd_aux_prod <- function() {
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

make_actividad_dia <- function(corte, usuario = "001", n = 5) {
  tibble::tibble(
    usuario_num      = usuario,
    seccion          = "0001",
    desglose         = c(rep("Efectivo", n), "No abrieron"),
    duracion_minutos = c(rep(12, n), 1),
    fecha_inicio     = as.POSIXct(paste(corte, "08:00:00")),
    fecha_fin        = as.POSIXct(paste(corte, "14:00:00"))
  )
}

make_pl_main <- function(corte, supervisor = "002", vocero = "001") {
  tibble::tibble(
    obtener_usuario = supervisor,
    usuario         = vocero,
    fecha           = corte,
    asistencia      = "Si",
    observaciones   = NA_character_,
    finalizar       = "1"
  )
}

# =========================================================================
# TESTS: generar_reporte_productividad
# =========================================================================

test_that("generar_reporte_productividad returns list with expected keys", {
  corte <- as.Date("2026-03-25")

  resultado <- generar_reporte_productividad(
    bd_aux       = make_bd_aux_prod(),
    actividad_dia = make_actividad_dia(corte),
    corte        = corte,
    pl_main      = make_pl_main(corte)
  )

  expect_type(resultado, "list")
  expect_true(all(c("corte", "pase_lista", "registros", "bd_prod") %in% names(resultado)))
  expect_equal(resultado$corte, corte)
})

test_that("generar_reporte_productividad bd_prod has required columns", {
  corte <- as.Date("2026-03-25")

  resultado <- generar_reporte_productividad(
    bd_aux        = make_bd_aux_prod(),
    actividad_dia = make_actividad_dia(corte),
    corte         = corte,
    pl_main       = make_pl_main(corte)
  )

  expect_s3_class(resultado$bd_prod, "data.frame")
  expected_cols <- c("nombre_brigada", "nombre_coordinador", "nombre_vocero", "vocero")
  expect_true(all(expected_cols %in% names(resultado$bd_prod)))
})

test_that("generar_reporte_productividad lenient policy succeeds with empty actividad", {
  corte <- as.Date("2026-03-25")

  empty_actividad <- tibble::tibble(
    usuario_num = character(),
    seccion = character(),
    desglose = character(),
    duracion_minutos = numeric(),
    fecha_inicio = as.POSIXct(character()),
    fecha_fin = as.POSIXct(character())
  )

  resultado <- generar_reporte_productividad(
    bd_aux        = make_bd_aux_prod(),
    actividad_dia = empty_actividad,
    corte         = corte,
    pl_main       = make_pl_main(corte),
    missing_policy = "lenient"
  )

  expect_type(resultado, "list")
  expect_s3_class(resultado$bd_prod, "data.frame")
})

test_that("generar_reporte_productividad strict policy fails with empty actividad", {
  corte <- as.Date("2026-03-25")

  empty_actividad <- tibble::tibble(
    usuario_num = character(),
    seccion = character(),
    desglose = character(),
    duracion_minutos = numeric(),
    fecha_inicio = as.POSIXct(character()),
    fecha_fin = as.POSIXct(character())
  )

  expect_error(
    generar_reporte_productividad(
      bd_aux        = make_bd_aux_prod(),
      actividad_dia = empty_actividad,
      corte         = corte,
      pl_main       = make_pl_main(corte),
      missing_policy = "strict"
    )
  )
})

test_that("generar_reporte_productividad strict policy fails with empty pase lista for date", {
  corte <- as.Date("2026-03-25")

  # pl_main has a different date → no rows after filter by corte
  pl_wrong_date <- make_pl_main(as.Date("2026-03-01"))

  expect_error(
    generar_reporte_productividad(
      bd_aux        = make_bd_aux_prod(),
      actividad_dia = make_actividad_dia(corte),
      corte         = corte,
      pl_main       = pl_wrong_date,
      missing_policy = "strict"
    )
  )
})

test_that("generar_reporte_productividad fails with pl_extra when allow_multiple_sources = FALSE", {
  corte <- as.Date("2026-03-25")

  expect_error(
    generar_reporte_productividad(
      bd_aux                 = make_bd_aux_prod(),
      actividad_dia          = make_actividad_dia(corte),
      corte                  = corte,
      pl_main                = make_pl_main(corte),
      pl_extra               = make_pl_main(corte),
      allow_multiple_sources = FALSE
    ),
    "allow_multiple_sources"
  )
})

# =========================================================================
# TESTS: crear_tabla_usuarios (funciones_pdf.R)
# =========================================================================

# =========================================================================
# TESTS: generar_tablas_reporte
# =========================================================================

make_bd_prod_tablas <- function(
  nombre_brigada     = "BRIGADA NORTE",
  nombre_coordinador = "COORD A",
  nombre_vocero      = "VOC 001",
  dialogos           = 5L,
  tiene_pase         = TRUE
) {
  tibble::tibble(
    nombre_brigada        = nombre_brigada,
    nombre_coordinador    = nombre_coordinador,
    nombre_vocero         = nombre_vocero,
    dialogos_efectivos_nube = dialogos,
    tiene_pase_lista      = tiene_pase,
    numero_pases_lista    = as.integer(tiene_pase)
  )
}

test_that("generar_tablas_reporte devuelve lista con resumen, detalle y cruda", {
  bd_prod <- make_bd_prod_tablas()

  resultado <- generar_tablas_reporte(bd_prod)

  expect_type(resultado, "list")
  expect_true(all(c("resumen", "detalle", "cruda") %in% names(resultado)))
  expect_s3_class(resultado$cruda, "data.frame")
  expect_true("TOTAL" %in% resultado$cruda$nombre)
  expect_true("BRIGADA NORTE" %in% resultado$cruda$nombre)
})

test_that("generar_tablas_reporte incluye correctamente brigada con coordinador inactivo con actividad", {
  # Escenario: bd_prod contiene una fila del coordinador con status_coord = FALSE
  # pero con dialogos > 0. postprocesar_output ahora la conserva; este test
  # verifica que generar_tablas_reporte no lanza error y la brigada aparece
  # en cruda con sus diálogos contabilizados.
  bd_prod <- tibble::tibble(
    nombre_brigada          = c("BRIGADA SUR", "BRIGADA SUR"),
    nombre_coordinador      = c("COORD BAJA", "COORD BAJA"),
    nombre_vocero           = c("COORD BAJA", "VOC 002"),   # fila coord + fila vocero
    dialogos_efectivos_nube = c(3L, 7L),
    tiene_pase_lista        = c(FALSE, TRUE),
    numero_pases_lista      = c(0L, 1L)
  )

  resultado <- expect_no_error(generar_tablas_reporte(bd_prod))

  fila <- dplyr::filter(resultado$cruda, nombre == "BRIGADA SUR")
  expect_equal(nrow(fila), 1L)
  expect_equal(fila$dialogos, 10L)   # 3 + 7
  expect_equal(fila$usuarios, 2L)    # ambas filas tienen dialogos > 0
})

# =========================================================================
# TESTS: crear_tabla_usuarios (funciones_pdf.R)
# =========================================================================

test_that("crear_tabla_usuarios returns pivoted data frame with 6 metric rows", {
  corte <- as.Date("2026-03-25")
  bd <- tibble::tibble(
    fecha        = corte,
    usuario_num  = c("001", "001", "002"),
    desglose     = c("Efectivo", "Efectivo", "No abrieron"),
    fecha_inicio = as.POSIXct(paste(corte, "08:00:00")),
    fecha_fin    = as.POSIXct(paste(corte, "14:00:00"))
  )

  resultado <- crear_tabla_usuarios(
    bd                  = bd,
    voceros_alta        = 50L,
    coordinadores_alta  = 10L,
    corte               = corte
  )

  expect_s3_class(resultado, "data.frame")
  expect_equal(ncol(resultado), 2L)
  expect_true(all(c("name", "value") %in% names(resultado)))
  expect_equal(nrow(resultado), 6L)
})

test_that("crear_tabla_usuarios returns dash for missing values when no activity", {
  corte <- as.Date("2026-03-25")
  # No rows matching corte
  bd <- tibble::tibble(
    fecha        = as.Date("2026-03-01"),
    usuario_num  = "001",
    desglose     = "Efectivo",
    fecha_inicio = as.POSIXct("2026-03-01 08:00:00"),
    fecha_fin    = as.POSIXct("2026-03-01 14:00:00")
  )

  resultado <- crear_tabla_usuarios(
    bd                  = bd,
    voceros_alta        = 50L,
    coordinadores_alta  = 10L,
    corte               = corte
  )

  expect_s3_class(resultado, "data.frame")
  # NaN/NA values should be replaced with "-"
  expect_true(all(resultado$value != "NaN"))
})
