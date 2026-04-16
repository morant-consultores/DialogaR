# tests/testthat/test-fct_validadores.R
#
# Cobertura de fct_validadores.R
# Organización por nivel de severidad:
#   · INTEGRIDAD  → expect_error(class = "error_contrato_*")
#   · CALIDAD     → expect_warning(class = "warn_calidad_*")
#   · PASS        → sin error ni warning

# =========================================================================
# Helpers compartidos
# =========================================================================

base_usuarios <- function() {
  data.frame(
    id_usuario       = c(1L, 2L),
    num              = c("001", "002"),
    cargo            = c("Vocero", "Coordinador de Brigada"),
    status           = c(TRUE, FALSE),
    municipio_usuario = c("Centro", "Centro"),
    id_brigada       = c(100L, 100L),
    stringsAsFactors = FALSE
  )
}

base_brigadas <- function() {
  data.frame(
    id_brigada             = 100L,
    nombre_brigada         = "06_BRIGADA NORTE",
    activo_brigada         = TRUE,
    id_zona_trabajo_brigada = 1L,
    stringsAsFactors       = FALSE
  )
}

base_actividad <- function() {
  data.frame(
    fecha            = as.Date("2026-03-20"),
    usuario_num      = "001",
    seccion          = "0001",
    desglose         = "Efectivo",
    duracion_minutos = 10,
    fecha_inicio     = as.POSIXct("2026-03-20 09:00:00"),
    fecha_fin        = as.POSIXct("2026-03-20 09:10:00"),
    stringsAsFactors = FALSE
  )
}

base_bd_aux <- function() {
  data.frame(
    distrito           = "01",
    municipio          = "Centro",
    nombre_brigada     = "06_BRIGADA NORTE",
    nombre_coordinador = "CARLOS SOTO",
    vocero             = "JUAN PEREZ",
    status_vocero      = TRUE,
    stringsAsFactors   = FALSE
  )
}

# =========================================================================
# validar_usuarios_cat
# =========================================================================

test_that("validar_usuarios_cat: pasa con datos válidos", {
  expect_silent(validar_usuarios_cat(base_usuarios()))
})

test_that("validar_usuarios_cat: pasa con data frame vacío", {
  df_vacio <- base_usuarios()[0, ]
  expect_silent(validar_usuarios_cat(df_vacio))
})

test_that("validar_usuarios_cat [INTEGRIDAD]: abort en columnas faltantes", {
  df <- base_usuarios()
  df$id_usuario <- NULL
  expect_error(validar_usuarios_cat(df), class = "error_contrato_usuarios")
})

test_that("validar_usuarios_cat [INTEGRIDAD]: abort en id_usuario duplicado", {
  df <- base_usuarios()
  df$id_usuario <- c(1L, 1L)
  expect_error(validar_usuarios_cat(df), class = "error_contrato_usuarios")
})

test_that("validar_usuarios_cat [INTEGRIDAD]: abort en status con tipo incorrecto", {
  df <- base_usuarios()
  df$status <- c("activo", "inactivo")
  expect_error(validar_usuarios_cat(df), class = "error_contrato_usuarios")
})

test_that("validar_usuarios_cat [INTEGRIDAD]: acepta status integer 0/1 (SQLite)", {
  df <- base_usuarios()
  df$status <- c(1L, 0L)
  expect_silent(validar_usuarios_cat(df))
})

test_that("validar_usuarios_cat [CALIDAD]: warning con resumen en id_usuario NA", {
  df <- base_usuarios()
  df$id_usuario[1] <- NA_integer_
  expect_warning(validar_usuarios_cat(df), class = "warn_calidad_usuarios")
})

# =========================================================================
# validar_brigadas_cat
# =========================================================================

test_that("validar_brigadas_cat: pasa con datos válidos", {
  expect_silent(validar_brigadas_cat(base_brigadas()))
})

test_that("validar_brigadas_cat [INTEGRIDAD]: abort en columnas faltantes", {
  df <- base_brigadas()
  df$id_brigada <- NULL
  expect_error(validar_brigadas_cat(df), class = "error_contrato_brigadas")
})

test_that("validar_brigadas_cat [INTEGRIDAD]: abort en id_brigada duplicado", {
  df <- rbind(base_brigadas(), base_brigadas())
  expect_error(validar_brigadas_cat(df), class = "error_contrato_brigadas")
})

test_that("validar_brigadas_cat [CALIDAD]: warning en prefijo de distrito no numérico", {
  df <- base_brigadas()
  df$nombre_brigada <- "SIN_PREFIJO_BRIGADA"
  expect_warning(validar_brigadas_cat(df), class = "warn_calidad_brigadas")
})

test_that("validar_brigadas_cat [CALIDAD]: no warning cuando prefijo es numérico", {
  df <- base_brigadas()
  df$nombre_brigada <- "12_BRIGADA SUR"
  expect_silent(validar_brigadas_cat(df))
})

# =========================================================================
# validar_bd_actividad
# =========================================================================

test_that("validar_bd_actividad: pasa con datos válidos", {
  expect_silent(validar_bd_actividad(base_actividad()))
})

test_that("validar_bd_actividad [INTEGRIDAD]: abort en columnas faltantes", {
  df <- base_actividad()
  df$fecha <- NULL
  expect_error(validar_bd_actividad(df), class = "error_contrato_actividad")
})

test_that("validar_bd_actividad [INTEGRIDAD]: abort cuando fecha no es Date", {
  df <- base_actividad()
  df$fecha <- as.character(df$fecha)
  expect_error(validar_bd_actividad(df), class = "error_contrato_actividad")
})

test_that("validar_bd_actividad [CALIDAD]: warning en desglose fuera de enum", {
  df <- base_actividad()
  df$desglose <- "VALOR_RARO"
  expect_warning(validar_bd_actividad(df), class = "warn_calidad_actividad")
})

test_that("validar_bd_actividad [CALIDAD]: warning en duracion_minutos negativa", {
  df <- base_actividad()
  df$duracion_minutos <- -5
  expect_warning(validar_bd_actividad(df), class = "warn_calidad_actividad")
})

test_that("validar_bd_actividad [CALIDAD]: warning en fecha_fin < fecha_inicio", {
  df <- base_actividad()
  df$fecha_fin <- as.POSIXct("2026-03-20 08:00:00")  # antes de fecha_inicio
  expect_warning(validar_bd_actividad(df), class = "warn_calidad_actividad")
})

test_that("validar_bd_actividad [CALIDAD]: enum personalizado acepta valor propio", {
  df <- base_actividad()
  df$desglose <- "VISITA_ESPECIAL"
  expect_silent(validar_bd_actividad(df, valores_desglose = c("Efectivo", "VISITA_ESPECIAL")))
})

test_that("validar_bd_actividad [CALIDAD]: resumen consolida múltiples problemas", {
  df <- base_actividad()
  df$desglose         <- "RARO"
  df$duracion_minutos <- -1
  # Debe emitir un solo warning con ambos problemas
  w <- testthat::capture_warnings(validar_bd_actividad(df))
  expect_length(w, 1)
})

# =========================================================================
# validar_bd_aux
# =========================================================================

test_that("validar_bd_aux: pasa con datos válidos", {
  expect_silent(validar_bd_aux(base_bd_aux()))
})

test_that("validar_bd_aux [INTEGRIDAD]: abort en columnas faltantes", {
  df <- base_bd_aux()
  df$status_vocero <- NULL
  expect_error(validar_bd_aux(df), class = "error_contrato_bd_aux")
})

test_that("validar_bd_aux [CALIDAD]: warning en vocero activo sin coordinador", {
  df <- base_bd_aux()
  df$nombre_coordinador <- NA_character_
  expect_warning(validar_bd_aux(df), class = "warn_calidad_bd_aux")
})

test_that("validar_bd_aux [CALIDAD]: warning en vocero activo sin brigada", {
  df <- base_bd_aux()
  df$nombre_brigada <- NA_character_
  expect_warning(validar_bd_aux(df), class = "warn_calidad_bd_aux")
})

test_that("validar_bd_aux [CALIDAD]: no warning cuando vocero inactivo carece de coordinador", {
  df <- base_bd_aux()
  df$status_vocero      <- FALSE
  df$nombre_coordinador <- NA_character_
  expect_silent(validar_bd_aux(df))
})

test_that("validar_bd_aux [CALIDAD]: resumen consolida coord y brigada faltantes", {
  df <- base_bd_aux()
  df$nombre_coordinador <- NA_character_
  df$nombre_brigada     <- NA_character_
  w <- testthat::capture_warnings(validar_bd_aux(df))
  expect_length(w, 1)
  expect_true(grepl("2 área", w[[1]]))
})

# =========================================================================
# validar_pase_lista
# =========================================================================

base_pase_lista <- function() {
  data.frame(
    supervisor       = "COORD A",
    vocero           = "001",
    fecha            = as.Date("2026-03-20"),
    stringsAsFactors = FALSE
  )
}

test_that("validar_pase_lista: pasa con datos válidos", {
  expect_silent(validar_pase_lista(base_pase_lista()))
})

test_that("validar_pase_lista [INTEGRIDAD]: abort en vocero NA", {
  df <- base_pase_lista()
  df$vocero <- NA_character_
  expect_error(validar_pase_lista(df), class = "error_contrato_pase_lista")
})

test_that("validar_pase_lista [INTEGRIDAD]: abort en supervisor NA", {
  df <- base_pase_lista()
  df$supervisor <- NA_character_
  expect_error(validar_pase_lista(df), class = "error_contrato_pase_lista")
})

# =========================================================================
# validar_metricas_diarias
# =========================================================================

base_metricas <- function() {
  data.frame(
    usuario_num              = "001",
    viviendas_visitadas_nube = 10,
    efectivos                = 5,
    stringsAsFactors         = FALSE
  )
}

test_that("validar_metricas_diarias: pasa con datos válidos", {
  expect_silent(validar_metricas_diarias(base_metricas()))
})

test_that("validar_metricas_diarias [CALIDAD]: warning en conteo negativo", {
  df <- base_metricas()
  df$efectivos <- -1
  expect_warning(validar_metricas_diarias(df), class = "warn_calidad_metricas")
})

test_that("validar_metricas_diarias [CALIDAD]: warning en efectivos > viviendas", {
  df <- base_metricas()
  df$efectivos <- 20
  expect_warning(validar_metricas_diarias(df), class = "warn_calidad_metricas")
})

# =========================================================================
# validar_base_contactos
# =========================================================================

base_contactos <- function(na_chr = "-") {
  data.frame(
    nombre           = "JUAN PEREZ",
    celular          = "6641234567",
    correo           = na_chr,
    edad             = 35L,
    stringsAsFactors = FALSE
  )
}

test_that("validar_base_contactos: pasa con datos válidos", {
  expect_silent(validar_base_contactos(base_contactos()))
})

test_that("validar_base_contactos [INTEGRIDAD]: abort en fila sin contacto tras filtro", {
  df <- base_contactos()
  df$celular <- "-"
  df$correo  <- "-"
  expect_error(validar_base_contactos(df), class = "error_contrato_contactos")
})

test_that("validar_base_contactos [CALIDAD]: warning en edad fuera de rango", {
  df <- base_contactos()
  df$edad <- 200L
  expect_warning(validar_base_contactos(df), class = "warn_calidad_contactos")
})

test_that("validar_base_contactos [CALIDAD]: warning en nombre NA", {
  df <- base_contactos()
  df$nombre <- NA_character_
  expect_warning(validar_base_contactos(df), class = "warn_calidad_contactos")
})

# =========================================================================
# validar_auditoria
# =========================================================================

base_auditoria <- function() {
  data.frame(
    totalEvaluacion  = c("85", "90"),
    dictamenFinal    = c("Buena", "Buena"),
    stringsAsFactors = FALSE
  )
}

test_that("validar_auditoria: pasa con datos válidos", {
  expect_silent(validar_auditoria(base_auditoria()))
})

test_that("validar_auditoria [INTEGRIDAD]: abort en columnas faltantes", {
  df <- base_auditoria()
  df$dictamenFinal <- NULL
  expect_error(validar_auditoria(df), class = "error_contrato_auditoria")
})

test_that("validar_auditoria [CALIDAD]: warning en totalEvaluacion no numérico", {
  df <- base_auditoria()
  df$totalEvaluacion[1] <- "N/A"
  expect_warning(validar_auditoria(df), class = "warn_calidad_auditoria")
})

test_that("validar_auditoria [CALIDAD]: warning en totalEvaluacion fuera de [0, 100]", {
  df <- base_auditoria()
  df$totalEvaluacion[1] <- "150"
  expect_warning(validar_auditoria(df), class = "warn_calidad_auditoria")
})

test_that("validar_auditoria [CALIDAD]: resumen consolida ambos problemas", {
  df <- base_auditoria()
  df$totalEvaluacion <- c("N/A", "150")
  w <- testthat::capture_warnings(validar_auditoria(df))
  expect_length(w, 1)
  expect_true(grepl("2 área", w[[1]]))
})

# =========================================================================
# .check_join_count (helper de integridad de joins)
# =========================================================================

test_that(".check_join_count: pasa cuando filas son iguales", {
  df <- data.frame(x = 1:3)
  expect_silent(DialogaR:::.check_join_count(df, df, "join de prueba", "error_test"))
})

test_that(".check_join_count: abort cuando el join multiplica filas", {
  df_antes  <- data.frame(x = 1:3)
  df_despues <- data.frame(x = 1:6)
  expect_error(
    DialogaR:::.check_join_count(df_antes, df_despues, "join multiplicado", "error_test"),
    "multiplicó filas"
  )
})

test_that(".check_join_count: abort cuando el join pierde filas", {
  df_antes  <- data.frame(x = 1:5)
  df_despues <- data.frame(x = 1:3)
  expect_error(
    DialogaR:::.check_join_count(df_antes, df_despues, "join con pérdida", "error_test"),
    "perdió filas"
  )
})
