# tests/testthat/test-fct_contactos.R

# ------------------------------------------------------------
# Fixtures
# ------------------------------------------------------------

make_bd_aux <- function() {
  tibble::tibble(
    vocero             = c(1L, 2L, 3L),
    nombre_vocero      = c("Ana Lopez", "Carlos Perez", NA_character_),
    nombre_coordinador = c("Coord Norte", "Coord Norte", "Coord Sur")
  )
}

make_bd_completa <- function(
  n_rows        = 3,
  conoce_col    = "conoce_garcia",   # NULL → usa genéricas
  opinion_col   = "opinion_garcia",
  partido_vals  = rep("PAN", 3),
  celular_vals  = c("5550000001", NA_character_, "5550000003"),
  correo_vals   = c(NA_character_, "b@mail.com", "c@mail.com"),
  fecha_vals    = rep(as.Date("2026-03-01"), 3)
) {
  base <- tibble::tibble(
    id                  = 1:n_rows,
    seccion             = c("1", "2", "3")[seq_len(n_rows)],
    usuario_num         = rep(1L, n_rows),
    nombre_entrevistado = c("Juan", "Ana", "Pedro")[seq_len(n_rows)],
    apellido_paterno    = c("Solis", "Ruiz", "Vega")[seq_len(n_rows)],
    apellido_materno    = c("Torres", "Luna", "Cruz")[seq_len(n_rows)],
    sexo                = "M",
    edad                = 30L,
    fecha               = fecha_vals[seq_len(n_rows)],
    celular             = celular_vals[seq_len(n_rows)],
    correo              = correo_vals[seq_len(n_rows)],
    grupo_whats         = NA_character_,
    direccion_calle     = "Reforma",
    direccion_num_ext   = "10",
    direccion_num_int   = NA_character_,
    partido             = partido_vals[seq_len(n_rows)]
  )

  if (!is.null(conoce_col)) {
    base[[conoce_col]]  <- c("Sí", "No", "Sí")[seq_len(n_rows)]
    base[[opinion_col]] <- c("Buena", "Buena", "Mala")[seq_len(n_rows)]
  } else {
    base[["personaje_conocimiento"]] <- c("Sí", "No", "Sí")[seq_len(n_rows)]
    base[["personaje_opinion"]]      <- c("Buena", "Buena", "Mala")[seq_len(n_rows)]
  }

  base
}

# ------------------------------------------------------------
# 1. Flujo básico: columnas dinámicas (conoce_X / opinion_X)
# ------------------------------------------------------------

test_that("construir_base_contactos devuelve df con columnas esperadas (cols dinámicas)", {
  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = make_bd_completa(),
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  expect_s3_class(res, "data.frame")
  expect_true(all(c("id", "nombre", "celular", "correo",
                    "personaje_conocimiento", "personaje_opinion") %in% names(res)))
})

# ------------------------------------------------------------
# 2. Fallback a columnas genéricas
# ------------------------------------------------------------

test_that("construir_base_contactos usa columnas genéricas cuando no existen las dinámicas", {
  bd <- make_bd_completa(conoce_col = NULL) # sin columnas dinámicas
  res <- construir_base_contactos(
    personaje   = lopez,
    bd_completa = bd,
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  expect_true("personaje_conocimiento" %in% names(res))
  expect_true("personaje_opinion" %in% names(res))
})

# ------------------------------------------------------------
# 3. Error cuando no existen ni dinámicas ni genéricas
# ------------------------------------------------------------

test_that("construir_base_contactos lanza error si faltan columnas de conocimiento/opinión", {
  bd <- make_bd_completa() |>
    dplyr::select(-dplyr::starts_with("conoce_"), -dplyr::starts_with("opinion_"))

  expect_error(
    construir_base_contactos(
      personaje   = xyz,
      bd_completa = bd,
      corte       = as.Date("2026-03-31"),
      bd_aux      = make_bd_aux()
    ),
    regexp = "No encontré columnas"
  )
})

# ------------------------------------------------------------
# 4. Filtro por fecha <= corte
# ------------------------------------------------------------

test_that("construir_base_contactos excluye registros posteriores al corte", {
  bd <- make_bd_completa(
    fecha_vals = c(as.Date("2026-02-01"), as.Date("2026-04-01"), as.Date("2026-02-15"))
  )
  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = bd,
    corte       = as.Date("2026-03-01"),
    bd_aux      = make_bd_aux()
  )

  expect_true(all(res$fecha <= as.Date("2026-03-01")))
  expect_false(as.Date("2026-04-01") %in% res$fecha)
})

# ------------------------------------------------------------
# 5. Solo se conservan contactos con correo O celular
# ------------------------------------------------------------

test_that("construir_base_contactos excluye filas sin celular ni correo", {
  bd <- make_bd_completa(
    celular_vals = c("5550000001", NA_character_, NA_character_),
    correo_vals  = c(NA_character_, "b@mail.com", NA_character_)
  )
  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = bd,
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  # id=3 no tiene ni correo ni celular → excluido
  expect_equal(nrow(res), 2L)
  expect_false(3L %in% res$id)
})

# ------------------------------------------------------------
# 6. partido = NULL: sin columna partido en el resultado
# ------------------------------------------------------------

test_that("construir_base_contactos no incluye columna partido cuando partido = NULL", {
  res <- construir_base_contactos(
    personaje   = garcia,
    partido     = NULL,
    bd_completa = make_bd_completa(),
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  expect_false("partido" %in% names(res))
})

# ------------------------------------------------------------
# 7. clasificacion = TRUE, partido = NULL → error
# ------------------------------------------------------------

test_that("construir_base_contactos lanza error si clasificacion=TRUE y partido=NULL", {
  expect_error(
    construir_base_contactos(
      personaje     = garcia,
      partido       = NULL,
      bd_completa   = make_bd_completa(),
      corte         = as.Date("2026-03-31"),
      bd_aux        = make_bd_aux(),
      clasificacion = TRUE
    ),
    regexp = "clasificacion = TRUE"
  )
})

# ------------------------------------------------------------
# 8. clasificacion = FALSE: sin columnas grupo/categoria
# ------------------------------------------------------------

test_that("construir_base_contactos no agrega grupo/categoria si clasificacion=FALSE", {
  res <- construir_base_contactos(
    personaje     = garcia,
    partido       = partido,
    bd_completa   = make_bd_completa(),
    corte         = as.Date("2026-03-31"),
    bd_aux        = make_bd_aux(),
    clasificacion = FALSE
  )

  expect_false("grupo"     %in% names(res))
  expect_false("categoria" %in% names(res))
})

# ------------------------------------------------------------
# 9. clasificacion = TRUE: grupos correctos
# ------------------------------------------------------------

test_that("construir_base_contactos clasifica contactos en los 6 grupos correctamente", {
  # El argumento `partido` es NSE para el NOMBRE de columna en bd_completa.
  # `lbl_partido` = ese nombre → la comparación es `valor_columna == "PAN"`.
  # Por eso la columna debe llamarse "PAN" y contener "PAN" para simpatizantes
  # o cualquier otro valor para los demás.
  bd <- tibble::tibble(
    id                  = 1:6,
    fecha               = rep(as.Date("2026-03-01"), 6),
    seccion             = c("1","2","3","1","2","3"),
    usuario_num         = rep(1L, 6),
    nombre_entrevistado = paste0("Persona", 1:6),
    apellido_paterno    = paste0("Ap", 1:6),
    apellido_materno    = paste0("Am", 1:6),
    sexo                = "M",
    edad                = 30L,
    celular             = paste0("555000000", 1:6),
    correo              = NA_character_,
    grupo_whats         = NA_character_,
    direccion_calle     = "Calle",
    direccion_num_ext   = "1",
    direccion_num_int   = NA_character_,
    PAN                 = c("PAN","PAN","PAN","PRI","PRI","PRI"),
    conoce_garcia       = c("Sí","No","Sí","Sí","Sí","No"),
    opinion_garcia      = c("Buena",NA_character_,"Mala","Buena","Mala",NA_character_)
  )

  res <- construir_base_contactos(
    personaje     = garcia,
    partido       = PAN,
    bd_completa   = bd,
    corte         = as.Date("2026-03-31"),
    bd_aux        = make_bd_aux(),
    clasificacion = TRUE
  )

  expect_true(all(c("grupo", "categoria") %in% names(res)))

  grupos <- res$grupo[order(res$id)]
  expect_equal(grupos, c("1", "2", "3", "4", "5", "6"))
})

# ------------------------------------------------------------
# 10. Normalización de nombres: ASCII + mayúsculas
# ------------------------------------------------------------

test_that("construir_base_contactos convierte nombres a ASCII mayúsculas", {
  bd <- make_bd_completa(n_rows = 1)
  bd$nombre_entrevistado <- "José"
  bd$apellido_paterno    <- "García"
  bd$apellido_materno    <- "Ñoño"

  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = bd,
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  expect_equal(res$nombre, "JOSE GARCIA NONO")
})

# ------------------------------------------------------------
# 11. NA → na_chr en columnas de carácter
# ------------------------------------------------------------

test_that("construir_base_contactos reemplaza NA de carácter con na_chr", {
  bd <- make_bd_completa(
    celular_vals = c("5551234567", NA_character_, "5559876543")
  )

  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = bd,
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux(),
    na_chr      = "-"
  )

  # correo del id=1 era NA → debe ser "-"
  expect_equal(res$correo[res$id == 1L], "-")
  # na_chr personalizado
  res2 <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = bd,
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux(),
    na_chr      = "N/D"
  )
  expect_equal(res2$correo[res2$id == 1L], "N/D")
})

# ------------------------------------------------------------
# 12. Prefijo direccion_ eliminado en columnas de dirección
# ------------------------------------------------------------

test_that("construir_base_contactos elimina prefijo 'direccion_' de columnas de dirección", {
  res <- construir_base_contactos(
    personaje   = garcia,
    bd_completa = make_bd_completa(),
    corte       = as.Date("2026-03-31"),
    bd_aux      = make_bd_aux()
  )

  expect_true("calle"   %in% names(res))
  expect_true("num_ext" %in% names(res))
  expect_true("num_int" %in% names(res))
  expect_false("direccion_calle"   %in% names(res))
  expect_false("direccion_num_ext" %in% names(res))
})
