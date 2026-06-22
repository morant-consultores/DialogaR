# tests/testthat/test-fct_paseLista.R

# =========================================================================
# HELPER: In-memory DB with the tables used by fct_paseLista
# =========================================================================
setup_mock_db_pl <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Usuarios: 1 supervisor (IdCargo=37), 1 active vocero, 1 inactive vocero
  DBI::dbWriteTable(con, "Usuarios", tibble::tibble(
    Id        = c(1L, 2L, 3L),
    IdProyecto = c(17L, 17L, 17L),
    Num        = c("S01", "V01", "V02"),
    Nombre     = c("Ana", "Luis", "Marta"),
    APaterno   = c("Garcia", "Perez", "Lopez"),
    AMaterno   = c("X", "Y", "Z"),
    Status     = c(1L, 1L, 0L),   # SQLite integers; 0 = inactive
    IdCargo    = c(37L, 99L, 99L)
  ))

  DBI::dbWriteTable(con, "Brigadas", tibble::tibble(
    Id            = 10L,
    IdProyecto    = 17L,
    NombreBrigada = "Brigada Alpha"
  ))

  # Luis (2) and Marta (3) are both subordinates of Ana (1)
  DBI::dbWriteTable(con, "UsuarioLog", tibble::tibble(
    IdProyecto   = c(17L, 17L),
    IdUsuario    = c(2L, 3L),
    IdSupervisor = c(1L, 1L),
    IdBrigada    = c(10L, 10L),
    FechaInsert  = c("2026-03-01", "2026-03-01")
  ))

  # BrigadasLog: fuente v0.3.0 del coordinador vigente por brigada (LOCF).
  # Brigada 10 está coordinada por Ana (IdUsuario = 1) al corte.
  DBI::dbWriteTable(con, "BrigadasLog", tibble::tibble(
    IdProyecto      = 17L,
    IdHistorico     = 1L,
    BrigadaId       = 10L,
    NombreBrigada   = "Brigada Alpha",
    IdUsuario       = 1L,
    IdZonaDeTrabajo = 1L,
    IdGrupo         = 1L,
    Activo          = 1L,
    FechaInsert     = "2026-03-01 00:00:00"
  ))

  # Ana (1) is linked to dialogo survey 292
  DBI::dbWriteTable(con, "UsuariosEncuesta", tibble::tibble(
    UsuarioId  = 1L,
    EncuestaId = 292L
  ))

  return(con)
}

# Minimal SurveyJS JSON with the 3 required pages: Inicial, 0 (template), Final
pase_lista_json <- function() {
  jsonlite::toJSON(
    list(
      title = "Pase de Lista",
      pages = list(
        list(
          name     = "Inicial",
          elements = list(list(
            type    = "radiogroup",
            name    = "Obtener_usuario",
            title   = "Selecciona tu nombre",
            choices = list()
          ))
        ),
        list(
          name     = "0",
          visible  = FALSE,
          elements = list(list(
            type      = "radiogroup",
            name      = "asistencia_0",
            title     = "Asistencia 0",
            visibleIf = "{Obtener_usuario} = '0'"
          ))
        ),
        list(
          name     = "Final",
          elements = list(list(type = "text", name = "finalizar"))
        )
      )
    ),
    auto_unbox = TRUE
  ) |> as.character()
}

# =========================================================================
# TESTS: construir_base_operativa_pl
# =========================================================================

test_that("construir_base_operativa_pl incluye voceros activos y coordinador como vocero", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  resultado <- DialogaR:::construir_base_operativa_pl(
    pool                  = con,
    id_proyecto           = 17L,
    ids_encuestas_dialogo = c(292L, 2923L),
    id_cargo_supervisor   = 37L
  )

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 2)  # Luis (vocero) + Ana (coordinador como vocero)
  expect_true(all(c("vocero", "nombre_vocero", "nombre_coordinador") %in% names(resultado)))
  expect_true("V01" %in% resultado$vocero)  # Luis aparece como vocero
  expect_true("S01" %in% resultado$vocero)  # Ana aparece como coordinador-vocero
})

test_that("construir_base_operativa_pl excluye voceros con Status = FALSE", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  resultado <- DialogaR:::construir_base_operativa_pl(
    pool                  = con,
    id_proyecto           = 17L,
    ids_encuestas_dialogo = c(292L, 2923L),
    id_cargo_supervisor   = 37L
  )

  # Marta (V02) está inactiva — no debe aparecer
  expect_false("V02" %in% resultado$vocero)
})

test_that("construir_base_operativa_pl excluye coordinador no asignado a encuestas dialogo", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  # Reemplazar la asignación de Ana a una encuesta que NO está en el filtro
  DBI::dbExecute(con, "DELETE FROM UsuariosEncuesta")
  DBI::dbWriteTable(con, "UsuariosEncuesta",
    tibble::tibble(UsuarioId = 1L, EncuestaId = 999L),
    append = TRUE
  )

  resultado <- DialogaR:::construir_base_operativa_pl(
    pool                  = con,
    id_proyecto           = 17L,
    ids_encuestas_dialogo = c(292L, 2923L),
    id_cargo_supervisor   = 37L
  )

  # Solo Luis queda; Ana no aparece como coordinador-vocero
  expect_equal(nrow(resultado), 1)
  expect_false("S01" %in% resultado$vocero)
})

# =========================================================================
# TESTS: extraer_json_molde
# =========================================================================

test_that("extraer_json_molde retorna lista con json_crudo y tabla_paginas correctos", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Encuesta", tibble::tibble(
    Id       = 294L,
    JsonData = pase_lista_json(),
    Version  = 1L
  ))

  resultado <- DialogaR:::extraer_json_molde(pool = con, id_pase_lista = 294L)

  expect_type(resultado, "list")
  expect_named(resultado, c("json_crudo", "tabla_paginas"))
  expect_true(all(c("name", "elements") %in% names(resultado$tabla_paginas)))
  expect_true(all(c("Inicial", "0", "Final") %in% resultado$tabla_paginas$name))
})

# =========================================================================
# TESTS: actualizar_pase_lista (orquestador)
# =========================================================================

test_that("actualizar_pase_lista trata Version = NULL como 0 y escribe Version = 1", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Encuesta", tibble::tibble(
    Id                = 294L,
    JsonData          = pase_lista_json(),
    Version           = NA_integer_,   # NULL en la BD → el bug reportado
    FechaModificacion = NA_character_
  ))

  # Mockear los helpers internos para aislar la lógica del orquestador
  testthat::local_mocked_bindings(
    construir_base_operativa_pl = function(...) {
      tibble::tibble(
        IdSupervisor     = 1L,  IdUsuario = 2L,    IdBrigada = 10L,
        NombreBrigada    = "Alpha",
        nombre_coordinador = "ANA GARCIA X",      supervisor = "S01",
        status_supervisor  = 1L,
        nombre_vocero    = "LUIS PEREZ Y",         vocero = "V01",
        status_vocero    = 1L,
        IdCargoSupervisor = 37L,                   IdCargoVocero = 99L
      )
    },
    extraer_json_molde = function(...) {
      list(
        json_crudo    = '{"title":"test"}',
        tabla_paginas = tibble::tibble(
          id      = c(0, 1, 2),
          name    = c("Inicial", "0", "Final"),
          visible = c(NA, FALSE, NA),
          elements = list(
            tibble::tibble(type = "radiogroup", name = "Obtener_usuario", title = "Sup"),
            tibble::tibble(type = "radiogroup", name = "asistencia_0",   title = "Asistencia 0", visibleIf = "{Obtener_usuario} = ''0''"),
            tibble::tibble(type = "text",       name = "finalizar")
          )
        )
      )
    },
    generar_paginas_dinamicas = function(...) list('{"name":"V01"}'),
    ensamblar_json_final      = function(...) '{"title":"updated","pages":[]}',
    .package = "DialogaR"
  )

  resultado <- actualizar_pase_lista(
    pool                  = con,
    id_proyecto           = 17L,
    id_pase_lista         = 294L,
    ids_encuestas_dialogo = c(292L, 2923L),
    dir_backup            = tempdir()
  )

  expect_true(resultado)

  version_nueva <- DBI::dbGetQuery(
    con, "SELECT Version FROM Encuesta WHERE Id = 294"
  )$Version
  expect_equal(version_nueva, 1L)
})

test_that("actualizar_pase_lista incrementa una Version existente correctamente", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Encuesta", tibble::tibble(
    Id                = 294L,
    JsonData          = pase_lista_json(),
    Version           = 5L,
    FechaModificacion = NA_character_
  ))

  testthat::local_mocked_bindings(
    construir_base_operativa_pl = function(...) {
      tibble::tibble(
        IdSupervisor = 1L, IdUsuario = 2L, IdBrigada = 10L,
        NombreBrigada = "Alpha", nombre_coordinador = "ANA GARCIA X",
        supervisor = "S01", status_supervisor = 1L,
        nombre_vocero = "LUIS PEREZ Y", vocero = "V01",
        status_vocero = 1L, IdCargoSupervisor = 37L, IdCargoVocero = 99L
      )
    },
    extraer_json_molde = function(...) {
      list(
        json_crudo    = '{"title":"test"}',
        tabla_paginas = tibble::tibble(
          id = c(0, 1, 2), name = c("Inicial", "0", "Final"),
          visible = c(NA, FALSE, NA),
          elements = list(
            tibble::tibble(type = "radiogroup", name = "Obtener_usuario", title = "Sup"),
            tibble::tibble(type = "radiogroup", name = "asistencia_0", title = "Asistencia 0", visibleIf = "{Obtener_usuario} = ''0''"),
            tibble::tibble(type = "text", name = "finalizar")
          )
        )
      )
    },
    generar_paginas_dinamicas = function(...) list('{"name":"V01"}'),
    ensamblar_json_final      = function(...) '{"title":"updated","pages":[]}',
    .package = "DialogaR"
  )

  actualizar_pase_lista(
    pool = con, id_proyecto = 17L, id_pase_lista = 294L,
    ids_encuestas_dialogo = c(292L, 2923L), dir_backup = tempdir()
  )

  version_nueva <- DBI::dbGetQuery(
    con, "SELECT Version FROM Encuesta WHERE Id = 294"
  )$Version
  expect_equal(version_nueva, 6L)
})

test_that("actualizar_pase_lista preserva comillas simples internas y neutraliza inyeccion SQL", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  # Fila objetivo + fila centinela para detectar un DROP/DELETE inyectado
  DBI::dbWriteTable(con, "Encuesta", tibble::tibble(
    Id                = c(294L, 295L),
    JsonData          = c(pase_lista_json(), pase_lista_json()),
    Version           = c(2L, 9L),
    FechaModificacion = c(NA_character_, NA_character_)
  ))

  # JSON válido con comillas simples internas y una carga de inyección SQL.
  # Con glue::glue() literal, la apostrofe cerraba la cadena SQL y corrompía
  # el blob; con glue_sql() debe persistir verbatim.
  json_malicioso <- "{\"brigada\":\"O'Brien's\",\"inject\":\"'); DROP TABLE Encuesta; --\",\"pages\":[]}"

  testthat::local_mocked_bindings(
    construir_base_operativa_pl = function(...) {
      tibble::tibble(
        IdSupervisor = 1L, IdUsuario = 2L, IdBrigada = 10L,
        NombreBrigada = "Alpha", nombre_coordinador = "ANA GARCIA X",
        supervisor = "S01", status_supervisor = 1L,
        nombre_vocero = "LUIS PEREZ Y", vocero = "V01",
        status_vocero = 1L, IdCargoSupervisor = 37L, IdCargoVocero = 99L
      )
    },
    extraer_json_molde = function(...) {
      list(
        json_crudo    = '{"title":"test"}',
        tabla_paginas = tibble::tibble(
          id = c(0, 1, 2), name = c("Inicial", "0", "Final"),
          visible = c(NA, FALSE, NA),
          elements = list(
            tibble::tibble(type = "radiogroup", name = "Obtener_usuario", title = "Sup"),
            tibble::tibble(type = "radiogroup", name = "asistencia_0", title = "Asistencia 0", visibleIf = "{Obtener_usuario} = ''0''"),
            tibble::tibble(type = "text", name = "finalizar")
          )
        )
      )
    },
    generar_paginas_dinamicas = function(...) list('{"name":"V01"}'),
    ensamblar_json_final      = function(...) json_malicioso,
    .package = "DialogaR"
  )

  actualizar_pase_lista(
    pool = con, id_proyecto = 17L, id_pase_lista = 294L,
    ids_encuestas_dialogo = c(292L, 2923L), dir_backup = tempdir()
  )

  # La tabla y la fila centinela sobreviven: no hubo inyección
  expect_true(DBI::dbExistsTable(con, "Encuesta"))
  expect_equal(
    DBI::dbGetQuery(con, "SELECT Version FROM Encuesta WHERE Id = 295")$Version,
    9L
  )

  # El JSON se almacenó intacto, comillas incluidas
  fila <- DBI::dbGetQuery(con, "SELECT JsonData, Version FROM Encuesta WHERE Id = 294")
  expect_equal(fila$JsonData, json_malicioso)
  expect_equal(fila$Version, 3L)
})

test_that("actualizar_pase_lista aborta sin ejecutar UPDATE cuando el JSON es invalido", {
  con <- setup_mock_db_pl()
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(con, "Encuesta", tibble::tibble(
    Id                = 294L,
    JsonData          = pase_lista_json(),
    Version           = 5L,
    FechaModificacion = NA_character_
  ))

  testthat::local_mocked_bindings(
    construir_base_operativa_pl = function(...) {
      tibble::tibble(
        IdSupervisor = 1L, IdUsuario = 2L, IdBrigada = 10L,
        NombreBrigada = "Alpha", nombre_coordinador = "ANA GARCIA X",
        supervisor = "S01", status_supervisor = 1L,
        nombre_vocero = "LUIS PEREZ Y", vocero = "V01",
        status_vocero = 1L, IdCargoSupervisor = 37L, IdCargoVocero = 99L
      )
    },
    extraer_json_molde = function(...) {
      list(
        json_crudo    = '{"title":"test"}',
        tabla_paginas = tibble::tibble(
          id = c(0, 1, 2), name = c("Inicial", "0", "Final"),
          visible = c(NA, FALSE, NA),
          elements = list(
            tibble::tibble(type = "radiogroup", name = "Obtener_usuario", title = "Sup"),
            tibble::tibble(type = "radiogroup", name = "asistencia_0", title = "Asistencia 0", visibleIf = "{Obtener_usuario} = ''0''"),
            tibble::tibble(type = "text", name = "finalizar")
          )
        )
      )
    },
    generar_paginas_dinamicas = function(...) list('{"name":"V01"}'),
    ensamblar_json_final      = function(...) '{"title": roto, "pages":',
    .package = "DialogaR"
  )

  expect_error(
    actualizar_pase_lista(
      pool = con, id_proyecto = 17L, id_pase_lista = 294L,
      ids_encuestas_dialogo = c(292L, 2923L), dir_backup = tempdir()
    ),
    regexp = "inválido"
  )

  # El UPDATE no debe haberse ejecutado: Version y JsonData intactos
  fila <- DBI::dbGetQuery(con, "SELECT JsonData, Version FROM Encuesta WHERE Id = 294")
  expect_equal(fila$Version, 5L)
  expect_equal(fila$JsonData, pase_lista_json())
})
