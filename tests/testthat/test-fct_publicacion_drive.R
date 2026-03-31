# tests/testthat/test-subida.R

test_that("subida falla inmediatamente si recibe un objeto no soportado", {
  # Un vector numérico simple no está soportado
  objeto_invalido <- c(1, 2, 3)

  expect_error(
    subida(
      carpeta_drive = "fake_folder",
      objeto = objeto_invalido,
      nombre = "test_fail",
      corte = "2026-03-27"
    ),
    "El objeto debe ser" # Espera que el mensaje de error contenga este fragmento
  )
})

test_that("subida formatea correctamente una lista de data.frames y ejecuta la subida", {
  # 1. Configurar datos simulados (Una lista nombrada de data frames)
  mock_lista <- list(
    "Hoja1" = data.frame(a = 1:3),
    "Hoja2" = data.frame(b = letters[1:3])
  )

  # 2. EL MOCK: Interceptar googledrive::drive_upload
  testthat::local_mocked_bindings(
    drive_upload = function(media, path, name, overwrite) {
      # Ponemos nuestras aserciones ¡DENTRO del mock!
      # Verificamos si 'subida' creó el archivo temporal antes de llamar a la API
      expect_true(file.exists(media))

      # Verificamos si lo nombró correctamente según nuestras reglas
      expect_equal(name, "reporte_test_2026-03-27.xlsx")

      # Retornamos un objeto 'dribble' falso para satisfacer a la función
      return(data.frame(name = name, id = "fake_google_drive_id_123"))
    },
    .package = "googledrive"
  )

  # 3. Ejecutar la función
  resultado <- subida(
    carpeta_drive = "mi_carpeta_drive",
    objeto = mock_lista,
    nombre = "reporte_test",
    corte = as.Date("2026-03-27")
  )

  # 4. Aserciones finales
  expect_equal(resultado$id, "fake_google_drive_id_123")
})

test_that("subida formatea correctamente un Workbook de openxlsx y ejecuta la subida", {
  # 1. Configurar datos simulados (Workbook)
  mock_wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(mock_wb, "HojaPrueba")
  openxlsx::writeData(mock_wb, "HojaPrueba", data.frame(x = 1))

  # 2. EL MOCK
  testthat::local_mocked_bindings(
    drive_upload = function(media, path, name, overwrite) {
      expect_true(file.exists(media))
      expect_equal(name, "wb_test_2026-03-27.xlsx")
      return(data.frame(id = "fake_wb_id"))
    },
    .package = "googledrive"
  )

  # 3. Ejecutar
  resultado <- subida(
    carpeta_drive = "mi_carpeta_drive",
    objeto = mock_wb,
    nombre = "wb_test",
    corte = "2026-03-27"
  )

  # 4. Aserciones finales
  expect_equal(resultado$id, "fake_wb_id")
})
