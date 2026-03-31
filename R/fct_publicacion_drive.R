#' Upload Objects to Google Drive
#'
#' @description
#' A universal loader function that identifies the class of an R object
#' (Data Frame, named list of Data Frames, openxlsx Workbook, or officer PPTX),
#' saves it to a temporary local file, and uploads it to a specified Google Drive folder.
#'
#' If a named list of data frames is provided, it will automatically be exported
#' as a multi-sheet Excel workbook where the list names become the sheet names.
#'
#' @param carpeta_drive Character. The Google Drive folder ID or URL.
#' @param objeto The R object to upload (data.frame, named list of data.frames, openxlsx Workbook, or rpptx).
#' @param nombre Character. The base name for the file (e.g., "reporte_semanal").
#' @param corte Date or Character. The cutoff date to append to the filename.
#'
#' @return A `dribble` object representing the uploaded file on Google Drive.
#'
#' @importFrom writexl write_xlsx
#' @importFrom openxlsx saveWorkbook
#' @importFrom googledrive drive_upload as_id
#' @export
subida <- function(carpeta_drive, objeto, nombre, corte) {
  # 1. Identify if the object is a single data.frame OR a list of data.frames
  is_df_or_list <- is.data.frame(objeto) ||
    (is.list(objeto) &&
      all(vapply(objeto, is.data.frame, FUN.VALUE = logical(1))))

  # 2. Set up the temporary file based on object type
  if (is_df_or_list) {
    nombre_archivo <- paste0(nombre, "_", corte, ".xlsx")
    ruta_temporal <- file.path(tempdir(), nombre_archivo)
    writexl::write_xlsx(objeto, ruta_temporal)
  } else if (inherits(objeto, "Workbook")) {
    nombre_archivo <- paste0(nombre, "_", corte, ".xlsx")
    ruta_temporal <- file.path(tempdir(), nombre_archivo)
    openxlsx::saveWorkbook(objeto, file = ruta_temporal, overwrite = TRUE)
  } else if (inherits(objeto, "rpptx")) {
    nombre_archivo <- paste0(nombre, "_", corte, ".pptx")
    ruta_temporal <- file.path(tempdir(), nombre_archivo)
    print(objeto, target = ruta_temporal)
  } else {
    stop(
      "El objeto debe ser un Data Frame, una lista de Data Frames, un Workbook de openxlsx o una presentación PPTX de officer."
    )
  }

  # 3. Upload to Google Drive
  archivo_subido <- googledrive::drive_upload(
    media = ruta_temporal,
    path = googledrive::as_id(carpeta_drive),
    name = nombre_archivo,
    overwrite = TRUE
  )

  message("File successfully uploaded: ", nombre_archivo)

  # 4. Clean up the temporary file
  unlink(ruta_temporal)

  return(archivo_subido)
}
