#' Copia una plantilla de script al directorio de trabajo
#'
#' @description
#' Copia uno de los scripts de ejemplo incluidos en el paquete al directorio
#' de trabajo actual, listo para ser adaptado al proyecto.
#'
#' @param plantilla Nombre de la plantilla. Opciones disponibles:
#'   `"chihuahua"`, `"sonora"`, `"pase_lista"`, `"productividad_chihuahua"`.
#'   Si se omite, imprime las opciones disponibles.
#' @param destino Nombre del archivo de destino. Por defecto usa el nombre
#'   original del script.
#' @param sobreescribir Logical. Si `TRUE`, sobreescribe el archivo si ya existe.
#'   Por defecto `FALSE`.
#'
#' @return La ruta del archivo copiado, de forma invisible.
#'
#' @examples
#' \dontrun{
#' usar_plantilla("sonora")
#' usar_plantilla("chihuahua", destino = "mi_proyecto_chih.R")
#' }
#'
#' @export
usar_plantilla <- function(plantilla, destino = NULL, sobreescribir = FALSE) {
  plantillas <- c(
    chihuahua              = "scripts/03_run_chihuahua_insumos.R",
    sonora                 = "scripts/04_run_sonora_insumos.R",
    pase_lista             = "scripts/02_run_pase_lista.R",
    productividad_chihuahua = "scripts/01_run_productividad_chihuahua.R"
  )

  if (missing(plantilla)) {
    cli::cli_inform(c(
      "i" = "Plantillas disponibles:",
      " " = paste0("{.val ", names(plantillas), "}", collapse = ", ")
    ))
    return(invisible(NULL))
  }

  if (!plantilla %in% names(plantillas)) {
    cli::cli_abort(c(
      "x" = "Plantilla {.val {plantilla}} no encontrada.",
      "i" = "Opciones: {.val {names(plantillas)}}"
    ))
  }

  origen <- system.file(plantillas[[plantilla]], package = "DialogaR")
  if (!nzchar(origen)) {
    cli::cli_abort("No se encontró el archivo de la plantilla en el paquete instalado.")
  }

  if (is.null(destino)) {
    destino <- basename(origen)
  }

  ruta_destino <- file.path(getwd(), destino)

  if (file.exists(ruta_destino) && !sobreescribir) {
    cli::cli_abort(c(
      "x" = "El archivo {.file {destino}} ya existe.",
      "i" = "Usa {.code sobreescribir = TRUE} para reemplazarlo."
    ))
  }

  file.copy(origen, ruta_destino, overwrite = sobreescribir)
  cli::cli_alert_success("Plantilla copiada a {.file {ruta_destino}}")
  invisible(ruta_destino)
}

coerce_numeric_candidates <- function(
  df,
  exclude = c(
    "id",
    "fecha",
    "obtener_usuario",
    "usuario",
    "vocero",
    "supervisor",
    "asistencia",
    "finalizar",
    "observaciones"
  )
) {
  df |>
    mutate(across(
      .cols = where(is.character) & !any_of(exclude),
      .fns = ~ {
        x <- stringr::str_squish(.x)
        # Solo intentar si hay al menos un valor no-NA
        if (all(is.na(x))) {
          return(x)
        }

        # Normalización mínima: quitar comas de miles (si existieran)
        x2 <- stringr::str_replace_all(x, ",", "")

        # ¿Qué tan "numérica" es la columna? (permitimos "", NA)
        ok <- is.na(x2) |
          x2 == "" |
          stringr::str_detect(x2, "^[+-]?[0-9]+(\\.[0-9]+)?$")
        ratio_ok <- mean(ok)

        if (ratio_ok >= 0.95) {
          suppressWarnings(as.numeric(dplyr::na_if(x2, "")))
        } else {
          .x
        }
      }
    ))
}

#' Autentica con Google Drive usando una Service Account
#'
#' @description
#' Lee las credenciales JSON desde la variable de entorno `drive_json`
#' (codificada en Base64), las escribe en un archivo temporal y autentica
#' la sesión de `googledrive` con esa Service Account.
#'
#' @return Invisible. Efecto secundario: sesión de `googledrive` autenticada.
#'
#' @section Seguridad y Privacidad:
#' Las credenciales se leen desde una variable de entorno y nunca se escriben
#' a disco de forma permanente. El archivo temporal es eliminado por el sistema
#' operativo al finalizar la sesión de R.
#'
#' @export
autenticar_googledrive <- function() {
  json_creds_string <- rawToChar(base64enc::base64decode(Sys.getenv("drive_json")))

  temp_json_path <- tempfile(fileext = ".json")
  writeLines(json_creds_string, temp_json_path)

  googledrive::drive_auth(path = temp_json_path)
}

#' Conectar a la Base de Datos SQL Server (Pool)
#'
#' @description
#' Establece una conexión segura a la base de datos SQL Server leyendo las credenciales
#' directamente desde las variables de entorno (`.Renviron`).
#' Detecta automáticamente el entorno de ejecución (Local vs. Positron Connect)
#' para asignar el driver ODBC correcto.
#'
#' @details
#' **Selección Automática de Driver:**
#' \itemize{
#'   \item Si existe la variable de entorno `pool_driver`, utiliza esa.
#'   \item Si el Sistema Operativo es Windows o Mac (Darwin), utiliza `ODBC Driver 17 for SQL Server`.
#'   \item Si el Sistema Operativo es Linux (Positron Connect), utiliza `FreeTDS`.
#' }
#'
#' **Cumplimiento ISO 27001:** Previene la exposición de credenciales mediante el uso
#' estricto de variables de entorno.
#'
#' @return Un objeto de clase `Pool` listo para ser utilizado con `dplyr` o `DBI`.
#'
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#' @export
conectar_base_datos <- function() {
  # 1. Leer credenciales seguras desde el entorno
  server <- Sys.getenv("pool_server")
  database <- Sys.getenv("pool_database")
  uid <- Sys.getenv("pool_uid")
  pwd <- Sys.getenv("pool_pwd")

  # 2. Lógica Inteligente de Selección de Driver
  driver_env <- Sys.getenv("pool_driver")
  os_name <- Sys.info()[["sysname"]]

  if (driver_env != "") {
    # Prioridad 1: Variable de entorno explícita
    driver_usar <- driver_env
  } else if (os_name %in% c("Windows", "Darwin")) {
    # Prioridad 2: Entorno Local (Windows o Mac)
    driver_usar <- "ODBC Driver 17 for SQL Server"
  } else {
    # Prioridad 3: Positron Connect o Servidores Linux
    driver_usar <- "FreeTDS"
  }

  # 3. Fail-fast de Seguridad
  if (server == "" || uid == "" || pwd == "") {
    stop(
      "Error de Seguridad: Faltan credenciales de base de datos. ",
      "Asegúrate de que 'pool_server', 'pool_database', 'pool_uid' y 'pool_pwd' ",
      "estén definidos en tu archivo .Renviron o en las variables de Positron Connect."
    )
  }

  # 4. Crear el pool de conexiones dinámico
  pool_obj <- pool::dbPool(
    drv = odbc::odbc(),
    Driver = driver_usar,
    Database = database,
    Server = server,
    UID = uid,
    PWD = pwd,
    Port = 1433,
    timeout = 120
  )

  return(pool_obj)
}
