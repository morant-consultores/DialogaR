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

autenticar_googledrive <- function() {
  json_creds_string <- rawToChar(base64enc::base64decode(Sys.getenv(
    "drive_json"
  )))

  # Crear un archivo JSON temporal para la autenticación
  temp_json_path <- tempfile(fileext = ".json")
  writeLines(json_creds_string, temp_json_path)

  # 3. Autenticar usando la ruta al archivo JSON temporal
  # Para Service Accounts, esta es la función correcta
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
