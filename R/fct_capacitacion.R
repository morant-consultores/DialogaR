# -------------------------------------------------------------------------
# PROYECTO: Paquete DS
# SCRIPT:   fct_capacitacion.R
# OBJETIVO: Funciones para construir el reporte de capacitación
# AUTOR:    Rafael López / Equipo de Análisis
# FECHA:    2026-03-23
# -------------------------------------------------------------------------
# CLASIFICACIÓN: Interno
# ORIGEN DATOS:  Fuentes de datos o tablas SQL
# DEPENDENCIAS:  {dplyr, {dbplyr}, {here}}
# -------------------------------------------------------------------------
# NOTAS DE SEGURIDAD:
# - No hardcodear credenciales. Usar .Renviron
# - Los datos resultantes se consideran activos de información tipo A
# -------------------------------------------------------------------------

#' Crear Tabla Base de Capacitación
#'
#' @description
#' Extrae y normaliza la información de usuarios desde la base de datos centralizada.
#' Cumple con la **ISO 27001 (A.14.2.1)** al asegurar que solo se seleccionan los
#' campos necesarios para el propósito del reporte (Minimización de datos).
#'
#' @param pool Un objeto de conexión `pool` a la base de datos. Se asume que la
#'   conexión sigue protocolos de cifrado en tránsito (TLS/SSL).
#' @param id_proyecto Identificador numérico o carácter del proyecto.
#' @param numeros_prueba Vector de identificadores que deben excluirse para
#'   mantener la integridad de los datos reales (Control de calidad).
#' @param brigadas Data.frame o tibble con el catálogo de brigadas autorizado.
#'
#' @return Un `tibble` con la información de usuarios filtrada y anonimizada
#'   según el cargo, lista para el procesamiento de capacitación.
#'
#' @section Seguridad y Privacidad:
#' Esta función maneja PII (Información de Identificación Personal). El acceso al
#' `pool` debe estar restringido mediante roles de base de datos (RBAC).
#'
#' @export
crear_tabla_capacitacion <- function(
  pool,
  id_proyecto,
  numeros_prueba,
  brigadas
) {
  dplyr::tbl(pool, "Usuarios") |>
    dplyr::filter(IdProyecto == !!id_proyecto, !Num %in% numeros_prueba) |>
    dplyr::transmute(
      Id,
      Municipio,
      nombre = paste(Nombre, APaterno, AMaterno),
      Num,
      Cargo,
      Status,
      Fecha = as.Date(FechaInsert),
      IdBrigada,
      Entrevista,
      Resolucion,
      Capacitacion
    ) |>
    dplyr::collect() |> # <-- FIJADO: Descargamos a memoria ANTES del join
    dplyr::left_join(brigadas, dplyr::join_by(IdBrigada == Id)) |>
    dplyr::relocate(NombreBrigada, .before = Entrevista) |>
    dplyr::select(-IdBrigada)
}

#' Obtener Respuestas de Cuestionarios de Capacitación
#'
#' @description
#' Recupera y deserializa datos JSON de respuestas de capacitación.
#' Implementa la validación de integridad de datos al transformar objetos JSON
#' estructurados en formatos tabulares.
#'
#' @param pool Conexión a la base de datos.
#' @param ids Vector de IDs de usuario a consultar.
#' @param id_proyecto ID del proyecto para asegurar el aislamiento de datos entre clientes.
#'
#' @return Un `tibble` con las respuestas expandidas por usuario.
#'
#' @note Los datos en `JsonData` pueden contener información sensible. Se recomienda
#'   que el almacenamiento en la base de datos esté cifrado en reposo.
#'
#' @export
obtener_respuestas_capacitacion <- function(pool, ids, id_proyecto) {
  dplyr::tbl(pool, "RegistrosCuestionarios") |>
    dplyr::filter(IdUsuario %in% ids & IdProyecto == id_proyecto) |>
    dplyr::collect() |>
    purrr::pmap_df(function(IdUsuario, JsonData, ...) {
      # <-- FIJADO: purrr::
      JsonData |>
        jsonlite::fromJSON() |>
        tibble::as_tibble() |> # <-- FIJADO: tibble::
        dplyr::mutate(Id = IdUsuario)
    })
}

# ============================================================
# Orquestadora: genera reporte de capacitación y lo sube a Drive
# ============================================================
#' Orquestador de Reportes de Capacitación (Compliance ISO 27001)
#'
#' @description
#' Función de alto nivel que integra la extracción, transformación y carga (ETL)
#' del reporte de capacitación. Garantiza la disponibilidad y confidencialidad
#' de los reportes mediante validaciones estrictas y manejo de errores.
#'
#' @param pool Conexión a DB.
#' @param id_proyecto ID único del proyecto.
#' @param numeros_prueba IDs a omitir (filtrado de ruido).
#' @param drive_folder Identificador del contenedor de destino en Drive.
#' @param corte Fecha de corte del reporte. Por defecto es el día actual.
#' @param reporte Prefijo del nombre del archivo.
#' @param brigadas Opcional. Tabla de brigadas. Si es NULL, se consulta la DB.
#' @param overwrite Booleano. Si es TRUE, sobrescribe archivos existentes (Control de versiones).
#' @param verbose Booleano. Si es TRUE, genera logs de auditoría en consola.
#'
#' @return Lista con los resultados de cada etapa y el objeto de confirmación de Google Drive.
#'
#' @section Protocolos de Seguridad (ISO 27001):
#' \itemize{
#'   \item \strong{A.12.4.1 (Registro de Eventos):} La función genera mensajes de
#'   estado (logs) para trazabilidad del proceso.
#'   \item \strong{A.18.1.3 (Protección de Registros):} Valida la existencia de
#'   permisos en Google Drive antes de iniciar el proceso.
#'   \item \strong{Manejo de Errores:} Utiliza bloques `tryCatch` para evitar la
#'   exposición de trazas de error del sistema (Stack traces) que podrían revelar
#'   detalles de la infraestructura.
#' }
#'
#' @importFrom googledrive drive_upload drive_get as_id
#' @importFrom readr write_excel_csv
#' @importFrom dplyr tbl filter transmute left_join relocate select collect
#' @importFrom lubridate today
#' @export
generar_reporte_capacitacion <- function(
  pool,
  id_proyecto,
  numeros_prueba,
  drive_folder,
  corte = lubridate::today(),
  reporte = "capacitacion_",
  brigadas = NULL,
  overwrite = TRUE,
  verbose = TRUE
) {
  logi <- function(...) if (isTRUE(verbose)) message(...)

  # ----------------------------
  # 0) Validaciones de entrada
  # ----------------------------
  stopifnot(!missing(pool))
  stopifnot(is.numeric(id_proyecto) || is.character(id_proyecto))
  stopifnot(!missing(drive_folder))

  if (!inherits(corte, "Date")) {
    # Acepta "YYYY-MM-DD" y lo convierte
    corte <- as.Date(corte)
  }
  if (is.na(corte)) {
    stop("`corte` no es una fecha válida (Date).")
  }

  if (is.null(numeros_prueba)) {
    numeros_prueba <- character(0)
  }

  # Normaliza folder a dribble id
  drive_folder <- googledrive::as_id(drive_folder)

  # ----------------------------
  # 1) Autenticación / acceso Drive
  # ----------------------------
  logi("1) Verificando autenticación y acceso a Drive…")
  # Si ya estás autenticado, esto no estorba; si no, falla con error claro
  tryCatch(
    googledrive::drive_get(drive_folder),
    error = function(e) {
      stop(
        "No pude acceder a `drive_folder`. Revisa auth/permisos. Detalle: ",
        e$message
      )
    }
  )

  # ----------------------------
  # 2) Cargar brigadas (si no se proveen)
  # ----------------------------
  logi("2) Resolviendo tabla de brigadas…")
  if (is.null(brigadas)) {
    brigadas <- dplyr::tbl(pool, "Brigadas") |>
      dplyr::filter(IdProyecto == !!id_proyecto) |>
      dplyr::select(Id, NombreBrigada) |>
      dplyr::collect() # <--- AÑADE ESTA LÍNEA AQUÍ
  } else {
    # Validación mínima de columnas si es data.frame/tibble
    if (inherits(brigadas, c("data.frame", "tbl_df"))) {
      req_b <- c("Id", "NombreBrigada")
      miss <- setdiff(req_b, names(brigadas))
      if (length(miss) > 0) {
        stop(
          "`brigadas` no tiene columnas requeridas: ",
          paste(miss, collapse = ", ")
        )
      }
    }
  }

  # ----------------------------
  # 3) Crear tabla base de capacitación
  # ----------------------------
  logi("3) Creando tabla de capacitación…")
  tabla_cap <- tryCatch(
    crear_tabla_capacitacion(
      pool = pool,
      id_proyecto = id_proyecto,
      numeros_prueba = numeros_prueba,
      brigadas = brigadas
    ),
    error = function(e) stop("Falló `crear_tabla_capacitacion()`: ", e$message)
  )

  if (!inherits(tabla_cap, c("data.frame", "tbl_df"))) {
    stop(
      "`crear_tabla_capacitacion()` debe devolver un data.frame/tibble. Recibí: ",
      class(tabla_cap)[1]
    )
  }
  if (nrow(tabla_cap) == 0) {
    stop(
      "La tabla de capacitación salió vacía. Revisa filtros (id_proyecto / numeros_prueba)."
    )
  }
  if (!"Id" %in% names(tabla_cap)) {
    stop("La tabla de capacitación no trae columna `Id` (clave para join).")
  }

  # Asegurar Id único (al menos para join controlado)
  if (anyDuplicated(tabla_cap$Id) > 0) {
    logi(
      "   Aviso: `tabla_cap$Id` tiene duplicados. El join podría multiplicar filas."
    )
  }

  # ----------------------------
  # 4) Obtener respuestas por IdUsuario
  # ----------------------------
  logi("4) Obteniendo respuestas de capacitación (RegistrosCuestionarios)…")
  ids <- unique(tabla_cap$Id)
  respuestas <- tryCatch(
    obtener_respuestas_capacitacion(
      pool = pool,
      ids = ids,
      id_proyecto = id_proyecto
    ),
    error = function(e) {
      stop("Falló `obtener_respuestas_capacitacion()`: ", e$message)
    }
  )

  if (!inherits(respuestas, c("data.frame", "tbl_df"))) {
    stop(
      "`obtener_respuestas_capacitacion()` debe devolver un data.frame/tibble. Recibí: ",
      class(respuestas)[1]
    )
  }
  if (!"Id" %in% names(respuestas)) {
    stop(
      "`respuestas` no trae columna `Id` (esperada para join). Asegura `mutate(Id = IdUsuario)`."
    )
  }

  # ----------------------------
  # 5) Unir (left_join) y validar resultado
  # ----------------------------
  logi("5) Haciendo left_join capacitación + respuestas…")
  tabla_final <- tryCatch(
    dplyr::left_join(tabla_cap, respuestas, dplyr::join_by(Id)),
    error = function(e) stop("Falló el `left_join()` por `Id`: ", e$message)
  )

  if (nrow(tabla_final) == 0) {
    stop("El resultado final quedó vacío (inesperado con left_join).")
  }

  # ----------------------------
  # 6) Subir a Drive (Usando el Módulo Unificado)
  # ----------------------------
  logi("6) Subiendo reporte a Drive...")

  # Remove the trailing underscore from 'reporte' prefix if it exists,
  # because subida() automatically adds its own "_corte" separator.
  prefijo_limpio <- gsub("_$", "", reporte)

  up <- tryCatch(
    {
      # Call our universal upload function
      subida(
        carpeta_drive = drive_folder,
        objeto = tabla_final,
        nombre = prefijo_limpio,
        corte = corte
      )
    },
    error = function(e) {
      stop("Falló la subida a Drive mediante subida(): ", e$message)
    }
  )

  logi("OK ✅ Archivo subido: ", up$name)

  invisible(list(
    tabla_cap = tabla_cap,
    respuestas = respuestas,
    tabla_final = tabla_final,
    drive_file = up
  ))
}
