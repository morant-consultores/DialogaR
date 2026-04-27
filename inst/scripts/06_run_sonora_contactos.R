# =========================================================================
# SCRIPT: 06_run_sonora_contactos.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Construcción de la base de contactos de Sonora (Hermosillo) a
#           partir de la base de campo, con clasificación estratégica
#           opcional por personaje político y simpatía de partido.
#
# PRERREQUISITO: Haber ejecutado 04_run_sonora_insumos.R en la misma sesión.
#   Objetos requeridos en el entorno:
#     bd_completa, corte, bd_aux
# =========================================================================

library(DialogaR)
library(dplyr)

# =========================================================================
# 1. VERIFICAR PRERREQUISITOS
# =========================================================================

prereqs <- c(
  "bd_completa",
  "corte",
  "bd_aux"
)
faltantes <- prereqs[!sapply(prereqs, exists)]
if (length(faltantes) > 0) {
  cli::cli_abort(c(
    "Faltan objetos del entorno. Ejecuta primero 04_run_sonora_insumos.R",
    "x" = "Faltantes: {paste(faltantes, collapse = ', ')}"
  ))
}

# =========================================================================
# 2. PARÁMETROS DEL REPORTE
# =========================================================================

# Nombre del personaje político (string). La función buscará las columnas
# `conoce_<personaje>` y `opinion_<personaje>` en la base de campo.
personaje_col <- "lorenia" # ← AJUSTAR

# Nombre de la columna de simpatía de partido (string) o NULL.
# IMPORTANTE: la columna debe llamarse igual que el partido y contener
# ese nombre como valor para los simpatizantes
# (e.g., columna "PAN" con valores "PAN"/"otro" → etiqueta "PAN").
partido_col <- NULL # ← AJUSTAR o dejar NULL

# Activar clasificación estratégica (requiere partido_col != NULL)
usar_clasificacion <- FALSE # ← AJUSTAR

# =========================================================================
# 3. PREPARAR BASE DE CAMPO PARA CONTACTOS
# =========================================================================

cli::cli_h1("Preparando base de campo para contactos")

# Se filtran los registros con diálogo efectivo o donde la pregunta de puerta
# no aplicó (puerta = NA), que son los contactos con datos de interés.
bd_contactos_raw <- bd_completa |>
  dplyr::filter(desglose == "Efectivo" | is.na(puerta))

cli::cli_alert_success(
  "Base de campo filtrada: {nrow(bd_contactos_raw)} registros disponibles para contactos."
)

# =========================================================================
# 4. CONSTRUIR BASE DE CONTACTOS
# =========================================================================

cli::cli_h1("Construyendo base de contactos")

# Se usa rlang::inject() para inyectar los nombres de columna como símbolos
# desde las variables de cadena anteriores, compatible con la captura NSE
# de construir_base_contactos.
args_contactos <- list(
  personaje = rlang::sym(personaje_col),
  bd_completa = bd_contactos_raw,
  corte = corte,
  bd_aux = bd_aux,
  clasificacion = usar_clasificacion
)

if (!is.null(partido_col)) {
  args_contactos$partido <- rlang::sym(partido_col)
}

bd_contactos <- rlang::inject(construir_base_contactos(!!!args_contactos))

cli::cli_alert_success(
  "Base de contactos construida: {nrow(bd_contactos)} contactos con medio de comunicación."
)

# =========================================================================
# 5. VALIDACIONES
# =========================================================================

cli::cli_h1("Validando integridad de la base de contactos")

# 5.1 Sin registros duplicados por id -------------------------------------
ids_dup <- bd_contactos |>
  dplyr::count(id, sort = TRUE) |>
  dplyr::filter(n > 1)

if (nrow(ids_dup) > 0) {
  cli::cli_alert_danger(
    "{nrow(ids_dup)} id(s) duplicado(s) en la base de contactos."
  )
  print(ids_dup)
} else {
  cli::cli_alert_success("Sin ids duplicados.")
}

# 5.2 Todos los contactos tienen al menos celular o correo ----------------
sin_contacto <- bd_contactos |>
  dplyr::filter(celular == "-" & correo == "-")

if (nrow(sin_contacto) > 0) {
  cli::cli_alert_danger(
    "{nrow(sin_contacto)} registro(s) sin celular ni correo (no deberían existir)."
  )
} else {
  cli::cli_alert_success(
    "Todos los registros tienen al menos un medio de contacto."
  )
}

# 5.3 Integridad: contactos con celular + correo vs solo uno --------------
n_celular <- sum(bd_contactos$celular != "-", na.rm = TRUE)
n_correo <- sum(bd_contactos$correo != "-", na.rm = TRUE)
n_ambos <- sum(
  bd_contactos$celular != "-" & bd_contactos$correo != "-",
  na.rm = TRUE
)
cli::cli_alert_info(
  "Celular: {n_celular} | Correo: {n_correo} | Ambos: {n_ambos}"
)

# 5.4 Cobertura por vocero ------------------------------------------------
n_sin_vocero <- sum(is.na(bd_contactos$vocero) | bd_contactos$vocero == "-")
if (n_sin_vocero > 0) {
  cli::cli_alert_warning("{n_sin_vocero} contacto(s) sin vocero asignado.")
} else {
  cli::cli_alert_success("Todos los contactos tienen vocero asignado.")
}

# 5.5 Compatibilidad: comparar total con llamada directa (referencia) -----
# Verifica que el resultado coincide con la forma de llamada anterior:
#   contacto(personaje = "lorenia", bd_completa = contactos_prueba, ...)
n_raw_con_contacto <- bd_contactos_raw |>
  dplyr::filter(!is.na(celular) | !is.na(correo)) |>
  dplyr::filter(fecha <= corte) |>
  nrow()

if (nrow(bd_contactos) > n_raw_con_contacto) {
  cli::cli_alert_warning(
    "El resultado ({nrow(bd_contactos)}) supera los registros esperados ({n_raw_con_contacto}). Revisar joins."
  )
} else {
  cli::cli_alert_success(
    "Cardinalidad coherente: {nrow(bd_contactos)} <= {n_raw_con_contacto} registros con contacto en bd_raw."
  )
}

# 5.6 Clasificación (solo si se activó) -----------------------------------
if (usar_clasificacion && "grupo" %in% names(bd_contactos)) {
  dist_grupos <- bd_contactos |>
    dplyr::count(grupo, categoria, sort = FALSE)

  n_sin_grupo <- sum(is.na(bd_contactos$grupo))
  if (n_sin_grupo > 0) {
    cli::cli_alert_warning("{n_sin_grupo} contacto(s) sin grupo asignado (NA).")
  }

  cli::cli_h2("Distribución por grupo estratégico")
  print(dist_grupos)
}

# =========================================================================
# 6. RESUMEN
# =========================================================================

cli::cli_h2("Resumen de la base de contactos")
cli::cli_bullets(c(
  "*" = "Corte                  : {format(corte)}",
  "*" = "Registros filtrados    : {nrow(bd_contactos_raw)}",
  "*" = "Total contactos        : {nrow(bd_contactos)}",
  "*" = "Con celular            : {n_celular}",
  "*" = "Con correo             : {n_correo}",
  "*" = "Con ambos              : {n_ambos}",
  "*" = "Sin vocero             : {n_sin_vocero}",
  "*" = "Voceros distintos      : {dplyr::n_distinct(bd_contactos$vocero, na.rm = TRUE)}",
  "*" = "Secciones distintas    : {dplyr::n_distinct(bd_contactos$seccion)}"
))

# =========================================================================
# 7. EXPORTAR
# =========================================================================

cli::cli_h1("Exportando base de contactos")

ruta_excel <- file.path(
  tempdir(),
  sprintf("contactos_sonora_%s.xlsx", format(corte, "%Y%m%d"))
)

writexl::write_xlsx(bd_contactos, ruta_excel)
cli::cli_alert_success("Archivo exportado: {ruta_excel}")

# Descomenta para subir a Google Drive (requiere drive_folder del script 04):
# subida(bd_contactos,
#        nombre  = sprintf("contactos_sonora_%s", format(corte, "%Y%m%d")),
#        carpeta = drive_folder)

cli::cli_alert_success("Reporte de contactos completado.")
