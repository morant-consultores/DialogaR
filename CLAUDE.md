# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

DialogaR is an R package that serves as a data processing and reporting engine for field survey operations (political polling/civic engagement in Mexico). It orchestrates ETL pipelines, data aggregation, report generation (Excel, PowerPoint, PDF), quality auditing, and Google Drive uploads.

## Development Commands

```r
# Load package interactively (development)
devtools::load_all()

# Regenerate documentation and NAMESPACE from roxygen2 comments
devtools::document()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-fct_capacitacion.R")

# Full package check (equivalent to R CMD check)
devtools::check()
```

The package uses roxygen2 with `PackageRoxygenize: rd,collate,namespace` — always run `devtools::document()` after modifying function signatures or exported functions, as NAMESPACE is auto-generated and must not be edited manually.

## Architecture

### Core Pattern: Plugin/Callback Injection

The central architectural pattern is business-rule injection via callbacks. Functions accept a `postprocess_insumos` hook that callers use to inject zone-specific transformations:

```r
# Example from inst/scripts/01_run_productividad_chihuahua.R
hook_chihuahua <- function(df) { ... }  # zone-specific rules
cargar_insumos(..., postprocess_insumos = hook_chihuahua)
```

This allows the package to be generic while supporting 3 geographic zones (Chihuahua, Juárez, Sur) with different data schemas.

### Data Flow

1. **ETL** (`fct_etl.R`) — `cargar_insumos()` loads raw data from SQL databases via `dplyr::tbl()` lazy evaluation, then applies the postprocess hook
2. **Normalization** (`homogenización.R`) — `normalizador_actividad()` unifies multi-zone schemas; Ascensión municipality is a special case (maps to Juárez zone, not Sur)
3. **Aggregation** — Hierarchical rollups: General → Distrito → Brigada → Vocero
4. **Export** — `fct_productividad.R`, `fct_semanal_mensual.R`, `fct_reporte_mensual.R` build flextable/Excel/PPTX outputs
5. **Upload** (`fct_publicacion_drive.R`) — `subida()` pushes DataFrames, lists, Workbooks, or PPTX to Google Drive

### Database Access

All queries use `dplyr::tbl()` + lazy evaluation with explicit `.collect()` calls to materialize data. Connections are pool-based (never individual connections). Tests use in-memory SQLite (`DBI::dbConnect(RSQLite::SQLite(), ":memory:")`).

### Module Responsibilities

| File | Key Exported Functions |
|------|----------------------|
| `fct_etl.R` | `cargar_insumos()`, catalog loaders (usuarios, brigadas, municipios) |
| `fct_productividad.R` | `construir_productividad_diaria()` |
| `fct_capacitacion.R` | `crear_tabla_capacitacion()`, `generar_reporte_capacitacion()` |
| `fct_reporte_auditoria.R` | `generar_metricas_auditoria()`, `crear_workbook_auditoria()` |
| `fct_semanal_mensual.R` | `generar_reporte_brigadistas()` |
| `fct_reporte_mensual.R` | `generar_reporte_productividad()` |
| `fct_publicacion_drive.R` | `subida()` |
| `fct_paseLista.R` | `procesar_pase_lista()`, `actualizar_pase_lista()` |
| `fct_secciones.R` | `meta_usuario_condicional()` |
| `funciones_pdf.R` | PDF report generation (ggplot2/officer) |
| `params.R` | Global color palettes, ggplot2 theme (`tema_m`) |
| `funciones_aux.R` | `coerce_numeric_candidates()` (95% threshold heuristic), Drive auth helpers |

## Testing Conventions

- Mock external APIs using `testthat::local_mocked_bindings()` — never call real Google Drive or production databases in tests
- Use in-memory SQLite for database tests
- Tests follow the pattern: create mock DB → insert fixture data → call function → assert output shape/values
- Test files live in `tests/testthat/` with the naming convention `test-<module>.R`

## Key Conventions

- **Function naming:** Exported functions use descriptive Spanish names (`generar_`, `crear_`, `construir_`, `cargar_`); internal helpers use `camelCase` or plain names
- **Column names:** Always `snake_case` (enforced by `janitor::clean_names()`)
- **Robustness guards:** Add early `nrow() == 0` checks before processing — see `fct_reporte_auditoria.R` for the established pattern
- **Credentials:** Stored in `.Renviron` (excluded from git). Never hardcode secrets.
- **`dev/` directory:** Excluded from package build (`.Rbuildignore`). Contains setup scripts only, not production code.
- **Documentation:** All exported functions require full roxygen2 docs including `@param`, `@return`, `@export`, and a `@section Seguridad y Privacidad:` block for functions handling PII

## Seguridad y Cumplimiento (ISO 27001 / LFPDPPP)

### Clasificación de datos

DialogaR maneja tres niveles de sensibilidad. Toda función nueva debe declarar en su bloque roxygen2 con cuál trabaja:

| Nivel | Contenido | Ejemplo |
|-------|-----------|---------|
| **PÚBLICO** | Métricas agregadas, conteos por zona, sin identificadores | Totales de productividad por distrito |
| **INTERNO** | Datos de brigadas y usuarios del sistema | Nombre de brigadista, zona asignada, rol |
| **CONFIDENCIAL** | Cualquier dato de encuestados | Nombre, domicilio, teléfono, preferencia política |

Declaración obligatoria en roxygen2 para funciones que toquen datos INTERNO o CONFIDENCIAL:

```r
#' @section Seguridad y Privacidad:
#' Nivel de datos: CONFIDENCIAL (PII de encuestados).
#' No registrar en logs. No exponer en mensajes de error.
#' Seleccionar únicamente las columnas mínimas necesarias para el reporte.
#' Control ISO 27001: A.8.2 (Clasificación de información).
```

### Manejo de PII en código

- Nunca usar `print()`, `message()`, o `warning()` sobre data frames que contengan columnas PII
- Los mensajes de error deben referenciar IDs, índices o conteos — nunca valores de columnas sensibles:

```r
# CORRECTO
stop(sprintf("Fila %d: valor de encuesta fuera del rango esperado [1-5]", i))

# INCORRECTO — expone PII en el stack trace
stop(sprintf("Error con encuestado %s, tel %s", row$nombre, row$telefono))
```

- En bloques `tryCatch`, capturar la condición sin re-emitir el objeto completo (que puede contener PII):

```r
# CORRECTO
tryCatch(
  procesar(df),
  error = function(e) stop(sprintf("Error en procesamiento: %s", conditionMessage(e)))
)
```

- Al usar `dplyr::select()` o `dplyr::tbl()`, seleccionar explícitamente solo las columnas necesarias — nunca `SELECT *` sobre tablas de encuestados

### Gestión de credenciales (A.9 / A.10)

- Toda credencial vive exclusivamente en `.Renviron` o en el keychain del sistema; nunca en código fuente ni en archivos de configuración commiteados
- Los tokens de Google Drive deben usar scope mínimo (`drive.file`, no `drive`)
- Validar la existencia de variables de entorno al inicio de cada función que las requiera:

```r
stopifnot(
  "DB_HOST no configurado — revisar .Renviron" = nchar(Sys.getenv("DB_HOST")) > 0,
  "DB_PASS no configurado — revisar .Renviron" = nchar(Sys.getenv("DB_PASS")) > 0
)
```

- `.Renviron`, `*.token`, `*.rds` con datos reales y cualquier output de reporte nunca deben commitearse; verificar `.gitignore` antes de hacer `git add`

### Convenciones de commits (A.12.1 — Gestión de cambios)

Usar **Conventional Commits** en español. Un commit = un cambio lógico. No mezclar refactors con features ni correcciones de seguridad con cambios funcionales.

```
<tipo>(<alcance>): <descripción en imperativo>
```

Tipos válidos:

| Tipo | Cuándo usarlo |
|------|---------------|
| `feat` | Nueva funcionalidad |
| `fix` | Corrección de bug |
| `refactor` | Cambio sin impacto funcional |
| `test` | Solo tests |
| `docs` | Solo documentación |
| `chore` | Dependencias, CI, configuración |
| `security` | Cambio con implicación de seguridad — **siempre requiere cuerpo explicativo** |

Los commits `security` deben incluir en el cuerpo: qué control ISO 27001 atienden y cómo verificar la corrección.

```
security(etl): sanitizar mensajes de error en cargar_insumos()

Removidos valores de columnas PII de los stop() en el bloque de
validación de esquema. Ahora solo se exponen nombres de columna y
conteos de filas.

Control ISO 27001: A.12.4 (Registro de actividad del operador).
Verificación: grep -r "row\$nombre\|row\$telefono" R/ debe retornar vacío.
```

### Dependencias externas (A.14.2 — Seguridad en desarrollo)

- Antes de agregar un paquete a `DESCRIPTION`, verificar que está en CRAN y tiene mantenimiento activo (último release < 2 años)
- Paquetes que manejen conexiones de red, credenciales o datos de usuario requieren justificación explícita en el commit de incorporación
- No instalar paquetes desde GitHub en código de producción (`remotes::install_github`) sin aprobación documentada en el commit

### Separación de entornos (A.12.1)

El directorio `dev/` es exclusivamente para exploración. Ningún script en `dev/` debe conectarse a bases de datos de producción sin una guardia explícita al inicio:

```r
if (Sys.getenv("DIALOGA_ENV") != "development") {
  stop("Este script es solo para desarrollo. Configurar DIALOGA_ENV=development en .Renviron.")
}
```

### Encabezado obligatorio en archivos R nuevos (A.12.1 — Trazabilidad)

Todo archivo `.R` nuevo en `R/` debe iniciar con este bloque antes del primer `#'`:

```r
# ============================================================
# Módulo    : <nombre_modulo>
# Propósito : <descripción en una línea>
# Datos     : PÚBLICO | INTERNO | CONFIDENCIAL
# Control   : <controles ISO 27001 aplicables, ej. A.8.2, A.9.4>
# Revisión  : YYYY-MM-DD
# ============================================================
```

## Estrategia de versiones

Este repositorio mantiene **dos versiones instalables simultáneas**:

| Versión | Rama | Tag git | Estado |
|---------|------|---------|--------|
| `0.2.0` | `master` | `v0.2.0` | Estable. Proyectos Chihuahua, Juárez, Sur. |
| `0.3.0.9000` | `feat/brigada-log-coordinator` | — (dev) | Activo. Proyectos que requieren resolución de coordinador desde `BrigadasLog`. |

### Qué introduce v0.3.0

- `cargar_brigada_log()` — carga histórico de asignaciones de brigada desde `BrigadasLog`
- `resolver_coordinador_en_fecha()` — LOCF sobre `BrigadasLog` para determinar coordinador vigente al corte
- `conectar_base_datos(perfil = "dev")` — soporte multi-entorno (producción / DEVSVNET-V2) vía variables de entorno `pool_dev_*`
- Cambios en `cargar_actividad()`: campo `id_usuario_actividad` (cuando `UsuarioId` está presente en el snapshot), `usuario_num` ahora es condicional
- `resolver_estructura_corte()`: elimina el fill de `IdSupervisor` (incompatible con proyectos que dependían de esa columna en v0.2.0)

### Instalar por versión

```r
# Proyectos en v0.2.0 (estable):
remotes::install_github("morant-consultores/DialogaR@v0.2.0")

# Proyectos en v0.3.x (BrigadasLog):
remotes::install_github("morant-consultores/DialogaR@feat/brigada-log-coordinator")
```

### Scripts con guardia de versión

Los scripts en `inst/scripts/` que usan funciones de v0.3.0 incluyen una guardia al inicio:

```r
if (utils::packageVersion("DialogaR") < "0.3.0") {
  stop("Este script requiere DialogaR >= 0.3.0 ...", call. = FALSE)
}
```

Scripts con guardia activa: `04_run_sonora_insumos.R`, `00_test_devsvnet.R`.

### Ruta de migración

Cuando todos los proyectos hayan validado v0.3.0, mergear `feat/brigada-log-coordinator` → `master`, tagear `v0.3.0`, y eliminar la guardia de los scripts (ya no serán necesarias).


