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


