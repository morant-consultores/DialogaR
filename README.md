# DialogaR

Motor de procesamiento de datos y reportes para operaciones de encuesta de campo (sondeos políticos y participación cívica en México). Orquesta pipelines ETL, agregación de datos, generación de reportes (Excel, PowerPoint, PDF), auditoría de calidad y carga a Google Drive.

## Instalación

```r
# Desde GitHub
remotes::install_github("morant-consultores/DialogaR")
```

## Uso rápido

```r
library(DialogaR)

# 1. Conectar a la base de datos
pool <- conectar_base_datos()

# 2. Cargar insumos (ETL)
insumos <- cargar_insumos(
  pool          = pool,
  id_proyecto   = 17,
  corte         = Sys.Date() - 1,
  fuentes_actividad = list(
    list(tabla = "snapshot_id_292", origen = "chih_capital"),
    list(tabla = "snapshot_id_293", origen = "chih_sur")
  ),
  ids_pase_lista  = c(210),
  procesador_pl   = procesar_pase_lista
)

# 3. Generar reporte de productividad
reporte <- generar_reporte_productividad(
  bd_aux        = insumos$bd_aux,
  actividad_dia = insumos$bd_actividad,
  corte         = Sys.Date() - 1,
  pl_main       = insumos$pase_lista$pl_210
)

# 4. Subir a Google Drive
subida(carpeta_drive, reporte$bd_prod, "reporte", Sys.Date())

pool::poolClose(pool)
```

Copia los scripts de ejemplo a tu proyecto con:

```r
usar_plantilla("chihuahua")       # pipeline Chihuahua
usar_plantilla("sonora")          # pipeline Sonora
usar_plantilla("pase_lista")      # procesamiento de pase de lista
```

## Flujo de datos

```
cargar_insumos()          →  ETL desde SQL Server, aplica hook de negocio
  ↓
generar_reporte_*()       →  Agrega métricas, construye tablas flextable/Excel/PPTX
  ↓
subida()                  →  Sube el resultado a Google Drive
```

La jerarquía de agregación es: **General → Distrito → Brigada → Vocero**.

## Módulos principales

| Archivo | Funciones exportadas |
|---------|---------------------|
| `fct_etl.R` | `cargar_insumos()`, cargadores de catálogos |
| `fct_productividad.R` | `generar_reporte_productividad()`, `construir_productividad_diaria()` |
| `fct_capacitacion.R` | `crear_tabla_capacitacion()`, `generar_reporte_capacitacion()` |
| `fct_reporte_auditoria.R` | `generar_metricas_auditoria()`, `crear_workbook_auditoria()` |
| `fct_semanal_mensual.R` | `generar_reporte_brigadas()` |
| `fct_paseLista.R` | `procesar_pase_lista()`, `actualizar_pase_lista()` |
| `fct_publicacion_drive.R` | `subida()` |
| `helpers.R` | `conectar_base_datos()`, `autenticar_googledrive()`, `usar_plantilla()` |

## Configuración de credenciales

Las credenciales se leen desde `.Renviron`. Nunca se hardcodean en el código.

```bash
# .Renviron
DB_SERVER=...
DB_DATABASE=...
DB_USER=...
DB_PASSWORD=...
GDRIVE_SERVICE_ACCOUNT=<JSON en Base64>
```

## Patrón de extensión: hooks de negocio

El paquete es genérico y admite zonas geográficas con esquemas de datos distintos (Chihuahua, Juárez, Sur) mediante callbacks inyectados en `cargar_insumos()`:

```r
hook_chihuahua <- function(df) {
  # Transformaciones específicas de zona
  df
}

insumos <- cargar_insumos(..., postprocess_insumos = hook_chihuahua)
```

## Desarrollo

```r
devtools::load_all()    # Cargar el paquete en modo desarrollo
devtools::document()    # Regenerar documentación y NAMESPACE
devtools::test()        # Correr pruebas
devtools::check()       # Verificación completa (R CMD check)
```

Los tests usan SQLite en memoria y mocks de Google Drive — nunca tocan bases de producción.

## Reporte de problemas

<https://github.com/morant-consultores/DialogaR/issues>
