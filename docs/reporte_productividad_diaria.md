# Reporte de Productividad Diaria

**Módulo:** `fct_productividad.R`  
**Función principal:** `generar_reporte_productividad()`  
**Función legacy (PPTX):** `construir_productividad_diaria()` en `fct_semanal_mensual.R`  
**Salida:** Excel (.xlsx) + lista de objetos en memoria

---

## ¿Qué hace?

Consolida en una tabla la actividad de campo para un día específico (fecha de corte), cruzando tres fuentes: la estructura operativa (quién pertenece a qué brigada), los hechos de actividad capturados en campo, y el pase de lista de asistencia enviado por los coordinadores.

El reporte **no carga datos desde la base de datos**; asume que los insumos ya fueron preparados por el ETL (`cargar_insumos()`).

---

## Supuestos y reglas de negocio

### Universo de personas incluidas

- Se incluyen únicamente voceros con `status_vocero = TRUE` al corte. Los inactivos se excluyen siempre.
- Se excluyen filas donde `vocero = "-"` (registros sin usuario asignado).
- Los coordinadores pueden aparecer como filas adicionales (configurable con `include_coordinators`). Por defecto están incluidos.
- Si un vocero activo tiene un **coordinador inactivo** (`status_coord = FALSE`) y ese vocero tiene actividad registrada, el reporte **se detiene con error** para evitar pérdida silenciosa de datos. Si no tiene actividad, se excluye con una advertencia.
- Se excluyen brigadas con nombre que contenga `"CAPACITACIONES"` del resumen por brigada.

### Filtro de coordinadores inactivos con datos — regla crítica

> Si un vocero activo tiene registros de actividad pero su coordinador figura como inactivo, el proceso falla explícitamente. Debe corregirse el estatus del coordinador en la base de datos antes de continuar.

### Métricas de actividad (fuente: nube)

Los conteos se calculan a partir del campo `desglose` de la tabla de actividad:

| Métrica | Criterio |
|---|---|
| `viviendas_visitadas_nube` | Total de registros del vocero en el día |
| `dialogos_efectivos_nube` | `desglose == "Efectivo"` |
| `no_abrieron_nube` | `desglose == "No abrieron"` |
| `rechazaron_nube` | `desglose == "Sí, rechazaron"` |
| `cancelado_nube` | `desglose == "Cancelado"` |
| `sin_informacion_nube` | `desglose == "ERROR"` |
| `duracion_promedio_min` | Promedio de duración solo sobre registros `"Efectivo"` |
| `total_horas_trabajadas` | Diferencia entre el primer y último timestamp del día (`fecha_fin` - `fecha_inicio`) |

Si un vocero trabajó en más de una sección en el día, la columna `seccion` muestra todas las secciones separadas por coma.

### Pase de lista

- Solo se incluyen registros del pase de lista cuya `fecha == corte`.
- Si hay múltiples pases del mismo coordinador para el mismo vocero en el mismo día, los valores se **concatenan separados por coma** (no se eliminan; se preserva trazabilidad). Se genera la bandera `numero_pases_lista`.
- Si no existe pase de lista para la fecha bajo la política `"lenient"` (default), el reporte se genera igualmente con los datos de actividad disponibles. Con política `"strict"`, el proceso falla.
- El uso de más de un cuestionario de pase de lista requiere habilitar explícitamente `allow_multiple_sources = TRUE`.

### Conversión numérica del pase de lista

Las columnas de texto del pase que parecen numéricas se convierten automáticamente si el **95% o más de sus valores no vacíos** son parseable como número. Columnas excluidas de esta heurística: `id`, `fecha`, `usuario`, `asistencia`, `observaciones`, y campos de motivo de inasistencia.

### Valores faltantes (imputación)

- Campos numéricos sin dato: se imputan con `0`.
- Campos texto sin dato: se imputan con `"-"`.
- La bandera `tiene_pase_lista` es `TRUE` si `numero_pases_lista > 0`.
- La bandera `tiene_actividad` es `TRUE` si `viviendas_visitadas_nube > 0`.

### Ordenamiento

Si la tabla tiene columna `distrito`, el orden es: `distrito (numérico) → municipio → brigada → vocero`. Sin `distrito`: `municipio → brigada → vocero`.

---

## Tabla resumen (KPI por brigada)

La función `generar_tablas_reporte()` produce adicionalmente:

- **Tabla resumen** (nivel General y por Brigada): usuarios con actividad, diálogos efectivos, promedio de diálogos por vocero.
- **Código de colores** sobre el promedio:

| Color | Criterio |
|---|---|
| Verde | Promedio ≥ 14.5 |
| Amarillo | Promedio ≥ 10.0 |
| Naranja | Promedio < 10.0 |

- **Tabla detalle de coordinadores**: indica si cada coordinador envió pase de lista (✅ / ❌) y el total de pases por coordinador.

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `missing_policy` | `"lenient"` | Si `"strict"`, falla ante ausencia de datos |
| `include_coordinators` | `TRUE` | Incluye coordinadores como filas del reporte |
| `coordinator_scope` | `"from_structure"` | Fuente del universo de coordinadores |
| `coordinator_grain` | `"coordinator_only"` | Una fila por coordinador (no por brigada) |
| `allow_multiple_sources` | `FALSE` | Requiere activación explícita para usar >1 cuestionario |
| `allow_multiple_per_day` | `TRUE` | Permite múltiples pases diarios (los colapsa) |
| `numeric_threshold` | `0.95` | Proporción para conversión heurística a numérico |
| `header_fill` | `"#7030A0"` | Color morado del encabezado Excel |

---

## Salida

Lista con cuatro elementos: `corte`, `pase_lista`, `registros`, `bd_prod`.  
El Excel se genera con una hoja nombrada igual a la fecha de corte.
