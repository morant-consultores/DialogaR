# Reporte de Avance por Sección

**Módulo:** `fct_secciones.R`  
**Función principal:** `meta_usuario_condicional()`  
**Salida:** Data frame en memoria (se guarda/sube desde el script orquestador)

---

## ¿Qué hace?

Agrega la actividad de campo por sección electoral y la cruza con las metas asignadas. Calcula el avance como proporción de diálogos efectivos sobre la meta de cada sección. Opcionalmente desglosa los resultados por coordinador y brigada.

---

## Supuestos y reglas de negocio

### Definición de diálogo efectivo

Solo se cuentan registros con `desglose == "Efectivo"`. El resto de los valores no contribuye al indicador de avance.

### Normalización de sección

Los números de sección se normalizan con padding de ceros a la izquierda hasta 4 dígitos (ej. `"0042"`). Esto aplica tanto a la actividad como al catálogo de metas, garantizando que el join no falle por diferencias de formato.

### Exclusión de secciones sin datos geográficos

Las filas con `seccion` en `NA` o vacía se excluyen antes de calcular cualquier métrica.

### Cálculo del avance

`avance_meta = diálogos_efectivos / meta`

Si la sección no tiene meta asignada en el catálogo (`meta = NA`) **o** la meta es `0`, el avance se imputa como `0` (no genera división por cero ni `NA`).

### Agrupación condicional

- **Sin `base_coordinadores`:** la tabla tiene una fila por sección, con el total de actividad acumulada en esa sección.
- **Con `base_coordinadores`:** la tabla tiene una fila por combinación `(seccion, nombre_coordinador, nombre_brigada)`, lo que permite ver qué coordinador trabajó cada sección y cuánto avanzó.

### Métricas calculadas

| Columna | Descripción |
|---|---|
| `VIVIENDAS VISITADAS` | Total de registros de campo en la sección (todos los desgloses) |
| `DIAS TRABAJADOS` | Número de fechas distintas con actividad en la sección |
| `DIÁLOGOS EFECTIVOS` | Registros con `desglose == "Efectivo"` |
| `META` | Meta asignada a la sección (redondeada al entero más cercano) |
| `FECHA CORTE` | Fecha de corte del reporte |
| `AVANCE META` | Proporción numérica (0 si meta es 0 o no asignada) |

Los nombres de columna se presentan en mayúsculas con espacios (en lugar de `snake_case`).

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `base_coordinadores` | `NULL` | Si se pasa, agrega por sección + coordinador + brigada |
| `corte` | — | Fecha de corte que se añade como columna informativa |
| `metas` | — | Data frame con columnas `seccion` y `meta` (numérica) |
