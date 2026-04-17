# Reporte de Auditoría

**Módulo:** `fct_reporte_auditoria.R`  
**Funciones:** `generar_metricas_auditoria()`, `crear_workbook_auditoria()`  
**Salida:** Excel (.xlsx) con dos hojas: `res_auditoria` y `observaciones`

---

## ¿Qué hace?

Extrae las evaluaciones de calidad (auditorías) realizadas a los registros de campo en una ventana semanal y calcula el promedio de calificación por vocero. Cruza los resultados con el catálogo de voceros activos y genera un Excel con formato condicional (escala de color rojo-verde).

---

## Supuestos y reglas de negocio

### Universo de voceros

Solo se incluyen voceros con `status = TRUE` en el catálogo. Los voceros inactivos se excluyen del reporte aunque hayan tenido auditorías en la semana.

### Ventana temporal (dinámica vs. explícita)

Por defecto, la semana se **ancla a la última fecha que tiene auditorías reales** en la tabla `EvaluacionRegistro`, no a la fecha de corte (`corte`) recibida como parámetro. Esto garantiza que el reporte siempre muestre la semana con datos disponibles, aunque la semana en curso no tenga auditorías aún.

Si se pasan explícitamente `fecha_inicio` y `fecha_fin`, se usa ese rango directamente sin calcular dinámicamente.

El inicio de semana por defecto es **lunes** (`week_start = 1`). Es configurable.

### Asignación de registros a semanas

Cada registro (`id`) se asigna a la semana de su **primera aparición** en la base de actividad (`bd_completa`). Si el mismo registro aparece en múltiples fechas, solo se contabiliza en la semana donde fue creado originalmente. Esto garantiza semanas mutuamente excluyentes.

### Penalización por eliminación

Las auditorías con `dictamenFinal == "Eliminada"` reciben **calificación forzada a 0**, independientemente del valor original de `totalEvaluacion`. El conteo de registros eliminados se reporta en la columna `eliminados`.

### Promedio de evaluaciones

Se calcula como el promedio aritmético de `totalEvaluacion` (con la penalización de eliminados aplicada), redondeado a un decimal. Se reporta como `Promedio de evaluaciones`.

### Exclusión de brigadas

Por defecto se excluyen brigadas cuyo nombre contenga `"CAPACITACIONES"`. Es posible excluir otras brigadas pasando un vector al parámetro `excluir_brigadas`.

### Extracción optimizada

La función descarga la tabla `EvaluacionRegistro` completa a memoria y luego filtra por `inner_join` con los IDs de la ventana. Esto evita generar una cláusula SQL `IN (...)` con miles de IDs, que es el cuello de botella más común.

### Fail-fast

Si no hay registros de actividad en la ventana solicitada, el proceso falla de inmediato con un mensaje claro antes de intentar calcular promedios vacíos.

---

## Hoja `res_auditoria`

Una fila por vocero activo que tenga al menos una auditoría en la ventana. Columnas principales:

| Columna | Descripción |
|---|---|
| `nombre_brigada` | Brigada del vocero |
| `nombre_completo` | Nombre del vocero |
| `Promedio de evaluaciones` | Promedio ponderado (eliminadas = 0) |
| `dialogos_auditados` | Total de registros auditados en la semana |
| `eliminados` | Cuántas auditorías recibieron dictamen "Eliminada" |
| `efectivos` | Total de diálogos efectivos en la ventana |
| `fecha_ultimo_registro` | Fecha más reciente de actividad |

**Formato Excel:** escala de color rojo → verde sobre la columna `Promedio de evaluaciones`.

---

## Hoja `observaciones`

Detalle de cada auditoría individual: `RegistroId`, fecha, `usuario_num`, `observaciones`, `dictamenFinal`. Solo para voceros activos.

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `week_start` | `1` (lunes) | Día de inicio de semana para el cálculo dinámico |
| `fecha_inicio` / `fecha_fin` | `NULL` | Si se pasan, anulan el cálculo dinámico |
| `excluir_brigadas` | `"CAPACITACIONES"` | Patrón de brigadas a excluir |
