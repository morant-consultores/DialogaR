# Reporte Semanal / Mensual de Brigadas

**Módulo:** `fct_reporte_mensual.R`  
**Función principal:** `generar_reporte_brigadas()`  
**Salida:** Lista en memoria con tabla de diálogos por día (pivot ancho), tabla de diálogos cortos y vector de fechas de la ventana

---

## ¿Qué hace?

Genera un consolidado de productividad operativa para una ventana **semanal** (desde el inicio de la semana hasta la fecha de corte) o **mensual** (el mes calendario anterior al corte). Produce una tabla con una columna por fecha, donde cada celda contiene los diálogos efectivos del vocero ese día.

---

## Supuestos y reglas de negocio

### Definición de diálogo efectivo

Solo se contabilizan registros con `desglose == "Efectivo"`. Cualquier otro valor (`"No abrieron"`, `"Sí, rechazaron"`, `"Cancelado"`, `"ERROR"`) no suma al total.

### Ventana temporal

- **Semanal:** desde el inicio de la semana (configurable con `week_start`, default sábado = `6`) hasta `corte` (inclusive).
- **Mensual:** el mes calendario completo anterior al corte. Se toma `floor_date(corte - 1, "month")` hasta `ceiling_date(corte - 1, "month")`.

### Gestión de altas y bajas

El reporte cruza la actividad con el historial de altas (`UsuarioLog`) y bajas (`Usuarios`) para mostrar `fecha_alta` y `fecha_baja` de cada integrante. Un usuario con `Status = FALSE` recibe `fecha_baja` igual a `FechaUpdate`; uno con `Status = TRUE` aparece con `fecha_baja = NA`.

### Voceros sin coordinador (huérfanos operativos)

Los voceros que no tienen coordinador asignado en el catálogo administrativo **no se eliminan**. Se imputa `"SIN ASIGNAR"` en `nombre_coordinador`, `supervisor` y `nombre_brigada`. Al final del cálculo se emite una **advertencia de calidad de datos** indicando cuántos registros quedaron en esta categoría.

### Deduplicación por cambios de coordinador

Si un vocero aparece asociado a más de un coordinador (por cambios históricos), se conserva únicamente el coordinador con `status_coord = TRUE`. Si no hay ninguno activo, se toma la primera fila disponible.

### Integridad de totales

El sistema verifica que la suma de diálogos efectivos en la tabla final sea igual a la suma calculada directamente desde `bd_completa`. Si hay diferencia, se emite una **alerta de integridad** con el detalle del delta. Si los totales coinciden, se confirma con un mensaje de éxito.

**Comportamiento esperado con voceros sin catálogo:** todo vocero con actividad registrada debe aparecer en el reporte. Si un vocero no figura en el catálogo operativo (`bd_aux`), sus diálogos se deben incluir igualmente en los totales; los campos administrativos (brigada, coordinador, municipio) aparecerán vacíos. El sistema emite una advertencia de calidad de datos para que el equipo corrija la asignación en el catálogo.

> **Nota de implementación:** La versión actual usa `bd_aux` como tabla izquierda del join, lo que en la práctica excluye a estos voceros del reporte detallado. Esto hace que la verificación de integridad detecte una discrepancia y emita `cli_alert_danger`. Es una limitación conocida del código que debe corregirse para garantizar la consistencia total de los datos.

### Promedio diario

Se calcula dividiendo el `Total` de diálogos entre los `dias_habiles_trabajados` (días de la ventana en que el vocero registró al menos un diálogo efectivo). Si el vocero no trabajó ningún día, el promedio es `0` (no genera división por cero).

### Diálogos cortos

Se considera "corto" todo diálogo con `duracion_minutos <= umbral_cortos` (default: 2 minutos). Se genera una tabla paralela en formato pivot ancho con el conteo de cortos por día.

### Estructura de la tabla de salida

La tabla principal (`registros`) tiene:
- Columnas administrativas: `municipio`, `nombre_brigada`, `nombre_coordinador`, `supervisor`, `status_coord`, `nombre_vocero`, `vocero`, `status_vocero`
- Una columna por cada fecha de la ventana con el conteo de diálogos efectivos ese día
- Columnas resumen: `Total`, `dias_habiles_trabajados`, `promedio_diario`, `duracion_promedio`, `trabajo_diario`
- Datos de alta/baja: `fecha_alta`, `fecha_baja`

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `reporte` | — | `"semanal"` o `"mensual"` (requerido) |
| `week_start` | `6` (sábado) | Día de inicio de la semana semanal |
| `umbral_cortos` | `2` | Minutos máximos para clasificar un diálogo como corto |
