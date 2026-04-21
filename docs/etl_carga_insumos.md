# ETL: Carga de Insumos Operativos

**Módulo:** `fct_etl.R`  
**Función principal:** `cargar_insumos()`  
**Funciones auxiliares:** `procesar_pase_lista()`, `resolver_brigada_en_fecha()`  
**Salida:** Lista estructurada `insumos` con todos los datos base para los reportes

---

## ¿Qué hace?

Orquesta la descarga y construcción de todos los datos necesarios para generar reportes. Produce la estructura operativa al corte (`bd_aux`), la sábana de actividad (`bd_actividad`), los pases de lista y los catálogos de referencia.

Es el punto de entrada único de datos para todos los módulos de reporte.

---

## Supuestos y reglas de negocio

### Catálogo de usuarios

Solo se incluyen usuarios que cumplan alguna de estas dos condiciones:
- `Capacitacion == TRUE`, **o**
- `Cargo == "Coordinador de Brigada"`

Este filtro se aplica en SQL antes de descargar a memoria.

### Estructura operativa al corte (`bd_aux`)

La jerarquía operativa se reconstruye históricamente usando la tabla `UsuarioLog`. Para cada usuario, se toma la **última asignación registrada** con `fecha_evento <= corte` (LOCF — Last Observation Carried Forward). Esto garantiza que el reporte refleja la estructura vigente en la fecha de análisis, no la estructura actual.

Si un usuario no tiene ningún evento en `UsuarioLog` antes del corte, no aparece en `bd_aux`.

### Actividad: filtro mínimo antes de descargar

Antes de traer datos a memoria se aplican estos filtros en SQL:
- Se excluyen registros con `seccion = NULL`.
- Si la fuente tiene `requiere_puerta = TRUE`, se excluyen registros con `puerta = NULL`.

Esto reduce el volumen descargado y evita registros geográficamente inválidos.

### Normalización de sección

El campo `seccion` se convierte a texto y se rellena con padding de espacios a la izquierda hasta 4 caracteres al momento de la descarga.

### Zona geográfica

El campo `municipio` se asigna de acuerdo a la zona de la fuente de datos:
- Fuentes de zona `"chih"`: municipio forzado a `"CHIHUAHUA"`.
- Fuentes de zona `"juarez"`: municipio forzado a `"JUAREZ"`.
- Fuentes de zona `"sur"`: los registros del municipio `"CASAS GRANDES"` con `fecha >= 2025-09-01` se recodifican como `"NUEVO CASAS GRANDES"`.

### Resolución histórica de brigada por registro

La función `resolver_brigada_en_fecha()` permite enriquecer la actividad con la brigada que tenía asignada el vocero **en el momento de cada registro**, no su brigada actual. Usa LOCF sobre `UsuarioLog`: para cada par `(usuario_num, fecha)` busca la última asignación de brigada cuya `fecha_evento <= fecha`.

### Pase de lista

El pase de lista se procesa mediante la función `procesar_pase_lista()`. Las reglas aplicadas:

1. **Fecha oficial del pase:** se usa `FechaInicio` del registro, convertida a `Date` (sin hora) en zona horaria `America/Mexico_City`.
2. **Coordinador:** se toma del campo JSON `Obtener_usuario`. Se normaliza con `toupper()` y `str_squish()`. Si el campo está ausente o vacío, el registro se descarta (no puede vincularse a ninguna persona).
3. **Variables por usuario:** las claves del JSON con patrón `<variable>_<num_usuario>` se asocian al usuario indicado en el sufijo numérico.
4. **Variables globales del pase:** `observaciones` y `finalizar` se replican a todas las filas de usuarios del mismo registro.
5. **Múltiples pases del mismo coordinador en el mismo día:** no se colapsan. Cada pase tiene su propio `id` y produce filas separadas. La deduplicación, si se requiere, ocurre en el módulo de reportes.
6. **Duplicados dentro de un pase (misma combinación id/fecha/coordinador/usuario/variable):** se aplica la regla de tomar el **primer valor no-NA** en orden de aparición.
7. **JSON inválido:** los registros con JSON que no puede parsearse se omiten silenciosamente (el proceso no se interrumpe).

### Inyección de reglas de negocio específicas por zona (`postprocess_insumos`)

El ETL es agnóstico a las reglas de cada zona geográfica. El parámetro `postprocess_insumos` acepta una función (callback/hook) que recibe el objeto `insumos` completo y retorna una versión modificada. Cada script de zona usa este mecanismo para aplicar sus transformaciones específicas (reasignación de municipios, filtros adicionales, etc.) sin modificar el código central.

---

## Estructura del objeto `insumos` devuelto

```
insumos
├── id_proyecto       # ID del proyecto
├── corte             # Fecha de corte (Date)
├── bd_actividad      # Sábana de actividad (todos los registros de campo)
├── bd_aux            # Estructura operativa al corte (voceros, coordinadores, brigadas)
├── pase_lista        # Lista nombrada con un tibble por cuestionario (pl_<id>)
└── cat
    ├── usuarios      # Catálogo de usuarios del proyecto
    ├── brigadas      # Catálogo de brigadas
    ├── municipios    # Catálogo de municipios
    └── usuario_log   # Historial completo de asignaciones (UsuarioLog)
```

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `fuentes_actividad` | — | Lista de tablas origen con metadatos de zona y columnas |
| `ids_pase_lista` | `integer()` | IDs de cuestionarios de pase de lista a procesar |
| `procesador_pl` | `NULL` | Función para parsear el JSON; requerida si `ids_pase_lista` no está vacío |
| `fecha_min_actividad` | `NULL` | Límite inferior de la extracción de actividad |
| `postprocess_insumos` | `NULL` | Hook para inyectar reglas de negocio específicas de zona |
