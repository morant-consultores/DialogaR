# Base de Contactos

**Módulo:** `fct_contactos.R`  
**Función principal:** `construir_base_contactos()`  
**Salida:** Data frame en memoria (se guarda/sube desde el script orquestador)

---

## ¿Qué hace?

Construye un directorio de contactos a partir de los registros de encuesta de campo. Filtra únicamente los entrevistados que tienen al menos un medio de contacto (correo electrónico o celular) y, opcionalmente, clasifica a cada contacto según su simpatía de partido y su opinión sobre un personaje político.

---

## Supuestos y reglas de negocio

### Filtro temporal

Solo se incluyen registros con `fecha <= corte`. Registros posteriores a la fecha de corte no aparecen en la base.

### Condición de inclusión: contactabilidad

Un registro solo aparece en la base si tiene **al menos uno** de los dos campos de contacto:
- `correo` con valor distinto al marcador de NA (default `"-"`), **o**
- `celular` con valor distinto al marcador de NA.

Los registros sin ningún medio de contacto se descartan silenciosamente.

### Catálogo de voceros: solo activos con brigada activa

Al cruzar la actividad con el catálogo operativo:
- Solo se consideran brigadas con `activo_brigada == TRUE`.
- De los voceros coordinadores, solo aquellos con `cargo == "Coordinador de Brigada"` y `status == TRUE`.
- Los coordinadores del catálogo de zonas deben tener `status_coord == TRUE`.

Si el vocero de un registro no está en el catálogo activo, el campo `vocero` se toma del coordinador asignado (`coalesce(vocero, coordinador)`).

### Normalización de nombres

Los nombres de los entrevistados se normalizan: se convierten a mayúsculas, se eliminan acentos y caracteres especiales (ASCII normalizado), y se concatenan `nombre_entrevistado`, `apellido_paterno` y `apellido_materno`.

### Sección electoral

El número de sección se rellena con ceros a la izquierda hasta 4 dígitos (`str_pad(..., 4, pad = "0")`). Esto permite el join con el catálogo de zonas geográficas `aux_zonas`.

### Columnas de conocimiento y opinión del personaje

La función busca las columnas en este orden de prioridad:
1. `conoce_{personaje}` / `opinion_{personaje}` (columnas específicas al personaje pasado como parámetro)
2. `personaje_conocimiento` / `personaje_opinion` (columnas genéricas de fallback)

Si no encuentra ninguna de las dos versiones, el proceso falla con un error descriptivo.

### Clasificación estratégica (opcional)

Si `clasificacion = TRUE`, se añaden las columnas `grupo` (valores `"1"` a `"6"`) y `categoria` (etiqueta legible). Requiere que se pase el argumento `partido`.

| Grupo | Criterio |
|---|---|
| 1 | Simpatizante del partido **y** conoce al personaje **y** tiene opinión "Buena" |
| 2 | Simpatizante del partido **y** no conoce al personaje |
| 3 | Simpatizante del partido **y** conoce al personaje **y** opinión ≠ "Buena" |
| 4 | No simpatizante **y** conoce al personaje **y** opinión "Buena" |
| 5 | No simpatizante **y** conoce al personaje **y** opinión ≠ "Buena" |
| 6 | No simpatizante **y** no conoce al personaje |

Registros que no caen en ninguna categoría reciben `grupo = NA`.

### Datos personales (PII)

La función maneja información de identificación personal: nombre, domicilio, celular, correo. No persiste datos en disco ni los transmite. La distribución del resultado es responsabilidad del script que llama a la función.

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `personaje` | — | Nombre de la variable del personaje político (requerido) |
| `partido` | `NULL` | Columna de simpatía de partido; requerido si `clasificacion = TRUE` |
| `clasificacion` | `FALSE` | Si `TRUE`, añade columnas `grupo` y `categoria` |
| `na_chr` | `"-"` | Marcador de NA para campos de carácter |
