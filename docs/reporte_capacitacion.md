# Reporte de Capacitación

**Módulo:** `fct_capacitacion.R`  
**Función principal:** `generar_reporte_capacitacion()`  
**Salida:** Excel (.xlsx) subido a Google Drive, con los datos de usuarios y sus respuestas de cuestionario

---

## ¿Qué hace?

Extrae el padrón de usuarios del proyecto, lo cruza con sus respuestas al cuestionario de capacitación (almacenadas en formato JSON en la base de datos) y sube el resultado a una carpeta de Google Drive. Sirve como insumo para dar seguimiento al proceso de capacitación del equipo operativo.

---

## Supuestos y reglas de negocio

### Filtro de usuarios

Solo se incluyen usuarios que cumplan **ambas** condiciones:
1. Pertenezcan al `id_proyecto` indicado.
2. Tengan `Capacitacion == TRUE` **o** tengan `Cargo == "Coordinador de Brigada"`.

Los coordinadores se incluyen independientemente de su campo `Capacitacion`.

### Exclusión de números de prueba

Los identificadores listados en `numeros_prueba` se excluyen siempre del reporte. Este mecanismo sirve para eliminar cuentas de prueba o de control de calidad que no deben figurar en los reportes oficiales.

### Minimización de datos (ISO 27001 A.14.2.1)

Solo se seleccionan los campos necesarios para el propósito del reporte. De la tabla `Usuarios` se extraen: `Id`, `Municipio`, nombre completo (concatenado), `Num`, `Cargo`, `Status`, `FechaInsert`, `IdBrigada`, `Entrevista`, `Resolucion`, `Capacitacion`. No se incluyen campos adicionales aunque existan en la tabla.

### Join con catálogo de brigadas

El nombre de la brigada (`NombreBrigada`) se incorpora mediante un `left_join` con la tabla de brigadas usando `IdBrigada`. Si la brigada no existe en el catálogo, el campo queda en `NA`.

### Respuestas del cuestionario

Las respuestas se obtienen de la tabla `RegistrosCuestionarios` filtrando por los `Id` de usuario del proyecto. Cada registro contiene un JSON en `JsonData` que se deserializa y transforma a columnas tabulares. El resultado final es un `left_join` entre el padrón de usuarios y sus respuestas, de modo que aparecen todos los usuarios aunque no hayan respondido el cuestionario.

### Validaciones de entrada (fail-fast)

El proceso verifica antes de ejecutar:
- Que `pool` esté presente y sea válido.
- Que la autenticación y acceso a la carpeta de Google Drive funcionen. Si no, el proceso falla con un mensaje descriptivo.
- Que la tabla de capacitación resultante no esté vacía (si lo está, se detiene con error).
- Que la tabla traiga la columna `Id` necesaria para el join con las respuestas.

### Nombre del archivo en Drive

El archivo se nombra con el patrón `{prefijo}_{corte}.xlsx`, donde `prefijo` es el valor de `reporte` (default: `"capacitacion"`) y `corte` es la fecha de corte del reporte.

---

## Parámetros clave

| Parámetro | Default | Descripción |
|---|---|---|
| `corte` | Hoy | Fecha de corte para el nombre del archivo |
| `numeros_prueba` | — | IDs de usuarios a excluir del reporte |
| `brigadas` | `NULL` | Si se pasa, evita la consulta a la BD; si no, la consulta automáticamente |
| `overwrite` | `TRUE` | Sobrescribe el archivo si ya existe en Drive |
| `verbose` | `TRUE` | Genera logs de trazabilidad en consola |
