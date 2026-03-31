# Si no los tienes, instálalos primero: install.packages(c("usethis", "devtools"))
library(usethis)
library(devtools)

# 1. Crear el paquete (cambia la ruta a donde guardes tus repositorios)
# Es convención usar CamelCase o todo minúsculas sin espacios. Usaremos DialogoSocial
usethis::create_package(".")

# 2. Inicializar Git
usethis::use_git()
# Manipulación de datos y strings
usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("stringr")
usethis::use_package("lubridate")
usethis::use_package("purrr")
usethis::use_package("rlang")

# Bases de datos y JSON
usethis::use_package("dbplyr")
usethis::use_package("DBI")
usethis::use_package("jsonlite")

# Tablas de alto rendimiento
usethis::use_package("data.table")

# Exportación y Reportes
usethis::use_package("flextable")
usethis::use_package("officer")
usethis::use_package("openxlsx")
usethis::use_package("janitor")
usethis::use_package("cli")


usethis::use_github("morant-consultores", private = T)

devtools::document()
usethis::use_test("fct_secciones")
usethis::use_test("fct_reporte_auditoria")
usethis::use_test("fct_publicacion_drive")
usethis::use_test("fct_capacitacion")
usethis::use_test("fct_paseLista")
usethis::use_package("RSQLite", type = "Suggests")


usethis::use_build_ignore("dev")
