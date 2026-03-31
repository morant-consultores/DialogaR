# =========================================================================
# SCRIPT: 02_run_pase_lista.R
# UBICACIÓN: inst/scripts/
# OBJETIVO: Actualización dinámica del cuestionario de pase de lista en BD
# =========================================================================

library(DialogaR)

# 1. Parameters
id_proyecto           <- 17
id_pase_lista         <- 294
ids_encuestas_dialogo <- c(292, 2923)

# 2. Connect to DB
pool <- conectar_base_datos()

# 3. Execute
actualizar_pase_lista(
  pool                  = pool,
  id_proyecto           = id_proyecto,
  id_pase_lista         = id_pase_lista,
  ids_encuestas_dialogo = ids_encuestas_dialogo
)

# 4. Close connection
pool::poolClose(pool)
