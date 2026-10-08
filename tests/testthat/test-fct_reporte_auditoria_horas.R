# tests/testthat/test-fct_reporte_auditoria_horas.R

hora <- function(x) as.POSIXct(x, tz = "UTC")

bd_horas <- function() {
  tibble::tibble(
    usuario_num = c("001", "001", "001", "001", "001",  "002", "002", "003"),
    # lun 5, lun 5 (2º registro), mar 6, sáb 3 (fin de semana), vie 9 fuera de rango
    fecha = as.Date(c("2026-10-05", "2026-10-05", "2026-10-06", "2026-10-03", "2026-10-09",
                      "2026-10-05", "2026-10-07", "2026-10-06")),
    fecha_inicio = hora(c("2026-10-05 16:00:00", "2026-10-05 18:00:00", "2026-10-06 16:00:00",
                          "2026-10-03 16:00:00", "2026-10-09 16:00:00",
                          "2026-10-05 17:00:00", "2026-10-07 17:00:00", NA)),
    fecha_fin = hora(c("2026-10-05 17:00:00", "2026-10-05 22:00:00", "2026-10-06 20:00:00",
                       "2026-10-03 23:00:00", "2026-10-09 23:00:00",
                       "2026-10-05 19:00:00", "2026-10-07 18:00:00", "2026-10-06 20:00:00"))
  )
}

test_that("horas_jornada_promedio usa max(fin) - min(inicio) por día y promedia los días hábiles", {
  res <- horas_jornada_promedio(bd_horas(), as.Date("2026-10-03"), as.Date("2026-10-08"))

  # 001: lun 5 = 16:00 -> 22:00 = 6 h; mar 6 = 4 h; el sábado 3 no cuenta; el vie 9 queda fuera del rango.
  expect_equal(res$horas_jornada_promedio[res$usuario_num == "001"], 5)
  # 002: lun 5 = 2 h; mié 7 = 1 h  -> 1.5 (los días sin registros no cuentan como cero).
  expect_equal(res$horas_jornada_promedio[res$usuario_num == "002"], 1.5)
  # 003: su único registro no tiene fecha_inicio -> sin jornada, no aparece.
  expect_false("003" %in% res$usuario_num)
})

test_that("horas_jornada_promedio devuelve tibble vacío si el rango no tiene días hábiles con registros", {
  res <- horas_jornada_promedio(bd_horas(), as.Date("2026-10-10"), as.Date("2026-10-11"))
  expect_equal(nrow(res), 0)
  expect_named(res, c("usuario_num", "horas_jornada_promedio"))
})
