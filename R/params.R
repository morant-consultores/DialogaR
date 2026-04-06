# Parametros --------------------------------------------------------------

# Opinion -----------------------------------------------------------------

tema_m <- function(base_size = 15, base_family = "Poppins", fondo="#FFFFFF") {
  (ggthemes::theme_foundation(base_size = base_size,
                              base_family = base_family)
   + theme(
     line = element_line(colour = "#4C5B61"),
     rect = element_rect(fill = fondo,
                         linetype = 0,
                         colour = NA),
     panel.background = element_rect(fill='transparent'), #transparent panel bg
     plot.background = element_rect(fill='transparent', color=NA),
     text = element_text(color = "#2C423F"),
     axis.title = element_blank(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line.x = element_line(colour = "#E1356D"),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     legend.text = element_text(size = 11),
     panel.grid = element_line(colour = NULL),
     panel.grid.major.y = element_blank(),
     panel.grid.major.x = element_line(colour = "#C5C5C5",
                                       linetype = "dotted"),
     panel.grid.minor = element_blank(),
     plot.title = element_text(hjust = 0,
                               size = rel(1.1),
                               # face = "bold",
                               colour = "#4C5B61"),
     plot.title.position = "plot",
     plot.subtitle = element_text(hjust = 0,
                                  size = rel(1),
                                  face = "bold",
                                  colour = "#C5C5C5",
                                  family = "Poppins"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.text = element_text(colour ="#2C423F")))
}

color_buena <- "#52B788"
color_regular <- "#D5B9B2"
color_mala <- "#606299"


font_family <- "Poppins"

color_meta <- color_buena
color_efectivo <- color_mala
color_efectivo_complemento <- "#F5CD5F"
color_vivienda <- color_regular

color_linea <- "#709775"
color_cobertura <- "#A6032F"
color_cobertura_complemento <- "#F5CD5F"

color_duracion_alta <- "#52B788"
color_duracion_optima <- "#D5B9B2"
color_duracion_optima_complemento <- "#FF704D"
color_duracion_baja <- "#606299"

color_vocero <- "#6B3A8C"

# Generales ---------------------------------------------------------------

color_general <- "#CF6177"

color_verde <- "#AAD989"
color_amarillo <- "#D9BA5F"
color_rojo <- "#BE3E35"
color_no_se_trabaja <- "gray70"

color_si <- "#709775"
color_no <- "#4A4E69"

color_nsnc <- "#9a8c98"
color_otro <- "gray30"
color_ninguno <- "black"

color_joven <- "#a7c957"
color_adulto <- "#708d81"
color_mayores <- "#3c6e71"

# Que tan -----------------------------------------------------------------

color_mucho <- "#2D6A4F"
color_algo <- "#52B788"
color_poco <- "#606299"
color_nada <- "#4A4E69"

# Partido ----------------------------------------------------------------

color_MORENA <- "#7d1f32"
color_MORENA_complemento <- "#F5CD5F"
color_PANAL <- "#03A6A6"
color_PT <- "#D91136"
color_PRI <- "#038C33"
color_MC <- "#F27405"
color_PVEM <- "#98BF5E"
color_PAN <- "#0339a6"
color_PRD <- "#F2B705"

color_futuro <- "#2b0541"
color_hagamos <- "#8323CD"

color_rsp <- color_PRD

color_CHISUNIDO <- "#0396A6"
color_PMCHIS <- "#6B3A8C"
color_PENCSOLCHIS <- "#AE95BF"
color_RSPCHIS <-"#D9526B"

# Personajes locales ------------------------------------------------------

color_zoe <-"#A6032F"
color_era <-"#00AF52"
color_sasil <-"#693589"
color_jaac <-"#006466"
color_placido <-"#184e77"
color_rosaurbina <-"#ff6392"
color_pepecruz <- "#ac3568"
color_manuelita<-"#9b5de5"
color_carlosmorales <-"#A6032F"
color_marden<-"#2ec4b6"
color_robertoalbores <-"#8ac926"
color_luismelgar <-"#008000"
color_mariaorantes <-"#d90429"
color_patriaarmendariz <-"#f48c06"
color_zebadua<-"#c44536"

color_independiente <- "gray40"

color_hombres <- color_MORENA
color_mujeres <- "#b53881"

# Otros -------------------------------------------------------------------

facet_text <- "#A6032F"
