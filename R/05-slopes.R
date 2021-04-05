library(tidyverse)
library(tidytext)
library(readxl)
library(mdthemes)
library(ggthemes)
library(CGPfunctions)
library(tidyr)
library(dplyr)


# Tema --------------------------------------------------------------------


windowsFonts("Roboto Condensed" = windowsFont("Roboto Condensed"))
theme_desuc <- theme_minimal(base_family = 'Roboto Condensed') +
  theme(plot.title = element_text(size=rel(1.25), face = "bold", hjust = 0.5),
        plot.subtitle    = element_text(hjust = 0.5)) +
  theme(axis.text.x.top = element_text(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  theme(axis.title.y     = element_blank(),
        axis.text.y      = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.ticks = element_blank())+
  theme(plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))
  
theme_set(theme_desuc)


# Datos -------------------------------------------------------------------

#Se utilizan los datos del IMAD 2020 realizado por DESUC y Mujeres empresarias disponible en: https://www.me.cl/wp-content/uploads/2020/11/Ranking-IMAD-2020-para-web.pdf

df_imad <- readxl::read_xlsx("input/dia5_slopes.xlsx")

df_imad_1line <- df_imad %>% 
  filter(tipo_cargo == "Linea ejecutiva principal") %>% 
  mutate(Año = as.character(Año))

# Gráfico -----------------------------------------------------------------

color <- c("#a1dab4", "#41b6c4", "#253494")

newggslopegraph(dataframe = df_imad_1line,
                Times = Año,
                Measurement = prop,
                Grouping = tipo_empresa,
                LineColor = color,
                LineThickness = 1.5,
                YTextSize = 2,
                Title = "Proporción de cargos ocupados por mujeres",
                SubTitle = "Según tipo de empresa y año Ranking IMAD",
                Caption = "Ranking IMAD 2020") +
  theme_desuc

ggsave('output/05-slopes.png',
       width = 5,
       height = 3,
       scale = 3,
       units = 'cm')

