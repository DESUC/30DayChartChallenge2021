# Paquetes

library(tidyverse)
library(rgdal)
library(sf)
library(transformr)
library(readxl)
library(sinimr)
library(gganimate)
library(lubridate)

search_sinim_vars("Aporte Municipal al Sector Educación")

varcode <- 931

df_municipal <- get_sinim(varcode, 2005:2015, 
                 region = 13, 
                 geometry = T, 
                 unit = "comunas")

df_municipal <- df_municipal %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(year = parse_date_time(year, orders = "y")) %>% 
  mutate(year = year(year))

gg_monochrome <- df_municipal %>% 
  ggplot() +
  geom_sf(aes(fill = round(value)), colour = "white", size = .1) +
  transition_time(year) +
  scale_fill_gradient("Aporte municipal al\nsector educación:", low = "black", high = "white") +
  guides(fill = guide_legend(title.position = "top")) +
  labs(title = "Día 24: Monochrome #30DayChartChallenge",
       subtitle = "
       Aporte municipal al sector educacional, comunas de la RM (2005-2015).
       Santiago es y sigue siendo una de las comunas que más aportes entrega a la educación
       municipal, aunque eso no parece extraño. Aun así, es posible observar cómo varía ese
       dato en las comunas periféricas. En regiones, ¿qué comunas creen que tienen
       mayor gasto en educación?
       Año: {round(frame_time)}
       ",
       caption = "Fuente: elaboración propia con datos del Servicio Nacional de Datos Municipales. | @desuc_estudios") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        plot.title = element_text(color = "white", face = "bold", family = "Impact"),
        plot.subtitle = element_text(color = "white", face = "bold", family = "Avenir"),
        # legend.position = 'bottom', 
        # legend.direction = 'horizontal',
        legend.text = element_text(color = "white", family = "Avenir"),
        legend.title = element_text(color = "white", family = "Avenir"))
  
animate(gg_monochrome, height = 600, width = 800)
anim_save("output/24-monochrome.gif")
