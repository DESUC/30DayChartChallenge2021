
# Paquetes

library(tidyverse)
library(rgdal)
library(sf)
library(patchwork)
library(readxl)
library(extrafont)

# Carga de shapefile de las comunas de Chile, seleccionamos sólo las comunas de la RM y provincia de Santiago.

df_rm <- sf::st_read(dsn = "input/Comunas", layer = "comunas") %>% 
  filter(Region == "Región Metropolitana de Santiago") %>% 
  filter(Provincia == "Santiago")

# Cargamos el shapefile con las coordenadas de las estaciones del metro de Stgo.

df_metro <- sf::st_read(dsn = "input/Estaciones_actuales_Metro_de_Santiago", layer = "Estaciones_actuales_Metro_de_Santiago.")

# Hacer la paleta de colores de acuerdo a los colores institucionales:

#db3a34 rojo (linea 1)
#ffd500 amarillo (linea 2)
#6d4c3d café (linea 3)
#0072bb azul (linea 4)
#1e91d6 azul claro (linea 4A)
#007f5f verde (linea 5)
#b5179e violeta (linea 6)

metro_stgo <- c("#db3a34","#ffd500","#6d4c3d","#0072bb","#1e91d6","#007f5f","#b5179e", "black")
names(metro_stgo) <- levels(df_metro$linea)

# Gráfico del mapa:

mapa <- ggplot() +
  geom_sf(data = df_rm, fill = "#2b2d42", color = "#edf2f4") +
  geom_sf(data = df_metro, aes(geometry = geometry, color = linea)) +
  scale_colour_manual('', values = metro_stgo) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  theme(panel.background = element_rect(fill = "#2b2d42", color = NA),
        plot.background = element_rect(fill = "#2b2d42", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        legend.direction = 'horizontal',
        legend.key.size = unit(.5, "cm"),
        legend.background = element_rect(fill = "#2b2d42"),
        legend.key = element_rect(fill = "#2b2d42"),
        legend.text = element_text(colour = "#edf2f4", family = "Trebuchet MS"))

mapa

# Cargamos la información de movilidad obtenida del INE -> Transporte y comunicaciones.

info_metro <- read_excel("input/metro.xlsx") %>% 
  pivot_longer(cols = 4:7)

info_metro <- info_metro %>% 
  mutate(red = factor(red, levels = c("linea_1","linea_2","linea_3","linea_4","linea_4a","linea_5","linea_6","total_red"),
                      labels = c("Línea 1","Línea 2","Línea 3","Línea 4","Línea 4A","Línea 5","Línea 6","Total"))) %>% 
  mutate(name = factor(name, levels = c("pasajeros_comunes","escolares_total","escolares_pagados","escolares_basicos"),
                       labels = c("Pasajeros comunes","Escolares (total)","Escolares pagados","Escolares básicos")))

names(metro_stgo) <- levels(info_metro$red)

# Gráfico con la movilidad:

informacion <- info_metro %>% 
  filter(red != "Total") %>% 
  ggplot(aes(x = fecha, y = value, colour = red)) +
  geom_line(aes(group = red)) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",", accuracy = 0.1)) +
  scale_colour_manual('', values = metro_stgo) +
  labs(x = NULL, y = "Número de pasajeros") +
  #guides(colour = guide_legend(title.position = "top", nrow = 1, override.aes = list(size = 5))) +
  guides(colour = F) +
  facet_wrap(~name, nrow = 1) +
  theme_minimal() +
  theme(axis.text = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
        axis.title = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family = "Trebuchet MS"),
        strip.text = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
        panel.background = element_rect(fill = "#2b2d42", color = NA),
        plot.background = element_rect(fill = "#2b2d42", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  
informacion

# Utilizamos patchwork para unir ambos gráficos:

connections <- informacion / mapa

# Agregar los detalles como el color del fondo y títulos:

connections +
  plot_layout(widths = c(5, 5), heights = c(3,6)) +
  plot_annotation(
    title = "Día 18: Connections #30DayChartChallenge",
    subtitle = "
Número de pasajeros transportados en el Metro de Santiago (a febrero de 2021).
El tema era 'conexión' y, hace bastante, tenía la idea de hacer algo con los datos que entrega
el INE sobre transporte y telecomunicaciones. En este caso, usé los datos de pasajeros transportados
desde enero del 2020 a febrero del 2021. Es posible ver una caída en el uso de metro con las
primeras cuarentenas realizadas en Santiago, que repunta en enero de este año.
Hice el mapa porque, además, tenía ganas de hacer algo con el paquete patchwork que
empecé a ocupar hace muy poco y no conocía muy bien sus funciones. Espero, próximamente,
hacer un mapa que tenga contenido sobre movilidad en las estaciones del metro. O algo.
",
    caption = "Fuente: elaboración propia con datos del Instituto Nacional de Estadísticas. | @desuc_estudios",
    theme = theme(plot.background = element_rect(fill = "#2b2d42", color = NA),
                  plot.subtitle = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
                  plot.caption = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
                  plot.title = element_text(colour = "#edf2f4", face = "bold", family = "Impact")))


ggsave("output/18-connections.png", width = 22, height = 22, units = "cm")

                  