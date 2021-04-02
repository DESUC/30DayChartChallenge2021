library(sjmisc)
library(tidyverse)
library(waffle)
library(extrafont)
library(showtext)

# Creación de datos ----------------

#Información extraída de: https://www.statista.com/statistics/802811/per-capita-bread-consumption-latam/

datos <- tibble(pais = factor(c('Chile (86 kg)','Argentina (49 kg)','Brasil (31 kg)','Perú (27 kg)','Colombia (21 kg)','México (20 kg)'),
                              levels = c('Chile (86 kg)','Argentina (49 kg)','Brasil (31 kg)','Perú (27 kg)','Colombia (21 kg)','México (20 kg)')),
                cantidad = c(9,5,3,3,2,2))

# Gráfico ----------------

mult_10<- function(x){ 
  x*10 
}

datos %>%
  ggplot(aes(label = pais, values = cantidad))  +
  geom_pictogram(n_rows = 1, color = "#c68958", size = 6) +
  scale_label_pictogram(name = NULL,
                        values = rep(c(pais = "bread-slice",6))) +
  facet_grid(rows = vars(pais)) +
  scale_x_continuous(breaks=seq(1, 9, by = 1),
                     labels = mult_10) + 
  theme_minimal(base_family = "Roboto Condensed", base_size = 7) +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8),
        strip.text.y = element_text(angle = 0, size = 10),
        panel.background = element_rect(fill = "grey90", colour = NA),
        panel.grid = element_blank(),
        strip.background = element_rect(fill = "grey90", colour = NA),
        plot.title = element_text(size = rel(2.5)),
        plot.caption = element_text(size=8)) +
  labs(title = "Consumo anual de pan percápita (en kg)",
       caption = 'Euromonitor, Año 2017')

ggsave('output/02-pictogram.png',
       width = 5,
       height = 3,
       units = 'cm')
