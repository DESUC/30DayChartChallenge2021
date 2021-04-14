library(tidyverse)
library(readxl)
library(emojifont)
library(ggchicklet)

# Datos -------------------------------------------------------------------

#Se utilizan los datos de la ENCUESTA DE OPINIÓN SOBRE PERCEPCIÓN Y USO DE LA BANDA HORARIA ELIGE VIVIR SANO. En: http://www.ipsuss.cl/ipsuss/site/artic/20210405/asocfile/20210405230308/ppt_encuesta_evs_ipsuss_prensa.pdf
data <- readxl::read_xlsx("input/dia7_physical.xlsx") %>% 
  mutate(label = c(emoji('walking'), emoji('bike'), emoji('running'), emoji('muscle'), emoji('dog'), emoji('bow'), emoji('ocean')))

paleta <- c('#a3a948', '#edb92e', '#f85931', '#ce1836', '#009989', '#f2f26f', '#cbe86b')


ggplot(data, aes(x = reorder(factor(P1), +prop), y = prop, fill = P1)) + 
  ggchicklet::geom_chicklet(show.legend = FALSE, radius = grid::unit(6, "pt")) +
  coord_flip() +
  ylim(0,100)+
  geom_text(family = "EmojiOne", aes(label = label), size = 8, vjust = 0.25, hjust = -0.3) +
  geom_text(aes(label = prop), size = 3, hjust = 1.3) +
  labs(title = "Actividad física en banda horaria de plan paso a paso",
       subtitle = "¿Cuál o cuáles de las siguientes actividades realiza \nen la Banda Horaria Elige Vivir Sano? (%) \n",
       caption = "n = 282 quienes realizaron actividad física\nEncuesta de Opinión sobre Percepción y Uso de Banda Horaria Elige Vivir Sano \nde Universidad San Sebastián \nFuente en Script",
       x = "",
       y = "Porcentaje") +
  theme_minimal()+
  scale_fill_manual(values = paleta) + 
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family="serif", face = "italic"),
        plot.title = element_text(size=rel(1.25), face = "bold", hjust = 0.5, color = "#774f38"),
        plot.subtitle = element_text(hjust = 0.5, color = 'grey30'),
        plot.caption = element_text(hjust = 0.5, color = 'grey30', size=rel(0.80)))

# Grabar gráfico
dims <- c(w =  300, h = 200, res = 75)
dims <- dims * 2.2 #Retina

ragg::agg_png('output/07-physical.png', 
              width = dims["w"], 
              height = dims["h"], 
              res = dims["res"],
              units = "px")
last_plot()
invisible(dev.off())
