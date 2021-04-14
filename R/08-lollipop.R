
library(tidyverse)
library(desuctools)

# Información rescatada de 
# http://www.subdere.gov.cl/sala-de-prensa/las-curiosidades-del-registro-nacional-de-mascotas-dos-a%C3%B1os-de-su-obligatoriedad 


REGION <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "O'Higgins", "Maule", 
            "Ñuble", "Biobío", "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes", "Metropolitana")
GATOS <- c(2479, 4766, 10794, 7400,  9452, 32395, 18002, 18595, 8838, 24465, 20805, 12022, 21626, 4780,
           3431, 111544)
PERROS <- c(9064, 28394, 40202, 26817, 39417, 127962, 94474, 97831, 40216, 97807, 63638, 31699, 60374, 12014,
            10844, 437429)

mascotas <- tibble(REGION, 
                   GATOS, 
                   PERROS)

mascotas <- mascotas %>% 
  mutate(REGION = as_factor(REGION),
         REGION = fct_rev(REGION)) %>% 
  group_by(REGION) %>% 
  mutate(TOTAL = GATOS + PERROS,
         prop_gato = GATOS/TOTAL,
         prop_perro = PERROS/TOTAL,
         prop_diff = prop_perro - prop_gato)

# Color a partir de emojis
color <- c(perro = '#A96500',
           gato = '#F8CF37')

ggplot(data = mascotas,
       aes(y = REGION)) +
  geom_segment(aes(x=prop_gato, 
                   xend=prop_perro, 
                   yend = REGION,
                   colour = prop_diff),
               size = 1.5) +
  geom_text(aes(x=prop_perro), 
            label = '🐶',
            size = 6) +
  geom_text(aes(x=prop_gato), 
            label = '🐱',
            size = 6) + 
  scale_x_continuous('', 
                     limits = c(0,1), 
                     labels = scales::percent,
                     expand = expansion(add = c(0, 0.05))) +
  scale_colour_gradient(low = color['gato'],
                        high = color['perro'],
                        guide = 'none') + 
  labs(title = "Registro Nacional de Mascotas según región",
       subtitle = "Proporción de gatos y perros registrados respecto del total",
       caption = "Datos obtenidos desde página web de SUBDERE (2019)",
       y = NULL) +
  theme_minimal() +
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(size = rel(1)),
        axis.text.y = element_text(size = 10))

# Grabar gráfico
dims <- c(w =  400, h = 400, res = 72)
dims <- dims * 2 #Retina

ragg::agg_png('output/08-lollipop_mascotas.png', 
              width = dims["w"], 
              height = dims["h"], 
              res = dims["res"],
              units = "px")
last_plot()
invisible(dev.off())
