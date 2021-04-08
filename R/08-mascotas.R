
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

ggplot(data = mascotas,
       aes(y = REGION)) +
  geom_segment(aes(x=prop_gato, 
                   xend=prop_perro, 
                   yend = REGION,
                   colour = prop_diff),
               size = 1.2) +
  geom_text(aes(x=prop_perro), 
            label = '🐶',
            size = 6) +
  geom_text(aes(x=prop_gato), 
            label = '🐱',
            size = 6) + 
  scale_x_continuous('', 
                     limits = c(0,1), 
                     labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_shape_manual(values=c(19,20)) +
  scale_color_viridis_c(guide = 'none') + 
  # scale_colour_manual(values=c("#006633", "#000066")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = 10)) +
  labs(title = "Registro Nacional de Mascotas: Según región",
       subtitle = "Proporción de mascotas registradas",
       caption = "Datos obtenidos desde página web de SUBDERE (2019)",
       y = NULL) +
  theme(plot.title.position = 'plot',
        plot.title = element_text(face = 'bold'))

ggsave('lollipop_mascotas2.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
