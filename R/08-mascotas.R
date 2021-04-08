
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
  group_by(REGION) %>% 
  mutate(TOTAL = GATOS + PERROS,
         prop_gato = GATOS/TOTAL,
         prop_perro = PERROS/TOTAL)

gg_mascotas <- ggplot(data = mascotas) +
  geom_segment(aes(x=prop_gato, xend=prop_perro, y=REGION, yend=REGION), color = 'grey50', size = 1.2) +
  geom_point(aes(x=prop_perro, y=REGION, color = 'Perro'), size = 4) +
  geom_point(aes(x=prop_gato, y=REGION, color = 'Gato'), size = 4) + 
  scale_x_continuous('', limits = c(0,1), labels = function(x) scales::percent(x, accuracy = 1)) +
  # scale_y_discrete(limits=c("RM", "Magallanes", "Ays?n", "Los Lagos", "Los R?os", "La Araucan?a", "Biob?o", "?uble", "Maule",
  #                           "O?Higgins", "Valparaiso", "Coquimbo", "Atacama", "Antofagasta", "Tarapac?", "Arica y Parinacota")) +
  scale_shape_manual(values=c(19,20)) +
  scale_colour_manual(values=c("#006633", "#000066")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = 10)) +
  labs(title = "Registro Nacional de Mascotas: Seg?n regi?n",
       subtitle = "Proporci?n de mascotas registradas",
       caption = "Datos obtenidos desde p?gina web de SUBDERE (2019)",
       y ='')

ggsave('lollipop_mascotas2.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
