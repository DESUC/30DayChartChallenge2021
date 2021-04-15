

library(tidyverse)

# Con el paquete sinimr es posible obtener datos de Sistema Nacional de Información Municipal:

library(sinimr)

# Para buscar variables de interés en sinimr:

search_sinim_vars("rural")

# Obtuve algunas variables que me interesaba revisar:

areas_verdes <- get_sinim(759, 2017) %>%  select(-4) %>% rename(areas_verdes = value) 
#Metros Cuadrados (M2) de Areas Verdes con Mantenimiento por Habitante
casen <- get_sinim(562, 2017)  %>%  select(-4) %>% rename(casen = value) 
#Porcentaje de Población en Condiciones de Pobreza, según CASEN
psu <- get_sinim(778, 2017)  %>%  select(-4) %>% rename(psu = value) 
#Porcentaje de Puntajes PSU Igual o Superior a 450 Puntos en Establecimientos Municipales de Educación
fcm <- get_sinim(1272, 2017) %>% select(-4) %>% rename(fcm = value) 
#Dependencia del Fondo Común Municipal sobre los Ingresos Propios

# Juntamos las bases obtenidas con sinimr:

df_sinim <- areas_verdes %>% 
  left_join(casen) %>% 
  left_join(psu) %>% 
  left_join(fcm)

# Esta parte la copié de Cara Thompson (https://github.com/cararthompson):

df_sinim <- df_sinim %>%
  filter(!is.na(casen) & !is.na(fcm)) %>% 
  mutate(resid = resid(lm(casen ~ fcm, data = df_sinim)))

# De esta manera, se obtiene el siguiente gráfico:

df_sinim %>% 
  ggplot(aes(y = fcm, x = casen)) +
  geom_point(aes(fill = resid,
                 colour = -resid,
                 alpha = abs(resid)),
             size = 4,  
             shape = 21,
             show.legend = F) +
  geom_smooth(method = "lm", se = F, color = "#8ecae6", size = .5) +
  labs(y = "Dependencia del Fondo Común Municipal sobre los Ingresos Propios",
       x = "Porcentaje de Población en Condiciones de Pobreza, según CASEN",
       title = "Día 13: Correlación #30DayChartChallenge",
       subtitle = "Dependencia del FCM y el porcentaje de población en condiciones de pobreza según CASEN",
       caption = "Fuente: elaboración propia con datos del SINIM.") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#023047", color = NA),
        plot.background = element_rect(fill = "#023047", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title = element_text(color = "#219ebc", size = 10),
        axis.text = element_text(color = "#fb8500", size = 10),
        title = element_text(color = "#fb8500", face = "bold"))

ggsave("output/13-correlation.png", width = 20, height = 14, units = "cm")
