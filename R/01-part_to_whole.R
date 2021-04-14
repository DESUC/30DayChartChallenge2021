library(sjmisc)
library(tidyverse)
library(desuctools)
library(haven)
library(waffle)
library(extrafont)

# Lectura de base de datos ----------------

#Pueden descargar la base de datos en: https://www.cntv.cl/estudios-y-estadisticas/encuesta-nacional-de-television/
df_cntv <- read_sav("input/Base Datos IX ENTV CNTV.sav") %>%
  mutate(total = 1)

# Cálculo de los datos y preparación ----------------

datos <- tabla_vars_segmentos(df_cntv,
                              .vars = vars(P11),
                              .segmentos = vars(total),
                              .wt = PONDERADOR)

datos <- data.frame(resp = c('Sí (61%)','No (39%)'),
                    prop = c(61,39))

# Gráfico ----------------

datos %>%
  ggplot(aes(fill = resp, values = prop))  +
  geom_waffle(n_rows = 10, size = 0.8, colour = "white", radius = unit(3, "pt")) +
  scale_fill_manual(name = NULL,
                    values = c("gray60","#00525E"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid = element_blank()) +
  labs(title = '¿Ve programas, series o películas en familia?',
       caption = 'Encuesta Nacional de Televisión 2017')

ggsave('output/01-part-to-whole.png',
       width = 6,
       height = 5,
       scale = 1.5,
       units = 'cm')
