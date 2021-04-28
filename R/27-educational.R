library(tidyverse)
library(readxl)
library(sjmisc)
library(sjlabelled)
library(desuctools)


# Funcion guardar ---------------------------------------------------------

gg_save <- function(name,
                    plot = last_plot(),
                    width = 7,
                    height = 6) {
  ggsave(filename = str_glue("output/{name}.png"),
         plot = plot,
         width = width,
         height = height)
}


# Base de datos -----------------------------------------------------------

#Los datos se encuentran disponibles de manera abierta en la página de la Agencia de Calidad de la Educación: https://informacionestadistica.agenciaeducacion.cl/#/bases

base_2019 <- readxl::read_xlsx("input/idps19_rbd.xlsx") %>% 
  set_labels(cod_grupo, labels = c('Bajo', 'Medio bajo', 'Medio', 'Medio alto', 'Alto'))


# Tabla -------------------------------------------------------------------

tabla <- tabla_vars_segmentos(.data = base_2019,
                              .vars = vars(ind_pf, ind_cc),
                              .segmentos = vars(cod_grupo),
                              miss = c(NA)) %>% 
  filter(!is.na(pregunta_cat)) %>% 
  filter(!is.na(segmento_cat)) %>% 
  filter(pregunta_var=="ind_cc") %>% 
  mutate(media = round(mean(tabla$pregunta_cat), digits = 1))


# Gráficos ----------------------------------------------------------------


windowsFonts("Roboto Condensed" = windowsFont("Roboto Condensed"))

tabla %>% 
  ggplot(aes(x = segmento_cat, y = pregunta_cat, fill = segmento_cat, color = segmento_cat)) +
  geom_jitter(alpha = 0.2, show.legend = FALSE)+
  geom_violin(scale="width", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "#935887", show.legend = FALSE) +
  geom_hline(yintercept=mean(tabla$pregunta_cat), size=0.5, color = "#935887", linetype = "dashed") +
  scale_fill_manual(values = c('#FBC6A4', '#F4A9A8', '#CE97B0', '#AF9CB8', '#85C0D9')) +
  scale_color_manual(values = c('#FBC6A4', '#F4A9A8', '#CE97B0', '#AF9CB8', '#85C0D9')) +
  labs(title = "Puntaje Indicador Clima de Convivencia escolar del colegio",
       subtitle = "Según grupo socioeconómico, medición en 8° básico 2019",
       x = "Grupo socioeconómico", 
       y = "Puntaje indice",
       caption = "Datos de la Agencia de Calidad de la Educación año 2019") +
  #ylim(0,100) +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        plot.background = element_rect(fill = "aliceblue", color = NA),
        plot.title = element_text(colour = "grey10", size = 12, hjust = 0.5),
        plot.subtitle = element_text(colour = "grey30", size = 12, hjust = 0.5),
        plot.caption = element_text(colour = "grey30", size = 10, hjust = 0.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey90"),
        panel.grid.minor = element_blank(),
        text = element_text(family="Roboto Condensed"))


gg_save('27-educational_v1')

tabla %>% 
  ggplot(aes(x = segmento_cat, y = pregunta_cat, fill = segmento_cat, color = segmento_cat)) +
  geom_jitter(alpha = 0.2, show.legend = FALSE)+
  geom_violin(scale="width", show.legend = FALSE) +
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "#935887", show.legend = FALSE) +
  geom_hline(yintercept=mean(tabla$pregunta_cat), size=0.5, color = "#935887", linetype = "dashed") +
  scale_fill_manual(values = c('#FBC6A4', '#F4A9A8', '#CE97B0', '#AF9CB8', '#85C0D9')) +
  scale_color_manual(values = c('#FBC6A4', '#F4A9A8', '#CE97B0', '#AF9CB8', '#85C0D9')) +
  labs(title = "Puntaje Indicador Clima de Convivencia escolar del colegio",
       subtitle = "Según grupo socioeconómico, medición en 8° básico 2019",
       x = "Grupo socioeconómico", 
       y = "Puntaje indice",
       caption = "Datos de la Agencia de Calidad de la Educación año 2019") +
  ylim(0,100) +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        plot.background = element_rect(fill = "aliceblue", color = NA),
        plot.title = element_text(colour = "grey10", size = 12, hjust = 0.5),
        plot.subtitle = element_text(colour = "grey30", size = 12, hjust = 0.5),
        plot.caption = element_text(colour = "grey30", size = 10, hjust = 0.5),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "grey90"),
        panel.grid.minor = element_blank(),
        text = element_text(family="Roboto Condensed"))

gg_save('27-educational_v2')