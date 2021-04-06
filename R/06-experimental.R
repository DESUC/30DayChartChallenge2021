# DESUC - 6 experimental
# 
# #30DayChartChallenge2021
# 
# Measuring News Consumption in a Digital Era
# Fuente: https://www.journalism.org/2020/12/08/measuring-news-consumption-in-a-digital-era/
# 
# Comparación entre consumo de medios declarado y observado.
# 
# Pregunta:
# Estimates of online behaviors far higher in self-reported data than passive data
# % of respondents who ..."

library(tidyverse)
library(ggtext)
library(mdthemes)

md_str_wrap <- function(x, 
                        width = 80){
  str_wrap(x, width = width) %>% 
    str_replace_all('\n', '<br>')
}


# Lectura de base de datos
df <- read_csv2('input/dia6_experimental.csv')

caption <- "Source: Survey of U.S. adults conducted June 2-11, 2020; passive data collected May 16-June 15, 2020.\nMeasuring News Consumpüon in a Digital Era.
PEW RESEARCH CENTER"

# Cambio de nombre de variables

df <- df %>% 
  rename('Autoreporte\n(activo)' = `Self-report (survey)`,
         'Observación\n(pasivo)' = `Observed (passive)`)

# Ajuste de orden de variables
df <- df %>% 
  mutate(across(c(Pregunta, Media), as_factor),
         Media = fct_reorder(Media, `Autoreporte\n(activo)`))

# Base long para gráfico
df_long <- df %>% 
  pivot_longer(cols = !c(1:3), 
               names_to = 'var', values_to = 'prop')

df_long %>% 
  ggplot(aes(x = prop,
             y = Media,
             fill = var, colour = var)) +
  # geom_col() +
  # geom_point(size = rel(3)) + 
  geom_text(aes(label = prop),
            family = 'Roboto Condensed',
            fontface = 'bold') + 
  facet_grid(rows = vars(Pregunta),
             switch = 'y', 
             space = 'free_y',
             scales = 'free_y') +
  scale_x_continuous('Porcentaje',
                     limits = c(0, 100),
                     expand = expansion(add = 0)) +
  labs(title = 'Hola mundo',
       caption = md_str_wrap(caption, width = 50)) + 
  md_theme_ipsum_rc() +
  theme(legend.position = 'top',
        plot.title.position = 'plot')

