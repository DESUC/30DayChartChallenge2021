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
  if(!is.null(width)){
    x <- str_wrap(x, width = width)
  }
  
  str_replace_all(x, '\n', '<br>')
}

# Lectura de base de datos
df <- read_csv2('input/dia6_experimental.csv')

caption <- "Fuente: Encuesta de adultos en USA entre el 2 y 11 de junio de 2020;\ndatos observacionales recolectados de 15 de junio a 16 de mayo de 2020.\nEstudio: *Measuring News Consumption in a Digital Era*, PEW RESEARCH CENTER"

colores <- RColorBrewer::brewer.pal(5, 'Spectral')
colores <- colores[c(1, 5)]

# Cambio de nombre de variables

df <- df %>% 
  rename('Autoreporte<br>(activo)' = `Self-report (survey)`,
         'Observación<br>(pasivo)' = `Observed (passive)`)

# Saltos de línea para etiquetas de strips
df <- df %>% 
  mutate(Pregunta = md_str_wrap(Pregunta, width = 20))

# Ajuste de orden de variables
df <- df %>% 
  mutate(across(c(Pregunta, Media), as_factor),
         Media = fct_reorder(Media, `Autoreporte<br>(activo)`))

# Base long para gráfico
df_long <- df %>% 
  pivot_longer(cols = !c(1:3), 
               names_to = 'var', values_to = 'prop')

# Nota:
df_nota <- tibble(prop = 25,
                  Media = 2,
                  Pregunta = unique(df_long[['Pregunta']])[[2]],
                  label = str_glue("<span style = 'color:{colores[2]};'>Observando</span> su navegación,  
                                   vemos que personas <span style = 'color:{colores[1]};'>declaran</span>  
                                   mayor consumo de medios  
                                   respecto de lo que  
                                   realmente hacen.
                                   "))

gg <- df_long %>% 
  ggplot(aes(x = prop,
             y = Media,
             colour = var)) + 
  geom_linerange(data = df,
                 inherit.aes = FALSE,
                 aes(y = Media,
                     xmin = `Autoreporte<br>(activo)`,
                     xmax = `Observación<br>(pasivo)`),
                 size = rel(1.5),
                 colour = 'gray60') +
  geom_point(size = rel(4),
             colour = 'white') +
  geom_text(aes(label = prop),
            family = 'Roboto Condensed',
            fontface = 'bold') +
  facet_grid(rows = vars(Pregunta),
             switch = 'y', 
             space = 'free_y',
             scales = 'free_y') +
  geom_richtext(data = df_nota,
                aes(label = label),
                family = 'Roboto Condensed',
                hjust = 0,
                lineheight = 0,
                size = rel(3.5),
                colour = 'gray10') +
  scale_colour_manual(values = colores,
                      guide = 'none') + 
  scale_x_continuous('Porcentaje',
                     limits = c(0, 100),
                     labels = NULL,
                     expand = expansion(add = 0)) +
  labs(title = str_glue("Diferencia entre 
                        <span style = 'color:{colores[2]};'>**observación**</span>
       y <span style = 'color:{colores[1]};'>**declaración**</span> de consumo de medios"),
       y = 'Medios',
       caption = md_str_wrap(caption, width = NULL)) + 
  md_theme_ipsum_rc() +
  coord_cartesian(clip = 'off') + 
  theme(legend.position = 'top',
        plot.title.position = 'plot',
        plot.title = element_markdown(size = rel(1.2)),
        strip.placement = 'outside',
        strip.text.y.left = element_markdown(angle = 0,
                                             vjust = 1))

pg <- ggplotGrob(gg)

for(i in which(grepl("strip-l", pg$layout$name))){
  pg$grobs[[i]]$layout$clip <- "off"
}

# Grabar gráfico
dims <- c(w =  400, h = 600, res = 72)
dims <- dims * 2 #Retina

ragg::agg_png('output/06-experimental.png', 
        width = dims["w"], 
        height = dims["h"], 
        res = dims["res"],
        units = "px")
grid::grid.draw(pg)
invisible(dev.off())

