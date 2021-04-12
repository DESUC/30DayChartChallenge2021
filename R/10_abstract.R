### Paquetes ----

library(spotifyr)
library(tidyverse)
library(sjmisc)

spotify_df <- readRDS("taylor_df.rds")

# Función para guardar en el mismo formato ambos gráficos:

ggsave_estandar <- function(filename,
                            width = 20,
                            height = 14){
  ggsave(filename = filename,
         plot = last_plot(),
         width = width,
         height = height,
         units = 'cm')
}

# Extraer la información de las canciones de Taylor Swift

spotify_df <- get_artist_audio_features('taylor swift')

# Seleccionaremos sólo algunas columas que nos interesan:

taylor_df <- spotify_df %>%  select(album_name, album_release_year, track_name, track_number, danceability, energy, speechiness) %>% 
  filter(album_name %in% c("1989","evermore","Fearless","folklore","Lover","Red","reputation","Speak Now","Taylor Swift"))

# Hice una paleta con los colores del album 'Lover' de Taylor Swift utilizando la paleta de esta página (https://coolors.co/61aed9-fea8b6-fdb8db-3980bb-d6638f-9fb2cf-fec3b4-fdebb7)

lover_palette <- c("#61aed9","#fea8b6","#fdb8db","#3980bb","#d6638f","#9fb2cf","#fec3b4","#fdebb7","#fae1dd")

# El gráfico abstracto:

taylor_df %>% 
  mutate(album_name = str_to_lower(album_name)) %>% 
  ggplot(aes(y = danceability, x = energy, colour = album_name)) +
  geom_point(aes(size = speechiness), alpha = 0.8) +
  scale_color_manual(values = lover_palette, '') +
  scale_size_continuous(range = c(10,15)) +
  labs(x = 'energía', 
       y = 'danzabilidad',
       title = 'gráfico abstracto utilizando las métricas de las canciones de taylor swift',
       caption = 'tamaño del círculo varía según el porcentaje de habla en cada una de las canciones. \nentre más grande el círculo, más letra tiene.\nfuente: spotify',
       subtitle = 'danzabilidad, energía y habla en las canciones de taylor swift') +
  guides(size = F, colour = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        legend.key.size = unit(.5, "cm"),
        panel.background = element_rect(fill = "aliceblue", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family="Georgia", face = "italic"))

ggsave_estandar(filename = "taylor_swift.png")

#Para que geom_text tenga el mismo tipo de letra que utilizamos en el gráfico:

theme_set(theme_minimal(base_family = "Georgia"))

update_geom_defaults("text", list(family = theme_get()$text$family))

# Gráfico con el track en cada uno de los puntos, así pueden ver sus características y buscarla:

taylor_df %>% 
  mutate(album_name = str_to_lower(album_name)) %>% 
  ggplot(aes(y = danceability, x = energy, colour = album_name)) +
  geom_point(aes(size = speechiness), alpha = 1) +
  geom_text(aes(label = track_number),
            show.legend = F, colour = "white") +
  scale_color_manual(values = lover_palette, '') +
  scale_size_continuous(range = c(10, 15)) +
  labs(x = 'energía', 
       y = 'danzabilidad',
       title = 'gráfico abstracto utilizando las métricas de las canciones de taylor swift',
       caption = 'tamaño del círculo varía según el porcentaje de habla en cada una de las canciones. \nentre más grande el círculo, más letra tiene.\nfuente: spotify',
       subtitle = 'danzabilidad, energía y habla en las canciones de taylor swift') +
  guides(size = F, colour = guide_legend(nrow = 1, override.aes = list(size = 5))) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        legend.key.size = unit(.5, "cm"),
        panel.background = element_rect(fill = "aliceblue", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family="Georgia", face = "italic"))

ggsave_estandar(filename = "taylor_swift_tracks.png")
