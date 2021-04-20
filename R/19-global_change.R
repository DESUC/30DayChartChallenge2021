library(tidyverse)
library(countrycode)
library(patchwork)

df_gender <- read_csv("input/dia19_globalchange.csv") %>% 
  janitor::clean_names() %>% 
  filter(indicator == "Overall Global Gender Gap Index") %>% 
  mutate(region = countrycode(sourcevar = country_iso3,
                              origin = "iso3c",
                              destination = "region")) %>% 
  filter(region %in% c("Latin America & Caribbean","North America"))

df_gender_longer_index <- df_gender %>% 
  filter(subindicator_type == "Index") %>% 
  pivot_longer(cols = 6:20) %>% 
  mutate(name = as.numeric(gsub("x","", name))) %>% 
  group_by(country_iso3, country_name) %>% 
  mutate(value_lag = value - lag(value)) %>% 
  mutate(category = case_when(
    value_lag < 0 ~ "Negativo",
    value_lag > 0 ~ "Positivo",
    value_lag == 0 ~ "Sin cambios",
    T ~ "Sin información"
)) %>% 
  filter(name == 2020) %>% 
  select(-region)

# Mapa: cambios en el índice de brecha de género

df_mapa <- map_data('world') %>% 
  filter(region != "Antarctica") %>% 
  mutate(iso3c = countrycode(sourcevar = region,
                              origin = "country.name",
                              destination = "iso3c")) %>% 
  left_join(df_gender_longer_index, by = c("iso3c" = "country_iso3")) %>% 
  filter(!is.na(country_name))

p <- ggplot() +
  geom_map(data = df_mapa, map = df_mapa, aes(x = long, y = lat, group = group, map_id = region, fill = category), 
           color = "#293241", size = 0.05) +
  scale_fill_manual(name = "Cambios en el Índice de Brechas de Género 2020-2019:", values = c("#ee6c4d","#3d5a80","#98c1d9","#e0fbfc")) +
  guides(fill = guide_legend(title.position = "top", nrow = 1, override.aes = list(size = 5))) +
  labs(title = "Diferencias en el Índice de Brechas de Género\nentre el 2019 y 2020") +
  scale_x_continuous(limits = c(-180,-25)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = "#293241", color = NA),
        plot.background = element_rect(fill = "#293241", color = NA),
        plot.title = element_text(colour = "#edf2f4", family = "Trebuchet MS", size = 12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'top', 
        legend.direction = 'horizontal',
        legend.key.size = unit(.5, "cm"),
        legend.background = element_rect(fill = "#293241"),
        legend.key = element_rect(fill = "#293241", colour = "#293241"),
        legend.text = element_text(colour = "#e0fbfc", family = "Trebuchet MS"),
        legend.title = element_text(colour = "#e0fbfc", family = "Trebuchet MS"))

p

# Mapa: ranking 

df_gender_longer_rank <- df_gender %>% 
  filter(subindicator_type == "Rank") %>% 
  pivot_longer(cols = 6:20) %>% 
  mutate(name = as.numeric(gsub("x","", name))) %>% 
  filter(name == 2020) %>% 
  arrange(value) %>% 
  rename(year = name,
         rank = value) %>% 
  select(-subindicator_type, -indicator_id)

df_gender_longer_index <- df_gender_longer_index %>% 
  rename(year = name) %>% 
  select(-subindicator_type, -indicator_id)

df_gender_longer_rank <- df_gender_longer_rank %>% 
  full_join(df_gender_longer_index)

q <- df_gender_longer_rank %>% 
  filter(!is.na(value)) %>% 
  mutate(country_rank = paste0(country_name, sep = ", ", rank)) %>% 
  ggplot(aes(x = fct_reorder(country_rank, value), y = value)) +
  geom_col(fill = "#ee6c4d", width = .5) +
  geom_text(aes(label = value), colour = "#ee6c4d", hjust = -.2, fontface = "bold", family = "Trebuchet MS", size = 2) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "Países de América - Posición en el ranking internacional",
       y = "Global Gender Gap Index",
       title = "Posición en el Global Gender Gap Index\nPaíses de América (2020)") +
  coord_flip() +
  theme(axis.text = element_text(colour = "#e0fbfc", family = "Trebuchet MS"),
        axis.title = element_text(colour = "#e0fbfc", family = "Trebuchet MS", size = 8),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#293241", color = NA),
        plot.background = element_rect(fill = "#293241", color = NA),
        plot.title = element_text(colour = "#edf2f4", family = "Trebuchet MS", size = 12),
        plot.title.position = "plot",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

q

gender_gap <- p | q

gender_gap +
  plot_layout(widths = c(10, 3)) +
  plot_annotation(
    title = "Día 19: Global Change #30DayChartChallenge",
    subtitle = "
Diversas investigaciones han demostrado que las crisis económicas golpean fuentemente el empleo femenino.
Por esa razón, una de las principales preocupaciones con la pandemia de COVID-19 fue que las brechas entre 
hombre y mujeres aumentaran, en vez de disminuir. En marzo de este año, el Foro Económico Mundial publicó
un reporte con los datos obtenidos y, una de las principales conclusiones, es que con la trayectoria actual
tomará aproximadamente 135.6 años en cerrarse las brechas de género a nivel mundial, donde 'Empoderamiento
político', 'participación política' y 'Participación económica' son las brechas más pronunciadas.",
    caption = "Fuente: elaboración propia con datos del Banco Mundial. | @desuc_estudios",
    theme = theme(plot.background = element_rect(fill = "#293241", color = NA),
                  plot.subtitle = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
                  plot.caption = element_text(colour = "#edf2f4", family = "Trebuchet MS"),
                  plot.title = element_text(colour = "#edf2f4", face = "bold", family = "Impact")))


ggsave("output/19-global_change.png", width = 22, height = 20, units = "cm")

