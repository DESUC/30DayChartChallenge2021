library(tidyverse)
library(lubridate)
library(vdemdata)
library(sjmisc)
library(labelled)

df_democracy <- vdem

df_democracy_m <- df_democracy %>% 
  select(1:5, e_regionpol_6C, e_boix_regime) %>% 
  filter(year > 1900 & year < 2015) %>% 
  set_value_labels(e_regionpol_6C = c("Eastern Europe and Central Asia" = 1,
                                      "Latin America and the Caribbean" = 2,
                                      "The Middle East and North Africa" = 3,
                                      "Sub-Saharan Africa" = 4,
                                      "Western Europe and North America" = 5,
                                      "Asia and Pacific" = 6)) %>% 
  filter(e_regionpol_6C == 2) %>% 
  mutate(e_boix_regime = as.factor(e_boix_regime))
  
dem_palette <- c("#bf0603","#708d81")
names(dem_palette) <- levels(df_democracy_m$e_boix_regime)

df_democracy_m %>% 
  ggplot() + 
  geom_tile(aes(x = year, y = factor(country_name), fill = e_boix_regime)) +
  annotate(geom = "text", x = 2000, y = "Barbados", 
           label = 
"Los países sin información 
corresponden a países que no 
eran independientes.", 
           color = "#8d0801", family = "Trebuchet MS", size = 3) +
  labs(title = "Día 23: Tiles #30DayChartChallenge",
       subtitle = "¿Es este país democrático?",
       caption = "Fuente: Elaboración propia con datos de Boix et al. (2013) en V-Dem. | @desuc_estudios",
       y = NULL, x = NULL) +
  scale_fill_manual('', values = dem_palette, labels = c("No","Sí","")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#cfdbd5", color = NA),
        plot.background = element_rect(fill = "#cfdbd5", color = NA),
        plot.title = element_text(colour = "#8d0801", family = "Trebuchet MS", face = "bold"),
        plot.subtitle = element_text(colour = "#8d0801", family = "Trebuchet MS"),
        plot.caption = element_text(colour = "#8d0801", family = "Trebuchet MS"),
        axis.text = element_text(colour = "#8d0801", family = "Trebuchet MS"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'top', 
        legend.direction = 'horizontal',
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_rect(fill = "#cfdbd5", colour = "#cfdbd5"),
        legend.text = element_text(colour = "#8d0801", family = "Trebuchet MS"),
        legend.title = element_text(colour = "#8d0801", family = "Trebuchet MS"))

ggsave("output/23-tiles.png", width = 22, height = 18, units = "cm")
