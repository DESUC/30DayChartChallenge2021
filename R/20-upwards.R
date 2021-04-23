library(tidyverse)
library(lubridate)
library(gganimate)

# Temperatura promedio anual

df_temperatura <- read.delim("input/cr2_tasDaily_2018/cr2_tasDaily_2018.txt", 
                             sep = ",", 
                             header = T, 
                             na.strings = c("NA","-9999")) %>% 
  select(1, contains("330020"))

df_temperatura_f <- df_temperatura[15:43181,]

df_temperatura_f <- df_temperatura_f %>% 
  rename(date = 1,
         temperatura = 2) %>% 
  mutate(date = parse_date_time(date, orders = "ymd"),
         date = format(date, "%Y"),
         temperatura = as.numeric(temperatura)) %>% 
  group_by(date) %>% 
  summarise(temperatura = mean(temperatura, na.rm = T)) %>% 
  filter(temperatura != 'NaN') %>% 
  filter(date > 1970 & date < 2018)

str(df_temperatura_f)

temperatura <- df_temperatura_f %>% 
  mutate(date = as.numeric(date)) %>% 
  ggplot(aes(x = date, y = temperatura)) +
  geom_point(aes(group = seq_along(date)), color = "#22b455") +
  geom_line(aes(group = 1), color = "#22b455") +
  geom_text(aes(label = scales::comma(temperatura, accuracy = 0.1)), color = "#92e5a1") +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  transition_reveal(date) +
  labs(title = "Día 20: Upwards (gganimate) #30DayChartChallenge",
       subtitle = "Temperatura promedio anual (1970-2017)",
       x = "Año",
       y = "Temperatura (promedio anual)",
       caption = "Fuente: elaboración propia con datos compilados por el equipo de Datos y Cómputos del (CR)2.\n
       Estación Quinta Normal, código 330020\n
       @desuc_estudios") +
  theme_minimal() +
  theme(axis.text = element_text(colour = "#22b455", family = "Courier New"),
        axis.title = element_text(colour = "#22b455", family = "Courier New"),
        panel.background = element_rect(fill = "#020204", color = NA),
        plot.background = element_rect(fill = "#020204", color = NA),
        plot.title = element_text(colour = "#22b455", family = "Courier New", face = "bold"),
        plot.caption = element_text(colour = "#22b455", family = "Courier New"),
        plot.subtitle = element_text(colour = "#22b455", family = "Courier New"),
        panel.grid.major = element_line(colour = "#204829"), 
        panel.grid.minor = element_line(colour = "#204829"))

animate(temperatura, height = 500, width = 800)
anim_save("output/20-upwards.gif")

# Lluvia promedio anual ----

df_lluvia <- read.delim("input/cr2_prAmon_2019/cr2_prAmon_2019.txt", 
                        sep = ",", 
                        header = T, 
                        na.strings = c("NA","-9999")) %>% 
  select(1, contains("330020"))

df_lluvia_f <- df_lluvia[15:1454,]

df_lluvia_f <- df_lluvia_f %>% 
  rename(date = 1,
         lluvia = 2) %>% 
  mutate(date = parse_date_time(date, orders = "ym"),
         date = format(date, "%Y"),
         lluvia = as.numeric(lluvia)) %>% 
  group_by(date) %>% 
  summarise(lluvia = mean(lluvia, na.rm = T)) %>% 
  filter(lluvia != 'NaN') %>% 
  filter(date > 1970 & date < 2018)

lluvia <- df_lluvia_f %>% 
  mutate(date = as.numeric(date)) %>% 
  ggplot(aes(x = date, y = lluvia)) +
  geom_point(aes(group = seq_along(date)), color = "#22b455") +
  geom_line(aes(group = 1), color = "#22b455") +
  geom_text(aes(label = scales::comma(lluvia, accuracy = 0.1)), color = "#92e5a1") +
  scale_y_continuous(limits = c(0,60)) +
  scale_x_continuous(breaks = seq(1970,2020,5)) +
  labs(title = "Día 21: Downwards (gganimate) #30DayChartChallenge",
       subtitle = "Precipicación acumulada promedio anual (1970-2017)",
       x = "Año",
       y = "Datos observados de precipitación\nacumulada mensual (promedio anual)",
       caption = "Fuente: elaboración propia con datos compilados por el equipo de Datos y Cómputos del (CR)2.\n
       Estación Quinta Normal, código 330020\n
       @desuc_estudios") +
  transition_reveal(date) +
  theme_minimal() +
  theme(axis.text = element_text(colour = "#22b455", family = "Courier New"),
        axis.title = element_text(colour = "#22b455", family = "Courier New"),
        panel.background = element_rect(fill = "#020204", color = NA),
        plot.background = element_rect(fill = "#020204", color = NA),
        plot.title = element_text(colour = "#22b455", family = "Courier New", face = "bold"),
        plot.caption = element_text(colour = "#22b455", family = "Courier New"),
        plot.subtitle = element_text(colour = "#22b455", family = "Courier New"),
        panel.grid.major = element_line(colour = "#204829"), 
        panel.grid.minor = element_line(colour = "#204829"))

animate(lluvia, height = 500, width = 800)
anim_save("output/21-downwards.gif")
