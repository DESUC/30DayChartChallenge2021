setwd('C:/Users/Alex Leyton/Dropbox (ISUC)/ALEX/IG Desuc')

#Paquetes a ocupar

library(ggplot2movies)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

options(scipen=999)

#Importar la base desde el paquete ggplot2movies
#Filtramos peliculas con mayor cantidad de votos
#Sí lo hacemos desde 23814 hacia arriba nos quedamos con 250 peliculas (casos)


peliculas1 = na.omit(movies) %>%  filter (votes>=23814)

#Creamos la categoría género

peliculas1 <- peliculas1 %>% 
  mutate(Género = case_when(Action == 1 ~ "Acción",
                            Animation ==1 ~ "Animación",
                            Comedy ==1 ~ "Comedia",
                            Drama==1 ~ "Drama",
                            Documentary==1 ~ "Otro",
                            Romance==1 ~ "Romántica",
                            Short==1 ~ "Cortometraje"),
         budget2= budget/1000)



peliculas1$Género <- peliculas1$Género %>% replace_na("Otro")
peliculas1$Género <- as.factor(peliculas1$Género)
peliculas1$title <- as.factor(peliculas1$title)


#hacemos el plot 


CPOP<- peliculas1 %>%
  arrange(desc(length)) %>%
  mutate(title = factor(title)) %>%
  ggplot(aes(x=rating, y=budget2, size=length, colour=Género, fill=Género)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size_continuous(range = c(.1, 9)) +
  xlim(3.3, 9.5) + ylim(-5000, 220000) +
  scale_fill_viridis(discrete=TRUE, option="B") +
  labs(x = 'Rating IMDb', 
       y = 'Presupuesto (miles de dólares)',
       title = 'Pop Culture: Welcome to Hollywood',
       subtitle = 'Rating y presupuestos según género de la película',
       caption = 'El círculo aumenta según la duración de la película \nN: 250 películas con mayor cantidad de votos \nFuente: IMDb') +
  guides(size = F, colour = guide_legend(nrow = 1, size = 10000)) +
  theme_minimal() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal',
        legend.key.size = unit(.5, "cm"),
        legend.text = element_text(size = rel(1)))




ggsave('Culturepop_movies.png',
       width = 6,
       height = 7,
       scale = 3,
       units = 'cm')

getwd()
