library(sjmisc)
library(tidyverse)
library(dplyr)
library(plyr)
library(tidytext)
library(extrafont)
library(harrypotter)
library(emojifont)

# Preparación de datos ----------------

#Información extraída de base de datos incluída en el paquete Harry Potter
#La fuente de Harry Potter se puede descargar en: https://www.fontspace.com/category/harry-potter

# Preparamos un dataframe que tenga las palabras de los libros.
palabras_hp <- list(
  piedra = philosophers_stone,
  camara = chamber_of_secrets,
  prisionero = prisoner_of_azkaban,
  caliz = goblet_of_fire,
  orden = order_of_the_phoenix,
  principe = half_blood_prince,
  reliquias = deathly_hallows
) %>%
  ldply(rbind) %>%
  mutate(libro = factor(seq_along(.id), labels = .id)) %>% 
  select(-.id) %>% 
  gather(key = 'chapter', value = 'text', -libro) %>%
  filter(!is.na(text)) %>% # 
  mutate(chapter = as.integer(chapter)) %>% 
  unnest_tokens(word, text, token = 'words')

# Filtramos las palabras que queremos buscar

palabras_hp <- palabras_hp %>% 
  filter(word %in% c('harry','potter','hermione','granger','ron','weasley','albus','dumbledore',
                  'voldemort','severus','snape')) %>%
  mutate(word = gsub('potter','harry',word)) %>% 
  mutate(word = gsub('albus','dumbledore',word)) %>% 
  mutate(word = gsub('granger','hermione',word)) %>% 
  mutate(word = gsub('weasley','ron',word)) %>% 
  mutate(word = gsub('severus','snape',word))

# Luego creamos la variable con emoticones. La pieza de ajedréz se agrega después (no se encuentra incporada en el paquete)

datos <- count(palabras_hp$word)
label <- c(emoji('older_man'),emoji('nerd_face'),emoji('books'),'\u{265F}','\U002697',emoji('skull'))
label2 <- c('','','','\u{265F}','','')

datos <- cbind(datos,label,label2)

# Gráfico ----------------

paleta_hp <- c('#AE2825','#9A1C1F','#86162A','#F7EE23','#E5BA30','#DDB03C')

datos %>% 
  mutate(x = fct_reorder(x,freq)) %>%
  ggplot(aes(x=x, y=freq, label = label, fill = x)) +
  geom_bar(stat="identity", alpha=.9, width=.6) +
  geom_text(family = "EmojiOne", size = 25, vjust = -.05) +
  geom_text(aes(label = label2), size = 25, vjust = -.3) +
  geom_text(aes(label = freq), size = 10, vjust = 1.2, hjust = .5, fontface = "bold") +
  scale_y_continuous(limits = c(0,20000)) + 
  scale_fill_manual(values = rev(paleta_hp)) + 
  theme_minimal(base_family = "Harry Potter") +
  theme(axis.text = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "grey95"),
        panel.grid.major = element_blank(),
        plot.title = element_text(size = rel(4.5)),
        axis.title = element_text(size = rel(2.5))) +
  labs(title = 'Menciones de personajes en saga Harry Potter',
       x = 'Personaje',
       y = 'Cantidad de Menciones')
  
ggsave('output/04-magical.png',
       width = 5,
       height = 3,
       scale = 3,
       units = 'cm')
