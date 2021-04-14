library(sjmisc)
library(tidyverse)
library(extrafont)
library(readxl)

# Lectura de datos ----------------

#Información extraída de: https://www.statista.com/search/?q=historical+expectancy+life&qKat=newSearchFilter&sortMethod=idrelevance&isRegionPref=152&sortMethodMobile=idrelevance&statistics-group=1&statistics=1&forecasts=1&infos=1&topics=1&studies-reports=1&dossiers=1&groupA=1&xmo=1&surveys=1&toplists=1&groupB=1&branchreports=1&countryreports=1&groupC=1&expert-tools=1&cmo=1&mmo=1&co=1&tmo=1&amo=1&io=1&dmo=1&accuracy=and&isoregion=0&isocountrySearch=&category=0&interval=0&archive=1

datos <- read_excel("input/dia3_historical.xlsx")

# Gráfico ----------------

datos %>% 
  ggplot(aes(x=ano, y=esperanza, group=pais)) +
  geom_line(aes(color=pais), size = .6, alpha = .3) +
  geom_point(aes(color=pais), size = 1.5) +
  scale_color_brewer(palette="Dark2") + 
  scale_y_continuous(name = 'Esperanza de vida en años',
                     breaks=seq(0, 100, by = 10)) + 
  scale_x_continuous(name = 'Año',
                     breaks=seq(1880, 2020, by = 5)) + 
  theme_minimal(base_family = 'Roboto Condensed') +
  theme(axis.text.x = element_text(angle = 90, size = 5),
        plot.title = element_text(size = rel(1.5))) + 
  labs(title = 'Evolución de Esperanza de vida al nacer (1880-2020)',
       caption = 'World Population Prospects, 2019',
       color = 'País')

ggsave('output/03-historical.png',
       width = 5,
       height = 3,
       scale = 3,
       units = 'cm')
