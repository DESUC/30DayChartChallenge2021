
library(sjmisc)
library(sjlabelled)
library(tidyverse)
library(viridis)


# Apertura base -----------------------------------------------------------

#Datos del estudio de alfabetización financiera, presentación de datos en link siguiente.
#https://www.aafm.cl/comunicados/estudio-reafirma-bajo-nivel-de-alfabetizacion-financiera-en-estudiantes-universitarios-chilenos-y-la-necesidad-de-reforzar-esfuerzos-y-alcances/

df_multivariado <- readRDS("input/dia15_multivariado.RDS")

# Modelo ------------------------------------------------------------------

ahorro_logit_1 <- glm(ahorro ~ p5_6_1 + p5_6_2 + p5_6_3 + p5_6_4 + p5_6_5 + p5_6_6 + p14_10_1 + p14_10_2 + p14_10_3 + p14_10_4 + p14_10_5 + p14_10_6 + p14_10_7 + p14_10_8 + p14_10_9 + p14_10_10 + p1_rec.f + alfab_2.f + sexo.f + edad.f + area_estudio.f + sit_academica.f, data = df_multivariado, family = "binomial")
summary(ahorro_logit_1)

# Significancia del modelo y medidas de ajuste ----------------------------

#El modelo resulta significativo dado el valor p (es decir que el modelo es mejor que el modelo nulo, con sólo el intercepto)
#El AIC del modelo baja en las pruebas con más variables, pero tampoco lo hace de manera tan drástica.

with(ahorro_logit_1, null.deviance - deviance)
with(ahorro_logit_1, df.null - df.residual)
with(ahorro_logit_1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

## Colinealidad

#VIF que excede 5 o 10 indica problemas de colinealidad. Ninguna variable del modelo excede ese valor.

car::vif(ahorro_logit_1)

## Outliers

#Se observa en gráfico la distancia de Cook's.
plot(ahorro_logit_1, which = 4, id.n = 4) # se observan valores outliers

#Para identificar los valores outliers, se utiliza el paquete broom, calculando los residuos estandarizados y la distancia de cook's (calculada en el gráfico anterior)

model.data <- broom::augment(ahorro_logit_1) %>% 
  mutate(index = 1:n())
model.data %>% top_n(4, .cooksd)

#Ahora se grafican los residuos estandarizados, sobre nuestra variable de interés, sin embargo, se observa que los outliers no corresponden a "influential data", por lo que se mantienen y no se eliminan de la base. Si se quisieran remover los outliers, habría que usar el siguiente código:
  
#  model.data %>% 
#  filter(abs(.std.resid) > 4)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = ahorro), alpha = .5) +
  theme_bw()


# Identificación de ODDS --------------------------------------------------


exp(cbind(OR = coef(ahorro_logit_1), confint(ahorro_logit_1)))

exp(cbind("Odds ratio" = coef(ahorro_logit_1), 
       confint.default(ahorro_logit_1, level = 0.95)))


# Gráfico de ODDS ---------------------------------------------------------


modelo <- sjPlot::plot_model(ahorro_logit_1, 
                             type = "est",
                             order.terms = 1:(length(ahorro_logit_1$coefficients)-1),
                             group.terms = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6, 7, 7, 7, 7, 8, 8),
                             colors = "viridis",
                             show.values = TRUE,
                             value.offset =  0.3,
                             value.size = 3,
                             show.p = TRUE,
                             vline.color = "grey80",
                             title = 'Modelo logístico sobre ahorro habitual',
                             wrap.labels = 50)

#etiqueta para "group terms", variable group.
etiqueta <- c(`1` = "Fuente de \ningreso \ncat.ref= no", 
              `2` = "Conocimiento \ninstrumentos \ninversión \ncat.ref= no", 
              `3` = "Autoevaluación \nconocimiento \nfinanciero \ncat.ref= negativo", 
              `4` = "Alfabetización \nfinanciera \ncat.ref= no alfabetizado", 
              `5` = "Sexo \ncat.ref= mujer", 
              `6` = "Edad \ncat.ref= menos de 20", 
              `7` = "Area de \nestudio \ncat.ref= Adm y comercio", 
              `8` = "Situación \nacadémica \ncat.ref= primeros años")


# Personalización  -----------------------------------------------------------


modelo + 
  theme_light() +
  labs(caption = "Encuesta de Alfabetización y Comportamiento Financiero") +
  facet_grid(rows = vars(group), scales = 'free', space = 'free', labeller = as_labeller(etiqueta))+
  theme(strip.text.y = element_text(angle = 0),
        plot.caption = element_text(size = rel(.5)),
        plot.title.position = 'plot',
        legend.position = 'top',
        strip.background =element_rect(fill="grey90"),
        axis.text.x  = element_text(size = rel(0.8)),
        strip.text = element_text(colour = 'black'))


ggsave('output/15-multivariate.png',
       width = 21,
       height = 21,
       scale = 1,
       units = 'cm')
