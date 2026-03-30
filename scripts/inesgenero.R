# Aanalisis AJT genero

# analisis CI FID UPLA
# cargar paquetes

library(readr)
library(janitor)

# get working directory
getwd()

# set working directory 
setwd ('C:/Users/Jennifer/Desktop/2025/INESGENERO/data') # run with Ctrl + Enter

# get data
data <- read.csv("data.csv", sep = ";") #esto es muy importante

data %>%
  summarise(mean = mean(respuestaes, na.rm = T), sd = sd(respuestaes, na.rm = T))

data %>%
  group_by(condicion, estructura) %>%
  summarise(mean = mean(respuestaes, na.rm = T), sd = sd(respuestaes, na.rm = T))


# Crear tabla cruzada de frecuencias
library(dplyr)

library(ggplot2)
data %>%
  ggplot(aes(x = condicion, y = respuestaes, color = estructura)) + 
  facet_grid(. ~ estructura) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'condiciongram', y = 'Proporcion de respuestas esperadas', caption = '', 
       title = 'Figura 1: Proporcion de respuestas esperadas condicion y estructura') 

sumas <- data %>%
  group_by(condicion, estructura) %>%
  summarise(
    n = n(),
    prop = mean(respuestaes, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(sumas, aes(x = condicion, y = prop, color = estructura, group = estructura)) +
  facet_grid(. ~ estructura) +
  geom_hline(yintercept = 0.5, linewidth = 1) +
  geom_point(position = position_dodge(width = 0.5)) +
  # Si quieres barras de error simples (±SE):
  # geom_errorbar(aes(ymin = pmax(0, prop - sqrt(prop*(1-prop)/n)),
  #                   ymax = pmin(1, prop + sqrt(prop*(1-prop)/n))),
  #               width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_brewer(palette = "Set1", name = "Estructura") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Condición", y = "Proporción de respuestas esperadas",
       title = "Proporción de respuestas correctas por condición y estructura") +
  theme_minimal(base_size = 12)

library(dplyr)

por_participante <- data %>%
  group_by(participante, condicion, estructura) %>%
  summarise(
    prop_esperadas = mean(respuestaes, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

por_participante

library(ggplot2)


ggplot(por_participante, aes(x = condicion, y = prop_esperadas, 
                             color = estructura, group = participante)) +
  geom_point() +
  geom_line(alpha = 0.5) +
  facet_wrap(~ estructura) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Condición (gramaticalidad)",
    y = "Proporción de respuestas esperadas",
    title = "Respuestas individuales por estructura y condición"
  ) +
  theme_minimal()

comparaciones <- por_participante %>%
  group_by(estructura, condicion) %>%
  summarise(media = mean(prop_esperadas), sd = sd(prop_esperadas), .groups = "drop")

comparaciones

ggplot(comparaciones, aes(x = condicion, y = media, fill = estructura)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd), width = 0.2,
                position = position_dodge(0.9)) +
  coord_cartesian(ylim = c(0,1)) +
  labs(x = "Condición", y = "Proporción promedio de respuestas esperadas",
       title = "Tendencias generales por estructura y condición") +
  theme_minimal()


library(dplyr)


data %>%
  group_by(tipobil) %>%
  summarise(
    n = n_distinct(participante),
    edad_promedio = mean(edad, na.rm = TRUE),
    edad_min = min(edad, na.rm = TRUE),
    edad_max = max(edad, na.rm = TRUE)
  )

ggplot(data, aes(x = condicion, y = respuestaes, color = estructura)) +
  facet_grid(tipobil ~ estructura) +   # compara en filas por grupo bilingüe
  stat_summary(fun = mean, geom = "pointrange",
               position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  coord_cartesian(ylim = c(0,1)) +
  labs(x = "Condición", y = "Proporción de respuestas esperadas",
       title = "Proporción de respuestas esperadas por tipo de bilingüe") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal(base_size = 12)

library(dplyr)
library(ggplot2)

# Asegura los tipos correctos
data <- data %>%
  mutate(
    tipobil = as.factor(tipobil),
    condicion = as.factor(condicion),
    estructura = as.factor(estructura),
    respuestaes = as.numeric(respuestaes)
  )

# Calcular proporciones promedio
sumas <- data %>%
  group_by(tipobil, condicion, estructura) %>%
  summarise(
    prop = mean(respuestaes, na.rm = TRUE),
    n = n(),
    se = sqrt(prop * (1 - prop) / n),   # error estándar
    .groups = "drop"
  )

# Graficar
ggplot(sumas, aes(x = condicion, y = prop, fill = estructura)) +
  geom_col(position = position_dodge(0.8)) +
  geom_errorbar(aes(ymin = prop - se, ymax = prop + se),
                width = 0.2,
                position = position_dodge(0.8)) +
  facet_wrap(~ tipobil) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_fill_brewer(palette = "Set1", name = "Estructura") +
  labs(
    x = "Condición (gramaticalidad)",
    y = "Proporción de respuestas esperadas",
    title = "Proporción de respuestas esperadas por tipo de bilingüe",
    caption = "Errores de concordancia en estructuras nominales y pronominales"
  ) +
  theme_minimal(base_size = 13)

data %>%
  ggplot(aes(x = condicion, y = respuestaes, color = estructura)) + 
  facet_grid(. ~ tipobil) + 
  geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
  stat_summary(fun = mean, geom = 'pointrange', size = 1, 
               position = position_dodge(width = 0.5)) + 
  scale_color_brewer(palette = "Set1", name = "") +
  #theme(legend.position = "none") +
  ylim(c(0,1)) +
  labs(x = 'condicion', y = 'Proporcion de respuestas esperadas', caption = '')
       
       
       data %>%
         ggplot(aes(x = condicion, y = respuestaes, color = estructura)) + 
         facet_grid(. ~ tipobil) + 
         geom_hline(yintercept = 0.5, color = 'white', size = 2) + 
         stat_summary(fun = mean, geom = 'pointrange', size = 1, 
                      position = position_dodge(width = 0.5)) + 
         scale_color_brewer(palette = "Set1", name = "") +
         ylim(c(0,1)) +
         labs(
           x = 'Condición',
           y = 'Proporción de respuestas esperadas',
           caption = ''
         )
       



