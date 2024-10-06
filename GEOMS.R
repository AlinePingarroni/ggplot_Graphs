# 06-Octubre-2024
# Uso de Geometrias en ggplot
# Prof:Aline Pingarroni
# Taller Introduccion al lenguaje R

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(ggridges)
library(tidyr)

#1 Cargar base de datos
data <- read.csv("data_domingo.csv", na.strings = c("NA"))
dir()
summary(data)
dim(data)
names(data)

#2 Agregar columnas log-transformadas
data$logBody_mass_kg <- log(data$Body_mass_kg)
data$logHome_Range_km2 <- log(data$Home_Range_km2)

#3 Gráfico de densidad para una variable continua
ggplot(data=data, aes(x=logHome_Range_km2)) +
  geom_density()

#3.1 Histograma de una variable continua
ggplot(data=data, aes(x=logHome_Range_km2)) +
  geom_histogram()

# Guardar el gráfico en un objeto y añadir histogramas
E <- ggplot(data=data, aes(x=logHome_Range_km2))
E
E + geom_histogram()

#3.2 Gráfico de dispersion para dos variables continuas
ggplot(data=data, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_point()

#3.3 Gráfico de área con más variables
ggplot(data=data, aes(x=precip_annual, y=solar_rad_mean, fill=Sex)) +
  geom_area()

# Eliminar filas con NA en las columnas usadas en el gráfico
data_filtered <- drop_na(data, precip_annual, solar_rad_mean, Sex)
ggplot(data=data_filtered, aes(x=precip_annual, y=solar_rad_mean, fill=Sex)) +
  geom_area()

# Filtrar observaciones con datos no faltantes y seleccionar 50 observaciones aleatorias
data_filtered <- data %>%
  filter(!is.na(logBody_mass_kg) & !is.na(logHome_Range_km2)) %>%
  sample_n(50)

# Gráfico de etiquetas mostrando las especies de las 50 observaciones seleccionadas
ggplot(data=data_filtered, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_label(aes(label=Species))

# Gráfico de texto
ggplot(data=data_filtered, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_text(aes(label=Species))

# Gráfico de hexágonos
ggplot(data=data, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_hex()

# Gráfico de barras para una variable categórica
summary(data)
unique(data$Life_Stage)
data_filtered <- drop_na(data, Life_Stage)
ggplot(data=data_filtered, aes(x= Life_Stage)) +
  geom_bar()

# Gráfico de columna para una variable categórica y una continua
ggplot(data=data_filtered, aes(x=Sex, y=Body_mass_kg )) +
  geom_col()

# Boxplot para una variable categórica y una continua
# Filtrar observaciones con datos no faltantes y seleccionar 50 observaciones aleatorias
data_filtered <- data %>%
  filter(!is.na(logBody_mass_kg) & !is.na(logHome_Range_km2)) %>%
  sample_n(50)

ggplot(data=data_filtered, aes(x=Sex, y=logHome_Range_km2)) +
  geom_boxplot(fill="red", color="yellow") +
  geom_jitter()

# Violin plot
ggplot(data=data_filtered, aes(x=Sex, y=logHome_Range_km2)) +
  geom_violin(fill="red", color="yellow") +
  geom_jitter()

# Ajustes de posición para gráfico de barras
summary(data)
ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar()

ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar(position="dodge")

ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar(position="fill")

ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar(position="stack")

# Sistema de coordenadas: coord_flip() para invertir ejes
ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar() +
  coord_flip()

# coord_polar() para convertir el gráfico de barras en un gráfico de pastel
ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar() +
  coord_polar()

# Gráfico de puntos con límites ajustados
ggplot(data=data, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_point()

ggplot(data=data, aes(x=logBody_mass_kg, y=logHome_Range_km2)) +
  geom_point() +
  coord_cartesian(ylim=c(0, 1), xlim=c(-5, -2.5))

## Contar la frecuencia de cada sexo
sex_counts <- data %>%
  count(Sex) %>%
  mutate(percentage = n / sum(n) * 100) 

# Crear el gráfico de pastel basado en la variable 'Sex'
ggplot(sex_counts, aes(x = "", y = percentage, fill = Sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +  # Transformar en un gráfico de pastel
  theme_void() +  # Eliminar ejes y fondo
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))

##########ETIQUETADO #########################
o<- ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar()
o
# Modificar los títulos y etiquetas
o+labs(
  title = "Frecuencia del sexo",
  subtitle = "Mamiferos",
  x = "Sexo",  # Etiqueta del eje X
  y = "Frecuencia",  # Etiqueta del eje Y
  caption = "Fuente: HomeRange: A global database of mammalian home ranges"
) 

