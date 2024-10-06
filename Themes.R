# 06-Octubre-2024
# Uso de Temas en ggplot
# Prof:Aline Pingarroni
# Taller Introduccion al lenguaje R

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(ggridges)
library(tidyr)

#1 Cargar base de datos
data <- read.csv("data_domingo.csv", na.strings = c("NA"))

#2 Agregar columnas log-transformadas
data$logBody_mass_kg <- log(data$Body_mass_kg)
data$logHome_Range_km2 <- log(data$Home_Range_km2)

# Filtrar las observaciones sin NA en las variables que se usarán para la regresión
data_filtered <- data %>%
  filter(!is.na(logBody_mass_kg) & !is.na(logHome_Range_km2))

# Seleccionar aleatoriamente el 20% de los datos
set.seed(123)  # Fijar semilla para hacer la selección reproducible
data_sample <- data_filtered%>%
  sample_frac(0.01)

# Verificar el número de observaciones seleccionadas
nrow(data_sample)

# Crear el gráfico con la línea de regresión
p <- ggplot(data_sample, aes(x = logBody_mass_kg, y = logHome_Range_km2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue")


# Aplicar cada uno de los temas y guardar cada gráfico en un objeto
p + theme_gray()
p + theme_bw()
p + theme_linedraw()
p + theme_light()
p + theme_dark()
p + theme_minimal()
p + theme_classic()
p + theme_void()


#Personalizar
# 1. Cambiar el fondo del panel
p1 <- p + theme_minimal() + 
  theme(panel.background = element_rect(fill = "yellow", colour = "red", size = 0.5)) +
  labs(caption = "Paso 1: Fondo del panel ajustado")
print(p1)

# 2. Ajustar las líneas de la cuadrícula
p2 <- p1 + 
  theme(panel.grid.major = element_line(colour = "pink", size = 0.8),  # Cuadrículas principales
        panel.grid.minor = element_line(colour = "green", size = 0.5)) +  # Cuadrículas secundarias
  labs(caption = "Paso 2: Cuadrículas modificadas")
print(p2)

# 3. Modificar los títulos y textos de los ejes
p3 <- p2 + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 1),  # Título centrado y en negritas
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 1),  # Subtítulo en cursiva
        axis.title = element_text(size = 14),  # Tamaño de los títulos de los ejes
        axis.text = element_text(size = 12, colour = "blue")) +  # Etiquetas de los ejes en azul
  labs(caption = "Paso 3: Títulos y textos modificados")
print(p3)

# 4. Personalizar los ejes (líneas y ticks)
p4 <- p3 + 
  theme(axis.line = element_line(colour = "black", size = 2),  # Ejes en negro y más gruesos
        axis.ticks = element_line(colour = "red", size = 2)) +  # Marcas de los ejes ajustadas
  labs(caption = "Paso 4: Ejes y marcas modificados")
print(p4)


############Modificar el estilo del etiquetado
##########ETIQUETADO #########################
o<- ggplot(data=data, aes(x=Sex, fill=Locomotion)) +
  geom_bar()
o
# Modificar los títulos y etiquetas
# Crear el gráfico base de la regresión lineal con etiquetas modificadas
ggplot(data_sample, aes(x = logBody_mass_kg, y = logHome_Range_km2)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  
  # Modificar los títulos y etiquetas
  labs(
    title = "Relación entre Masa Corporal y Rango de Hogar",
    subtitle = "Muestra aleatoria del 20% de los datos",
    x = "Log de Masa Corporal (kg)",  # Etiqueta del eje X
    y = "Log del Rango de Hogar (km²)",  # Etiqueta del eje Y
    caption = "Fuente: HomeRange: A global database of mammalian home ranges"
  ) +
  
  # Modificar el estilo de las etiquetas y título
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "darkblue"),  # Título del gráfico
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray"),  # Subtítulo del gráfico
    axis.title.x = element_text(size = 12, face = "italic", color = "darkred"),  # Etiqueta del eje X
    axis.title.y = element_text(size = 12, face = "italic", color = "darkred"),  # Etiqueta del eje Y
    axis.text.x = element_text(size = 10, color = "black"),  # Texto del eje X
    axis.text.y = element_text(size = 10, color = "black"),  # Texto del eje Y
    legend.title = element_text(size = 12, face = "bold"),  # Título de la leyenda
    legend.text = element_text(size = 10)  # Texto de la leyenda
  )





