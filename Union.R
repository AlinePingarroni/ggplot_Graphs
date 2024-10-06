# 06-Octubre-2024
# Unir graficos en una figura
# Prof:Aline Pingarroni
# Taller Introduccion al lenguaje R

# Instalar Paqueterias
#install.packages("patchwork")
library(patchwork)
library(ggplot2)

# Cargar la base de datos msleep y realizar las transformaciones
msleep <- msleep %>% 
  filter(!is.na(vore))

# Crear gráficos usando la base msleep
# Graficar relación entre masa corporal y tiempo total de sueño
p1 <- ggplot(msleep, aes(bodywt, sleep_total)) + 
  geom_point(aes(fill = vore), shape = 25, alpha=0.7) +  # Usar shape = 17 fuera de aes()
  scale_x_log10() + 
  ggtitle("A) Masa corporal vs Tiempo total de sueño")

p1  # Mostrar gráfico

# Graficar relación entre masa corporal y tiempo de sueño REM
p2 <- ggplot(msleep, aes(sleep_rem)) + 
  geom_histogram(fill="red",alpha=0.5,color="black") + 
  scale_x_log10() + 
  ggtitle("B) Tiempo de sueño REM")
p2 

# Graficar relación entre masa corporal y tiempo de despierto
p3 <- ggplot(msleep, aes(vore,log(bodywt))) + 
  geom_boxplot(aes(fill=vore,alpha = 0.7)) + 
  geom_jitter(alpha = 0.7)+
  ggtitle("C) Habito alimenticio y tamaño corporal")
p3

# 1. Unir gráficos horizontalmente
combined_horizontal <- p1 + p2
combined_horizontal

# 2. Unir gráficos verticalmente
combined_vertical <- p1 / p2
combined_vertical 

# 3. Crear una combinación más compleja con varios gráficos
complex_combination <- (p1 | p2) / p3
complex_combination 

# 4. Controlar la disposición con `plot_layout()`
# Ajustar el número de columnas y el ancho relativo de los gráficos
layout_combination <- (p1 + p2 + p3) + 
  plot_layout(ncol = 2, widths = c(2, 1))

# 5. Usar `wrap_plots()` para organizar múltiples gráficos en una cuadrícula
wrap_combination <- wrap_plots(p1, p2, p3, ncol = 2)
wrap_combination 

# 6. Agregar anotaciones y títulos a toda la combinación
annotated_combination <- (p1 + p2) + 
  plot_annotation(
    title = "Relación entre Masa Corporal y Diferentes Parámetros de Sueño en Mamíferos",
    subtitle = "Datos de msleep",
    caption = "Fuente: msleep dataset"
  )
annotated_combination

# 7. Ajustar el diseño personalizado usando `area()` y un layout específico
design <- c(
  area(1, 1, 1, 2),  # El primer gráfico ocupa toda la primera fila
  area(2, 1, 2, 1),  # El segundo gráfico en la esquina inferior izquierda
  area(2, 2, 2, 2)   # El tercer gráfico en la esquina inferior derecha
)

#area(1, 1, 1, 2):
#•	1, 1: El gráfico comienza en la fila 1 y en la columna 1.
#•	1, 2: El gráfico termina en la fila 1 y se extiende hasta la columna 2.

custom_layout_combination <- wrap_plots(p1, p2, p3, design = design)

# 8. Aplicar un tema común a todos los gráficos usando el operador `+`
theme_combination <- (p1 + p2 + p3) + theme_minimal()

# 9. Guardar una combinación de gráficos usando `ggsave()`
# Combinar gráficos en una disposición simple y guardarlos
combined_for_saving <- p1 + p2 + plot_layout(ncol = 1)
ggsave("combined_plot_msleep.png", plot = combined_for_saving, width = 8, height = 6)
