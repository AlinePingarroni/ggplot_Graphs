# Cargar la librería ggplot2
library(ggplot2)

# Crear un data frame con los números de las formas
df_shapes <- data.frame(
  shape = 0:25,  # Números de las formas disponibles
  x = rep(1:6, each = 5)[1:26],  # Ajustar para 26 valores
  y = rep(1:5, times = 6)[1:26]  # Ajustar para 26 valores
)

# Graficar todas las formas
ggplot(df_shapes, aes(x = x, y = y)) +
  geom_point(aes(shape = shape), size = 5, fill = "blue") +  # Aplicar las formas
  scale_shape_identity() +  # Escala de formas
  geom_text(aes(label = shape), vjust = -1.5) +  # Añadir los números de las formas
  theme_void() +  # Quitar elementos del gráfico
  ggtitle("Listado de formas disponibles en ggplot2 (shapes)")  # Título del gráfico