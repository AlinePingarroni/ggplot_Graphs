# 06-Octubre-2024
# Uso de paletas de color en ggplot
# Prof:Aline Pingarroni
# Taller Introduccion al lenguaje R
#https://r-charts.com/es/paletas-colores/
#https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
#https://r-graph-gallery.com/color-palette-finder
#https://www.blakerobertmills.com/my-work/met-brewer

# Cargar librerías necesarias
library(ggplot2)
library(dplyr).   # Manejo de datos
library(viridis)  # Paquete viridis para paletas de colores

# Cargar la base de datos msleep y realizar las transformaciones
msleep <- msleep %>%
  mutate(logSleepawake = log(sleep_total / awake),
         logBodywt = log(bodywt),
         logBrainwt = log(brainwt)) %>%
  filter(!is.na(logSleepawake), !is.na(logBodywt), !is.na(logBrainwt))

# 1. Usar terrain.colors() en la estética de color
p1 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = logBrainwt)) +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = terrain.colors(20)) +  # Usar terrain.colors
  labs(title = "Paleta terrain.colors() aplicada a logBrainwt")
print(p1)


# 2. Usar heat.colors() en la estética de relleno
p2 <- ggplot(msleep, aes(x = vore, y = logSleepawake, fill = vore)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = heat.colors(length(unique(msleep$vore)))) +  # Usar heat.colors
  labs(title = "Paleta heat.colors() aplicada a vore")
print(p2)

# 3. Usar topo.colors() en la estética de color
p3 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = logBrainwt)) +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = topo.colors(10)) +  # Usar topo.colors
  labs(title = "Paleta topo.colors() aplicada a logBrainwt")
print(p3)

# 4. Usar cm.colors() en la estética de relleno
p4 <- ggplot(msleep, aes(x = vore, y = logSleepawake, fill = vore)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = cm.colors(length(unique(msleep$vore)))) +  # Usar cm.colors
  labs(title = "Paleta cm.colors() aplicada a vore")
print(p4)

# 5. Usar rainbow() en la estética de color
p5 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = logBrainwt)) +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = rainbow(10)) +  # Usar rainbow
  labs(title = "Paleta rainbow() aplicada a logBrainwt")
print(p5)

# 6. Crear una paleta manual y aplicarla
unique(msleep$vore)
manual_palette <- c("carni" = "darkred", "herbi" = "forestgreen", 
                    "omni" = "blue", "insecti" = "purple")

manual_palette <- c("darkred","forestgreen","blue","purple")

p6 <- ggplot(msleep, aes(x = vore, y = logSleepawake, fill = vore)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = manual_palette) +  # Aplicar paleta manual
  labs(title = "Paleta manual aplicada a vore")
print(p6)

############Viridis############
# 1. Usar viridis en una escala continua
p1 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = logSleepawake)) +
  geom_point(size = 3) +
  scale_colour_viridis(option = "D", direction = -1) +  # Paleta continua viridis
  labs(title = "Paleta viridis continua con logSleepawake")
print(p1)

# 2. Usar viridis en una escala discreta
p2 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = vore)) +
  geom_point(size = 3) +
  scale_colour_viridis(discrete = TRUE, option = "C") +  # Paleta discreta viridis
  labs(title = "Paleta viridis discreta con vore")
print(p2)

msleep_clean <- msleep %>% 
  filter(!is.na(vore))
unique(msleep_clean$vore)

# 3. Usar viridis para escala de relleno
p3 <- ggplot(msleep_clean, aes(x = vore, y = logSleepawake, fill = vore)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE, option = "A") +  # Paleta discreta viridis para fill
  labs(title = "Paleta viridis aplicada al relleno (fill)")
print(p3)

# 4. Usar viridis para escala binned
p4 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = cut_interval(logBrainwt, n = 5))) +
  geom_point(size = 3) +
  scale_colour_viridis(discrete = TRUE, option = "B", direction = -1) +  # Usar viridis con colores discretos
  labs(title = "Paleta viridis binned con logBrainwt (grupos)") +
  theme_minimal()
print(p4)

# 5. Usar viridis en combinación con facetas y leyenda personalizada
p5 <- ggplot(msleep, aes(x = logBodywt, y = logSleepawake, colour = logSleepawake)) +
  geom_point(size = 3) +
  facet_wrap(~ vore) +
  scale_colour_viridis(option = "C") +  # Paleta continua viridis
  theme(legend.position = "bottom") +  # Leyenda en la parte inferior
  labs(title = "Paleta viridis con facetas por vore")
print(p5)