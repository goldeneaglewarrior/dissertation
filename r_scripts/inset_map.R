# Tanzania and africa map

library(ggplot2)
library(cowplot)
library(rnaturalearth)




world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- filter(world, continent == "Africa")

country_SF <- ne_countries(scale = 50, #1:50 million scale
                             country = "United Republic of Tanzania", 
                             returnclass = "sf")



africa_map <- ggplot() + 
  geom_sf(data = africa, fill = "white", col = "black", size = 0.2) + 
  geom_sf(data = country_SF, fill = "black", col = "black") +
  theme_void()

(tan_map <- ggplot() +
    geom_sf(data = country_SF, fill = NA, col = "black") +
    theme_classic())


ggdraw() +
  draw_plot(tan_map) +
  draw_plot(africa_map, x = 0.65, y = 0.65, width = 0.3, height = 0.3) +
  draw_plot_label(
    c("A", "B"),
    c(0.14, 0.67),
    c(0.9, 0.9),
    size = 12)

