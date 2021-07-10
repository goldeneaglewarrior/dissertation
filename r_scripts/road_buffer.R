## tanzania roads 

## libraries
library(sf) #necesarry for wdpar
#library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(wesanderson)
library(smoothr) #fill.hole
library(scales) # palette visualise show_col
library(tidyr)
library(ggridges)

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")
country_outline <-  map_data("world", regions = "Tanzania")

ggplot(bfr_20) +
  geom_sf(aes(fill = CONDITION)) +
  geom_polygon(country_outline, mapping = aes(x = long, y = lat, group = group), 
               fill = NA, colour = "black")

africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")


bfr_20 <- tan_roads %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


bfr_10 <- tan_roads %>% 
  st_buffer(dist = .1) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


agc_20 <- africa_07 %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T)




ggplot() +
  geom_tile(agc_20, mapping = aes(x=x, y=y, fill = mcnicol_AGC2007_1km)) +
  geom_polygon(country_outline, mapping = aes(x = long, y = lat, group = group), 
                fill = NA, colour = "black") +
  scale_fill_viridis_c(na.value = NA) +
  coord_quickmap() +
  theme_classic()



