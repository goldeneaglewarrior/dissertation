# mozambique buffer
# 7/7/21

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


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')


## SPDF of country ----
country_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "Mozambique")

country_outline <-  map_data("world", regions = "Mozambique")


## Crop to country ----
#agc
agc07 <- africa_07 %>% 
  crop(extent(country_SPDF)) %>% 
  mask(country_SPDF)

agc08 <- africa_08 %>% 
  crop(extent(country_SPDF)) %>% 
  mask(country_SPDF)

agc09 <- africa_09 %>% 
  crop(extent(country_SPDF)) %>% 
  mask(country_SPDF)

agc10 <- africa_10 %>% 
  crop(extent(country_SPDF)) %>% 
  mask(country_SPDF)



# population
pop <- world_pop %>% 
  crop(extent(country_SPDF)) %>% 
  mask(country_SPDF)


# Stack covariates
agc_stack <- stack(agc07,
                   agc08,
                   agc09,
                   agc10)

names(agc_stack) <- c("agc2007", 
                      "agc2008", 
                      "agc2009", 
                      "agc2010")


pop_1500 <- pop %>% 
  rasterToContour(nlevels = 20) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500")


# 20km
bfr_20 <- pop_1500 %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


# 50km
bfr_50 <- pop_1500 %>% 
  st_buffer(dist = .5) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

# 80km
bfr_50 <- pop_1500 %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


# crop and df 
# 20km
agc_20 <- agc_stack %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T)

# 50km crop 
agc_50 <- agc_stack %>% 
  crop(extent(bfr_50)) %>% 
  mask(bfr_50)  %>% 
  crop(bfr_20) %>% 
  mask(bfr_20, inverse = T) %>% 
  as.data.frame(xy = T)

# 80km
agc_80 <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) %>% 
  crop(bfr_50) %>% 
  mask(bfr_50, inverse = T) %>% 
  as.data.frame(xy = T)

# outside buffer
agc_outside <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80, inverse = T) %>% 
  as.data.frame(xy = T)

