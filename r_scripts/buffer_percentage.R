## Categorical measure percentage

## libraries
library(sf) #necesarry for wdpar
library(wdpar)
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

pal <- wes_palette("Zissou1", 21, type = "continuous")
show_col(pal)



## SPDF of tanzania ----
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")

tanza <-  map_data("world", regions = "Tanzania")
mainland_tan <- tanza %>% 
  filter(is.na(subregion))



## Crop to Tanzania ----
#agc
tan_agc07 <- africa_07 %>% 
  crop(extent(tan_SPDF)) %>% 
  mask(tan_SPDF)

tan_agc08 <- africa_08 %>% 
  crop(extent(tan_SPDF)) %>% 
  mask(tan_SPDF)

tan_agc09 <- africa_09 %>% 
  crop(extent(tan_SPDF)) %>% 
  mask(tan_SPDF)

tan_agc10 <- africa_10 %>% 
  crop(extent(tan_SPDF)) %>% 
  mask(tan_SPDF)



# population
tanpop <- world_pop %>% 
  crop(extent(tan_SPDF)) %>% 
  mask(tan_SPDF)


# Stack covariates
agc_stack <- stack(tan_agc07,
                   tan_agc08,
                   tan_agc09,
                   tan_agc10)

names(agc_stack) <- c("agc2007", 
                      "agc2008", 
                      "agc2009", 
                      "agc2010")

tan_agc_df <- agc_stack %>% 
  as.data.frame(xy = T) %>% 
  na.omit()



## contour


tan_pop_contour <- rasterToContour(tanpop, nlevels = 50) 
tan_pop_sf <- st_as_sf(tan_pop_contour)
tan_1500 <- filter(tan_pop_sf, level == "1500")


## 1500 ppkm buffers ----
# 20km
tan_bfr_20 <- st_buffer(tan_1500, dist = .2) # approximately 20km
plot(tan_bfr_20)
tan_smo_bfr_20 <- fill_holes(tan_bfr_20, threshold = 1000000000000)
plot(tan_smo_bfr_20)

# 50km
tan_bfr_50 <- st_buffer(tan_1500, dist = .5) # approximately 50km
plot(tan_bfr_50)
tan_smo_bfr_50 <- fill_holes(tan_bfr_50, threshold = 1000000000000)
plot(tan_smo_bfr_50)

# 80km
tan_bfr_80 <- st_buffer(tan_1500, dist = .8) # approximately 50km
plot(tan_bfr_80)
tan_smo_bfr_80 <- fill_holes(tan_bfr_80, threshold = 1000000000000)
plot(tan_smo_bfr_80)


##crop buffers?
bfr_20 <- sf:::as_Spatial(tan_smo_bfr_20)
bfr_50 <- sf:::as_Spatial(tan_smo_bfr_50)
bfr_80 <- sf:::as_Spatial(tan_smo_bfr_80)


# 20km
tan_agc_bfr_20 <- agc_stack %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20)

agc_20_df <- tan_agc_bfr_20 %>% 
  as.data.frame(xy = T) %>% 
  na.omit()


# 50km
tan_agc_bfr_50 <- agc_stack %>% 
  crop(extent(bfr_50)) %>% 
  mask(bfr_50)

agc_50_df <- tan_agc_bfr_50 %>% 
  as.data.frame(xy = T) %>% 
  na.omit()


# 80km
tan_agc_bfr_80 <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80)

agc_80_df <- tan_agc_bfr_80 %>% 
  as.data.frame(xy = T) %>% 
  na.omit()

