# Tanzania rural areas


source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


## libraries
library(sf) #necesarry for wdpar
#library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth) # 1:50 M scale country outline
library(wesanderson) # colours
library(smoothr) #fill.hole
library(scales) # palette visualise show_col
library(tidyr)
#library(ggridges)
library(mapview) # interactive geometry viewing


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')

#pal_cava <- wes_palette("Cavalcanti1", 21, type = "continuous")
show_col(pal_cava)


## SPDF of country ----
country_SPDF <- ne_countries(scale = 50, #1:50 million scale
                             country = "United Republic of Tanzania")

country_outline <-  map_data("world", regions = "Tanzania")


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


## Calculating population centres ----


pop_1500 <- pop %>% 
  rasterToContour(nlevels = 40) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500")
length(pop_1500$geometry[[1]])


pop_4000 <- pop %>% 
  rasterToContour(nlevels = 40) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "4000")
length(pop_4000$geometry[[1]])
# an population level for larger, older city, with less overlap

mapview(pop_4000)
# 12 large cities with high population densities (11 mainland cities)
# Arusha, Moshi, Zanzibar, Dar es salaam, Morogoro, 
# Tanga, Dodoma, Mbeya, Kigoma, Kasulu, Mwanza, Musoma



pop_3000_sf <- pop_3000 %>% 
  st_buffer(dist = .8) %>% #buffer to exclude the charcoal buffer of other towns
  fill_holes(threshold = 1000000000000)# %>% 
  as_Spatial()
  

mapview(pop_3000_sf)

pop_3000_on <- pop %>% 
  crop(extent(pop_3000_sf)) %>% 
  mask(pop_3000_sf)

pop_1500_high <- pop_3000_on %>% 
  rasterToContour(nlevels = 50) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500")

length(pop_1500_high$geometry[[1]])

pop_3000_out <- pop %>% 
  mask(pop_3000_sf, inverse = T)


pop_1500_out <- pop_3000_out %>% 
  rasterToContour(nlevels = 40) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500") # check this is a level

low_1500_bfr_80 <- pop_1500_out %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000)# %>% 
  as_Spatial() # turns sf to spdf
mapview(low_1500_bfr_80)





area <- st_area(pop_300_sf0) # calculate area 
area2 <- st_area(low_1500_bfr_80)
area
area2
  

intersection <- st_intersection(pop_3000_sf$geometry, low_1500_bfr_80$geometry)

area_intersection <- st_area(intersection)
area_intersection
plot(intersection)

ggplot() +
  geom_sf(pop_3000_sf, mapping = aes(fill = level),
          fill = "#EBCC2A", col = NA, alpha = 0.5) +
  geom_sf(low_1500_bfr_80, mapping = aes(fill = level), 
          fill  = "#78B7C5", col = NA, alpha = 0.5) +
  geom_sf(pop_1500, mapping = aes(fill = level), col = "#EE3700") +
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  guides( fill = FALSE) +
  theme_classic()





length(pop_1500$geometry[[1]])
length(pop_1500_out$geometry[[1]])
length(pop_1500_high$geometry[[1]])
# There are 57 urban areas with 1500 people km-2
# 23 of them are within 80 km spheres of large cities, over 4000 people km-2
# 34 of them are outside of 80km sphere of larger cities


# reproject data to longitude/latitude for plotting
mwi_pa_data <- st_transform(mwi_pa_data, 4326)
# need to transform to make metric
# = st_transform(us_states, crs = 2163)







bfr_20 <- pop_1500 %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_50 <- pop_1500 %>% 
  st_buffer(dist = .5) %>%  # approximately 50km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_80 <- pop_1500 %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


# crop and df 
# 20km
agc_20 <- agc_stack %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T) %>% 
  na.omit()


# 50km crop 
agc_50 <- agc_stack %>% 
  crop(extent(bfr_50)) %>% 
  mask(bfr_50)  %>% 
  crop(bfr_20) %>% 
  mask(bfr_20, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  na.omit()



# 80km
agc_80_what <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) %>% 
  crop(bfr_50) %>% 
  mask(bfr_50, inverse = T)# %>% 
  as.data.frame(xy = T) %>% 
  na.omit()#

tanbox <- as(extent(29.3399975929,40.31659, -11.7209380022, -0.95), 
             'SpatialPolygons')

crs(tanbox) <- crs(tanbox)

# outside buffer 
agc_country <- agc_stack %>% 
  #crop(extent(bfr_80)) %>% 
  #mask(bfr_80, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  na.omit()


agc_country <- africa_07 %>% 
  crop(extent(tanbox)) %>% 
  mask(tanbox)



agc_country_1 <- agc_country %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80, inverse = T)

agc_country_bfr <- agc_country_1 %>% 
  crop(country_SPDF) %>% 
  mask(country_SPDF) %>% 
  as.data.frame(xy = T)

ggplot() +
  geom_tile(agc_country_bfr, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km)) +
  theme_classic() +
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black")


agc_80 <- africa_07 %>%
  crop(extent(tanbox)) %>% 
  mask(tanbox)
agc_80 <- agc_80 %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) #%>% 
agc_80_in <- agc_80 %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_50, inverse = T)# %>% 
  as.data.frame(xy = T) %>% 
  na.omit()#

  
agc_80_in_tan <- agc_80_in %>% 
  #crop(extend(country_SPDF)) %>% 
  mask(country_SPDF)
  
  
plot(agc_80_what$agc2007)
plot(agc_80_in_tan)


hlo <- africa_07 %>% 
  
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) %>% 
  crop(bfr_50) %>% 
  mask(bfr_50, inverse = T)

plot(agc_80_what$agc2007)
plot(agc_80_in)
