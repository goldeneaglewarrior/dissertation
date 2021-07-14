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
library(scales) # palette visualise show_col()
library(tidyr)
#library(ggridges)
library(mapview) # interactive geometry viewing


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2007_1km_Aggregated.tif')

pal <- wes_palette("Zissou1", 21, type = "continuous")
show_col(pal)


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


## Calculating 2 types of population centres ----
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

pop_4000_bfr_sf <- pop_4000 %>% 
  st_buffer(dist = 0.8) %>% #buffer to mostly exclude the charcoal buffer of other towns
  fill_holes(threshold = 1000000000000)

pop_4000_in <- pop %>%
  mask(pop_4000_bfr_sf)

pop_1500_hi <- pop_4000_in %>% 
  rasterToContour(nlevels = 50) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500") # if error check this is an actual level

pop_4000_out <- pop %>% 
  mask(pop_4000_bfr_sf, inverse = T)


pop_1500_lo <- pop_4000_out %>% 
  rasterToContour(nlevels = 10) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500") # if error check this is an actual level

pop_low1500_bfr_sf <- pop_1500_lo %>% 
  st_buffer(dist = .8) %>% 
  fill_holes(threshold = 1000000000000)# 

pop_hi1500_bfr_sf <- pop_1500_hi %>% 
  st_buffer(dist = .8) %>% 
  fill_holes(threshold = 1000000000000)# 

## investigate ----
st_area(pop_hi1500_bfr_sf) # calculate area for
st_area(pop_low1500_bfr_sf)

intersection <- st_intersection(pop_hi1500_bfr_sf$geometry, pop_low1500_bfr_sf$geometry)
st_area(intersection)



length(pop_1500$geometry[[1]])
length(pop_1500_lo$geometry[[1]])
length(pop_1500_hi$geometry[[1]])
# There are 53 urban contour areas with 1500 people km-2
# 27 of them are within 80 km spheres of large cities, over 4000 people km-2
# 26 of them are outside of 80km sphere of larger cities

#mapview(pop_low1500_bfr_sf)
#mapview(pop_hi1500_bfr_sf)

poplow_overlap_mask <- st_difference(pop_low1500_bfr_sf, pop_hi1500_bfr_sf)
st_area(poplow_overlap_mask)
poplow_overlap_spdf <- poplow_overlap_mask %>% 
  as_Spatial()

whole_inside <- st_union(pop_low1500_bfr_sf, pop_hi1500_bfr_sf)



# reproject data to longitude/latitude for plotting
mwi_pa_data <- st_transform(mwi_pa_data, 4326)
# need to transform to make metric
# = st_transform(us_states, crs = 2163)


# Doesn't work yet but important
#pop_1500_low <- st_transform(pop_1500_low, 4326)
# proj4string(pop_1500_low) <- CRS("+init=epsg:4326") 



## buffer urban area ----

# buffer for lower urban area

bfr_20_lo <- pop_1500_lo %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_50_lo <- pop_1500_lo %>% 
  st_buffer(dist = .5) %>%  # approximately 50km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_80_lo <- pop_1500_lo %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


# crop and df 
# 20km low
agc_20_lo <- agc_stack %>% 
  mask(poplow_overlap_spdf) %>% 
  mask(bfr_20_lo) %>% 
  as.data.frame(xy = T) 

# 50km low
agc_50_lo <- agc_stack %>% 
  mask(poplow_overlap_spdf) %>% 
  mask(bfr_50_lo)  %>% 
  mask(bfr_20_lo, inverse = T) %>% 
  as.data.frame(xy = T)

# 80km low
agc_80_lo <- agc_stack %>% 
  mask(poplow_overlap_spdf) %>%
  mask(bfr_80_lo) %>% 
  mask(bfr_50_lo, inverse = T) %>% 
  as.data.frame(xy = T)


## high urban cities 
bfr_20_hi <- pop_1500_hi %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_50_hi <- pop_1500_hi %>% 
  st_buffer(dist = .5) %>%  # approximately 50km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_80_hi <- pop_1500_hi %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


# crop and df 
# 20km
agc_20_hi <- agc_stack %>% 
  mask(bfr_20_hi) %>% 
  as.data.frame(xy = T)

# 50km crop 
agc_50_hi <- agc_stack %>% 
  mask(bfr_50_hi)  %>% 
  mask(bfr_20_hi, inverse = T) %>% 
  as.data.frame(xy = T)

# 80km
agc_80_hi <- agc_stack %>% 
  mask(bfr_80_hi) %>% 
  mask(bfr_50_hi, inverse = T) %>% 
  as.data.frame(xy = T)


## outside 
# rural buffer 
agc_rural <- agc_stack %>% 
  mask(whole_inside, inverse = T) %>% 
  as.data.frame(xy = T)


ggplot() +
  geom_tile(agc_rural, mapping = aes(x=x,y=y, fill= agc2007, col = "#78B7C5")) +
  geom_tile(agc_20_lo,mapping = aes(x=x,y=y,fill = agc2007), col = "#EB5500") +
  geom_tile(agc_50_lo,mapping = aes(x=x,y=y,fill = agc2007), col = "#E4BA10") +
  geom_tile(agc_80_lo,mapping = aes(x=x,y=y,fill = agc2007), col = "#BDC367" ) +
  geom_tile(agc_20_hi,mapping = aes(x=x,y=y,fill = agc2007), col = "#F21A00") + #dark red
  geom_tile(agc_50_hi,mapping = aes(x=x,y=y,fill = agc2007), col = "#E1AF00") +
  geom_tile(agc_80_hi,mapping = aes(x=x,y=y,fill = agc2007), col = "#EBCC2A" ) + #yellow
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  coord_quickmap() +
  theme(legend.position = "none") +
  guides(color = FALSE, fill = FALSE) +
  theme_classic()



#saving files to analyse in other scripts
save(agc_20_hi,file="data/buffers/agc_20_hi")
save(agc_50_hi,file="data/buffers/agc_50_hi")
save(agc_80_hi,file="data/buffers/agc_80_hi")

save(agc_20_lo,file="data/buffers/agc_20_lo")
save(agc_50_lo,file="data/buffers/agc_50_lo")
save(agc_80_lo,file="data/buffers/agc_80_lo")

save(agc_rural,file="data/buffers/agc_rural")


save(bfr_20_hi, file = "data/buffers/bfr_20_hi")
save(bfr_50_hi, file = "data/buffers/bfr_50_hi")
save(bfr_80_hi, file = "data/buffers/bfr_80_hi")

save(bfr_20_lo, file = "data/buffers/bfr_20_lo")
save(bfr_50_lo, file = "data/buffers/bfr_50_lo")
save(bfr_80_lo, file = "data/buffers/bfr_80_lo")



