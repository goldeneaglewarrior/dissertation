# tan_bfr_road


library(sf) #necesarry for wdpar
library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth) # 1:50 M scale country outline
library(wesanderson) # colours
library(smoothr) #fill.hole
library(scales) # palette visualise show_col()
library(tidyr)
#library(ggridges)
library(mapview) 
library(purrr)
library(rgeos) #rastertopolygon()


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2007_1km_Aggregated.tif')


## roaaads

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")

road_20 <- tan_roads %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) #%>% 
  #as_Spatial()

# renaming agc_stack!
agc_stack <- mask(agc_stack, road_20)

mapview(agc_stack$agc2007)





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









poplow_overlap_mask <- st_difference(pop_low1500_bfr_sf, pop_hi1500_bfr_sf)
st_area(poplow_overlap_mask)
poplow_overlap_spdf <- poplow_overlap_mask %>% 
  as_Spatial()

whole_inside <- st_union(pop_low1500_bfr_sf, pop_hi1500_bfr_sf)



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
  as.data.frame(xy = T) %>% 
  mutate(buffer = 20)

# 50km low
agc_50_lo <- agc_stack %>% 
  mask(poplow_overlap_spdf) %>% 
  mask(bfr_50_lo)  %>% 
  mask(bfr_20_lo, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  mutate(buffer = 50)

# 80km low
agc_80_lo <- agc_stack %>% 
  mask(poplow_overlap_spdf) %>%
  mask(bfr_80_lo) %>% 
  mask(bfr_50_lo, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  mutate(buffer = 80)



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
  as.data.frame(xy = T) %>% 
  mutate(buffer = 20)

# 50km crop 
agc_50_hi <- agc_stack %>% 
  mask(bfr_50_hi)  %>% 
  mask(bfr_20_hi, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  mutate(buffer = 50)

# 80km
agc_80_hi <- agc_stack %>% 
  mask(bfr_80_hi) %>% 
  mask(bfr_50_hi, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  mutate(buffer = 80)


## outside 
# rural buffer 
agc_road_rural <- agc_stack %>% 
  mask(whole_inside, inverse = T) %>% 
  as.data.frame(xy = T) %>% 
  mutate(buffer = 100)

agc_road_rural <- agc_road_rural[rowSums(is.na(agc_road_rural[c(3:6)])) != 4, ]


mapview(agc_rural$agc2008)


agc_road_hi <- full_join(agc_20_hi, agc_50_hi)
agc_road_hi <- full_join(agc_road_hi, agc_80_hi)
agc_road_hi <- agc_road_hi[rowSums(is.na(agc_road_hi[c(3:6)])) != 4, ]


agc_road_lo <- full_join(agc_20_lo, agc_50_lo)
agc_road_lo <- full_join(agc_road_lo, agc_80_lo)
agc_road_lo <- agc_road_lo[rowSums(is.na(agc_road_lo[c(3:6)])) != 4, ]


write.csv(agc_road_hi,file="data/agc/agc_road_hi.csv", row.names = FALSE)
write.csv(agc_road_lo,file="data/agc/agc_road_lo.csv", row.names = FALSE)
write.csv(agc_road_rural,file="data/agc/agc_road_rural.csv", row.names = FALSE)

