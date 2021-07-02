## dar es salaam PA and pop density buffer


## libraries ----
library(sf) #necesarry for wdpar
library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth)
#library(terra) #faster better than raster
library(smoothr) #fill.hole
library(wesanderson) # colour alette


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')

lakes <- st_read("data/african_waterbodies/Africa_waterbody.shp")

## SPDF map and crop of tanzania ----
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")

## polygon outline of tanzania from ggplot2 -----
tanza <-  map_data("world", regions = "Tanzania")


pal <- wes_palette("Zissou1", 21, type = "continuous")
cava <- wes_palette("Cavalcanti1", 7,type = "continuous")
grndbud <- wes_palette("GrandBudapest1", 7, type = "continuous")


## Crop to Tanzania ----
#agc
tanagc07 <- crop(africa_07, extent(tan_SPDF))
tanagc07 <- mask(tanagc07, tan_SPDF)

# population
tanpop <- crop(world_pop, extent(tan_SPDF))
tanpop <- mask(tanpop, tan_SPDF)

tansf <- st_as_sf(tan_SPDF)

tanlakes <- st_intersection(lakes, tansf)

mainland_tan <- tanza %>% 
  filter(is.na(subregion))

tan_bfr_mnld <- st_intersection(tan_smo_bfr, tansf)



mainland_tansf <- st_intersection(tansf, mainland_sf)

## aggregates by mean, could choose median..
tanagc07 # 1479320 number of cells, 12 mb size
tanagc_agg2 <- aggregate(tanagc07, fact = 2) # 370140 cells, 3 mb size


tanpop_agg2 <- aggregate(tanpop, fact = 2)
plot(tanpop_agg2)


# dar es salaam rough coords
dar <- as(extent(38.5, 39.6, -7.2, -6.5), 'SpatialPolygons')
crs(dar) <-  "+proj=longlat +datum=WGS84 +no_defs" #coordinate reference system
daragc_07 <- crop(tanagc07, dar)
dar_pop <- crop(tanpop, dar)

plot(dar_pop)
plot(daragc_07)


# turn to dataframe
daragc_df <- as.data.frame(daragc_07, xy = T)

# not the smoothest way of getting 1500 population density but it works (here)...
dar_pop_contour <- rasterToContour(dar_pop, nlevels = 50) 
dar_pop_sf <- st_as_sf(dar_pop_contour)
plot(dar_pop_sf)

dar_pop_contour_20 <- rasterToContour(dar_pop, nlevels = 20)
plot(dar_pop_contour_20)

dar_pop_sf_20 <- st_as_sf(dar_pop_contour_20)
plot(dar_pop_sf_20)
dar_pop_sf_20$level


## plot of dar es salaam
ggplot() +
  geom_tile(daragc_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km)) +
  scale_fill_gradientn(colours = pal, name = "bomass (mgC/ha/km)", na.value = NA, trans = "sqrt") +
  geom_sf(dar_pop_sf_20, mapping = aes(col = level), size = 2, alpha = 0.5) +
  scale_colour_viridis_d(option = "inferno", alpha = 0.5, direction = 1) 
# add sea border, fill contour, fix colour order



# filter to 1500
dar_1500 <- filter(dar_pop_sf, level == "1500")
plot(dar_1500)
dar_1500


dar_bfr <- st_buffer(dar_1500, dist = .05) # approximately 50km
plot(dar_bfr)

dar_smo_bfr <- fill_holes(dar_bfr, threshold = 100000000)
plot(dar_smo_bfr)
dar_smo_bfr


ggplot() +
  geom_tile(daragc_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km)) +
  scale_fill_gradientn(colours = pal, name = "bomass (mgC/ha/km)", na.value = NA, trans = "sqrt") +
  geom_sf(dar_smo_bfr, mapping = aes(), size = 1, alpha = 0.5) +
  geom_polygon(data = tanza, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") 
  #scale_colour_viridis_d(option = "inferno", alpha = 0.5, direction = 1) 




## tanzania 

tan_pop_contour <- rasterToContour(tanpop, nlevels = 50) 
tan_pop_sf <- st_as_sf(tan_pop_contour)
tan_1500 <- filter(tan_pop_sf, level == "1500")

plot(tan_1500)

tan_bfr <- st_buffer(tan_1500, dist = .5) # approximately 50km
plot(tan_bfr)

tan_smo_bfr <- fill_holes(tan_bfr, threshold = 1000000000000)
plot(tan_smo_bfr)
tan_smo_bfr


tanagc07_df <- as.data.frame(tanagc07, xy = T)

ggplot() +
  geom_tile(tanagc07_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km)) +
  scale_fill_gradientn(colours = pal, name = "aboveground biomass \nkm2 average of mgC/ha", 
                       na.value = NA, trans = "sqrt") +
 # geom_sf(tza_terra_pa, mapping = aes(col = IUCN_CAT), size = 0.5, fill = "white", alpha = 0.3 , col = "black") +
 # scale_color_grey(name = "Protected Area\nIUCN Cat") +
  geom_sf(tan_bfr_mnld, mapping = aes(col = level), col = "black", size = 0.5, fill = NA) +
  geom_sf(tan_1500, mapping = aes(col = level), size = 1, col = "#BE00FE", fill = NA) +
  geom_sf(tanlakes, mapping =aes(fill = Shape_area), fill = "azure") +
  geom_polygon(data = mainland_tan, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  labs(title="Tanzania Aboveground Biomass map with urban area and buffer",
       subtitle ="1500 people per km2 urban population density (purple) with ~50km buffer ",
       x ="Latitude", y = "Longitude",
       caption = "McNicol et al. 2018, wdpar") +
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14, face = "plain"),             
        axis.title.y = element_text(size = 14, face = "plain"),             
        panel.grid.major.x = element_blank(),                                          
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),  
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
        plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
        plot.subtitle = element_text(size = 14, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 12, face = "italic"),
        panel.background = element_rect(fill = "white")
  )




ggplot() +
  geom_sf(tanlakes, mapping =aes(fill = Shape_area), fill = "white") +
  scale_colour_gradientn(colours = pal, name = "biomass (mgC/ha/km)", 
                       na.value = NA, trans = "sqrt") 





#
#
# Adding protected areas
#
#


tza_raw_pa_data <- wdpa_fetch("TZA")

tza_pa_data <- wdpa_clean(tza_raw_pa_data) # out of date april 2021

# reproject data to longitude/latitude for plotting
tza_pa_data <- st_transform(tza_pa_data, 4326)

tza_terra_pa <- tza_pa_data %>% 
  filter(MARINE == "terrestrial")


ggplot() +
  #geom_tile(tanagc07_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km )) +
  geom_sf(tza_terra_pa, mapping = aes(fill = as.factor(IUCN_CAT)), col = "black", alpha = 0.4) +
  geom_sf(tanlakes, mapping =aes(fill = Shape_area), fill = "white") +
  geom_sf(tan_bfr_mnld, mapping = aes(col = level), size = 1, fill = NA) +
  geom_polygon(data = mainland_tan, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black")
 # scale_fill_manual(values = cava) +
#  geom_sf(tanlakes, mapping =aes(fill = Shape_area), fill = "azure")
  

ggplot() +
  geom_tile(tanagc07_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km ))+
  geom_sf(tza_terra_pa, mapping = aes(col = IUCN_CAT),  alpha = 0.2, size = 1, fill = "white") +
  scale_colour_manual(values = cava)
  





## 
city <- c("Dar es Salaam", "Arusha", "Dodoma", "Mwanza", "Mbeya", "Tanga", "Morogoro")
lat <- c(-6.80, -3.37, -6.25, -2.52, -8.90, -5.07, -6.82)
long <- c(39.28, 36.68, 35.75, 32.90, 33.45, 39.10, 37.66)
city_df <- data.frame(city, lat, long)

