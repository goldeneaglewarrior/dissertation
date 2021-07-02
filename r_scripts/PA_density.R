# Intro
# biomass and pop den inside and out of PA
# 1/7/21


## libraries ----
library(sf) #necesarry for wdpar
library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(terra) #faster better than raster
library(smoothr) #fill.hole
library(wesanderson) # colour alette
#library(rnaturalearthdata)


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')

## SPDF map and crop of tanzania ----
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")

pal <- wes_palette("Zissou1", 5, type = "discrete")


## Crop to Tanzania ----
#agc
tanagc07 <- crop(africa_07, extent(tan_SPDF))
tanagc07 <- mask(tanagc07, tan_SPDF)



tanpop <- crop(world_pop, extent(tan_SPDF))
tanpop <- mask(tanpop, tan_SPDF)


## aggregate by mean, could choose median..
tanagc07 # 1479320 number of cells, 12 mb size
tanagc_agg2 <- aggregate(tanagc07, fact = 2) # 370140 cells, 3 mb size

tanpop
tanpop_agg2 <- aggregate(tanpop, fact = 2)
plot(tanpop_agg2)

## masking for buffer
values(tanpop_agg2)[values(tanpop_agg2) < 1500] = 0
values(tanpop_agg2)[values(tanpop_agg2) >= 1500] = 1

x <- rasterToContour(tanagc_agg2)
class(x)
plot(x, add = T)



kilipop_round <- round(kili_pop, .05)
kilipop_round
kili_pop


k <- as(extent(36.4, 37.7, -3.9, -2.6), 'SpatialPolygons')
crs(k) <-  "+proj=longlat +datum=WGS84 +no_defs" #coordinate reference system
kili_07 <- crop(africa_07, k)
kili_pop <- crop(tanpop, k)

kili_agc_df <- as.data.frame(kili_07, xy = T)

plot(kili_pop)

kx <- rasterToContour(kilipop_round)
plot(kx)

kilipoly <- rasterToPolygons(kilipop_agg2)
plot(kilipoly)


kilipoly <- terra::as.polygons(kilipop_agg2)


filledContour(kili_pop, y = .1)


contour(kili_pop, maxpixels=100000)


kilipop_agg2 <- aggregate(kili_pop, fact = 2)
plot(kilipop_agg2)
values(kilipop_agg2)[values(kilipop_agg2) < 1500] = NA
values(kilipop_agg2)[values(kilipop_agg2) >= 1500] = 1

plot(kilipop_agg2)

kili_bfr20<- raster::buffer(kilipop_agg2, width = 20000) # 20km buffer
plot(kili_bfr20, col = "red")
kili_bfr20



kilipop_agg_new <- aggregate(kili_pop, fact = 3)
plot(kilipop_agg_new)
kilipoly_new <- rasterToContour(kili_pop)
plot(kilipoly_new)

kili_sf <- st_as_sf(kilipoly_new)

plot(kili_sf)



ggplot() +
  geom_tile(kili_agc_df, mapping = aes(x=x,y=y, fill = mcnicol_AGC2007_1km)) +
  scale_fill_viridis_c(name = "bomass (mgC/ha/km)", na.value = NA, trans = "sqrt") +
  geom_sf(kili_sf, mapping = aes(col = level), size = 2, alpha = 0.5) +
  scale_colour_viridis_d(option = "cividis", alpha = 0.5) 

  
  


kili_1000 <- filter(kili_sf, level == "1000")
kili_1000
plot(kili_1000)

kili_sf_bfr <- st_buffer(kili_1000, dist = .05)
plot(kili_sf_bfr)

smoothr_bfr <- fill_holes(kili_sf_bfr, threshold = 100000000000)
plot(smoothr_bfr)

kili_pop_df <- as.data.frame(kili_pop, xy = T)


## ko

ggplot() +
  geom_tile(kili_pop_df, mapping = aes(x = x, y = y, fill = ppp_2010_1km_Aggregated)) +
  scale_fill_viridis_c(name = "pop density (ppkm)", na.value = NA, trans = "sqrt") + # trans = log
  geom_sf(data = smoothr_bfr, fill = "white", col = "white", size = 1.5, alpha = 0.1) +
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
        panel.background = element_rect(fill = "light grey")
  )





plot(kili_sf)

values(kilipop_agg_new)[values(kilipop_agg_new) < 1500] = NA
values(kilipop_agg_new)[values(kilipop_agg_new) >= 1500] = 1
plot(kilipop_agg_new)

kili_bfr20_new <- raster::buffer(kilipop_agg_new, width = 20000) # 20km buffer
plot(kili_bfr20_new)


kili_bfr_ply <- rasterToContour(kili_bfr20)
plot(kili_bfr_ply)


kilibfr_df <- as.data.frame(kili_bfr20, xy = T)

ggplot() +
  geom_tile(data = kilibfr_df, aes(x=x, y=y, fill = layer)) +
  coord_quickmap()

kilipop_agg2
