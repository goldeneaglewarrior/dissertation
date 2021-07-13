## Categorical measure

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
  dplyr::filter(is.na(subregion))



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



ggplot() +
  geom_tile(agc_80_df, mapping = aes(x=x,y=y, fill = agc2007), col = "dark grey") +
  geom_tile(agc_50_df, mapping = aes(x=x,y=y, fill = agc2007), col = "grey") +
  geom_tile(agc_20_df, mapping = aes(x=x,y=y, fill = agc2007), col = "light grey") +
  geom_sf(tan_1500, mapping = aes(col = level), size = 1, col = "#EE3700", fill = "#EE3700") +
  geom_polygon(data = mainland_tan, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "20km, 50km, 80km buffer around urban areas")
  




# 20km gains and loses
agc_20_df <- agc_20_df %>% 
  mutate(sustained_change = case_when(
    agc2007 > agc2008 & agc2008 > agc2009 & agc2009 > agc2010 ~ "sustained loses",
    agc2010 > agc2009 & agc2009 > agc2008 & agc2008 > agc2007 ~ "sustained gains")
  )


# 50km gains and loses
agc_50_df <- agc_50_df %>% 
  mutate(sustained_change = case_when(
    agc2007 > agc2008 & agc2008 > agc2009 & agc2009 > agc2010 ~ "sustained loses",
    agc2010 > agc2009 & agc2009 > agc2008 & agc2008 > agc2007 ~ "sustained gains")
  )


# 80km gains and loses
agc_80_df <- agc_80_df %>% 
  mutate(sustained_change = case_when(
    agc2007 > agc2008 & agc2008 > agc2009 & agc2009 > agc2010 ~ "sustained loses",
    agc2010 > agc2009 & agc2009 > agc2008 & agc2008 > agc2007 ~ "sustained gains")
  )



ggplot() +
  geom_bar(agc_20_df, mapping = aes(x = sustained_change)) +
  scale_x_discrete(na.translate = FALSE) +
  scale_colour_manual(values=c("#78B7C5", "#EBCC2A"))

ggplot() +
  geom_bar(agc_50_df, mapping = aes(x = sustained_change)) +
  scale_x_discrete(na.translate = FALSE) +
  scale_colour_manual(values=c("#78B7C5", "#EBCC2A"))

ggplot() +
  geom_bar(agc_80_df, mapping = aes(x = sustained_change)) +
  scale_x_discrete(na.translate = FALSE) +
  scale_colour_manual(values=c("#78B7C5", "#EBCC2A"))


agc_2bf <- agc_20_df %>% 
  dplyr::select(sustained_change) %>% 
  count(sustained_change, name = "20_bfr") %>% 
  na.omit()

agc_5bf <- agc_50_df %>% 
  dplyr::select(sustained_change) %>% 
  count(sustained_change, name = "50_bfr") %>% 
  na.omit()

agc_8bf <- agc_80_df %>% 
  dplyr::select(sustained_change) %>% 
  count(sustained_change, name = "80_bfr") %>% 
  na.omit()


bfr_chng <- left_join(agc_2bf, agc_5bf, by = "sustained_change")
bfr_chng <- left_join(bfr_chng, agc_8bf, by = "sustained_change")

bfr_chng_lng <- bfr_chng %>% 
  pivot_longer(!sustained_change, names_to = "bfr", values_to = "chng_count") %>% 
  tidyr::separate(bfr, c("bfr_dist", "bfr_name"), sep = "_", remove = FALSE) %>%  
  dplyr::select(-bfr_name) %>% 
  dplyr::select(-bfr)

bfr_long_new <- t(bfr_chng)

View(bfr_long_new)


colnames(bfr_long_new) <- c( "gains", "loses")
#bfr_long_new$bfr <- row.names(bfr_long_new)

bfr <- rownames(bfr_long_new)
rownames(bfr_long_new) <- NULL
data <- cbind(bfr,bfr_long_new)

data <- data[-1,]
data <- data %>% 
  tidyr::separate(bfr, c("bfr_dist", "bfr_name"), sep = "_", remove = FALSE) %>%  
  dplyr::select(-bfr_name) %>% 
  dplyr::select(-bfr)


data <- data.frame(data)
data$gains <- as.numeric(data$gains)
data$loses <- as.numeric(data$loses)
data$bfr <- as.factor(data$bfr)
str(data)





data %>% 
  ggplot() +
  geom_point(mapping = aes(x = bfr_dist, y = gains ), col = "dark green") +
  geom_point(mapping = aes(x = bfr_dist, y = loses ), col = "red") +
  labs(y = "Sustained trend (count of km2)",
       x = "Buffer distance from urban area (km)",
       title = "4 year of continuous data within buffer zones",
       subtitle = "Green = sustained gains, red = sustained loses") +
  theme_bw()

