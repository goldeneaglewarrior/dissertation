## Measure distances from raster to vector

## libraries
library(sf) #necesarry for wdpar
#library(wdpar)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(wesanderson)
library(scales) # palette visualise show_col


## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

pal <- wes_palette("Zissou1", 21, type = "continuous")
pal2 <- wes_palette("Zissou1", 5, type = "continuous")
darj <- wes_palette("Darjeeling1", 5, type = "continuous")
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



# Stack covariates
agc_stack <- stack(tan_agc07,
                    tan_agc08,
                    tan_agc09,
                    tan_agc10)

names(agc_stack) <- c("agc2007", 
                       "agc2008", 
                       "agc2009", 
                       "agc2010")

#tan_agc_df <- agc_stack %>% 
  as.data.frame(xy = T) %>% 
  na.omit()


#tan_agc_df <- tan_agc_df %>% 
  mutate(all_years = ifelse(is.na(agc2007) & is.na(agc2008) & is.na(agc2009) & is.na(agc2010),
                            "is na", "not na"))

tan_agc_df <- tan_agc_df %>% 
  mutate(all_years_x = (agc2007 + agc2008 + agc2009 + agc2010)/4)

tan_agc_df <- tan_agc_df %>% 
  mutate(only_growing = ifelse(agc2010 > agc2007, "Gaining Biomass", "Losing Biomass"))

tan_agc_df <- tan_agc_df %>% 
  mutate(sustained_change = case_when(
         agc2007 > agc2008 & agc2008 > agc2009 & agc2009 > agc2010 ~ "sustained loses",
         agc2010 > agc2009 & agc2009 > agc2008 & agc2008 > agc2007 ~ "sustained gains")
         )




tan_change <- tan_agc_df %>% 
  mutate(chng_07_08 = agc2008 - agc2007,
         chng_08_09 = agc2009 - agc2008,
         chng_09_10 = agc2010 - agc2009)


tan_4y <- tan_agc_df %>% 
  dplyr::select(x, y, all_years_x) %>% 
  na.omit()


length(tan_agc_df$all_years)

1479320 - 801234 # thats a good amount of data still!



ggplot() +
  geom_tile(tan_agc_df, mapping = aes(x=x, y=y, fill = all_years_x)) +
  scale_fill_gradientn(colours = pal, 
                       name = "bomass (mgC/ha/km)", 
                       na.value = NA, 
                       trans = "sqrt") +
  coord_quickmap() +
  theme_bw()


ggplot() +
  geom_tile(tan_agc_df, mapping = aes(x = x, y = y, fill = sustained_change)) +
  scale_fill_manual(values=c("#78B7C5", "#EBCC2A"), 
                    na.value = NA,
                    name = "") +
  labs(title = "4 year trend in Tanzania above ground biomass change",
       subtitle = "between 2007 and 2010") +
  geom_polygon(data = mainland_tan, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  coord_quickmap() +
  theme_bw()




ggplot() +
  geom_bar(tan_agc_df, mapping = aes(x = sustained_change)) +
  scale_x_discrete(na.translate = FALSE) +
  scale_colour_manual(values=c("#78B7C5", "#EBCC2A")) +
  labs(title = "4 year trend in Tanzania above ground biomass change",
       subtitle = "between 2007 and 2010",
       y = "count (km2)",
       x = "") +
  theme_classic()

  


ggplot() +
  geom_density(tan_change, mapping = aes(chng_07_08), col = "#EE3700", size = 1, alpha = 0.5) +
  geom_density(tan_change, mapping = aes(chng_08_09), col = "#EBCC2A", size = 1, alpha = 0.5) +
  geom_density(tan_change, mapping = aes(chng_09_10), col = "#78B7C5", size = 1, alpha = 0.5) +
  xlim(-20, 20) +
  labs(title = "red =",
       x = "Tanzanian biomass change per year") +
  theme_bw()


#
#
##
#
#
#

str(tan_4y$all_years_x)

tan_4y <- st_as_sf(x = tan_4y, 
                   coords = c("y", "x"),
                   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

tan_4y <- st_transform(tan_4y, 3055)


tanzania <- ne_countries(scale = 50, 
                         country = "United Republic of Tanzania", 
                         returnclass = "sf")


tanzania <- st_transform(tanzania, 3055)

#tan_4y <- st_cast(tan_4y, "MULTILINESTRING")

dist <- st_distance(tan_4y, tanzania)

dist_to <- st_nearest_feature(tanzania, tan_4y)


dist_tan4y <- data.frame(dist = as.vector(dist)/1000,
                         st_coordinates(tan_4y))



dist_to_tan4y <- data.frame(dist_to = as.vector(dist)/1000,
                            st_coordinates(tan_4y))




summary(dist_tan4y)
summary(dist_to_tan4y)


ggplot(dist_tan4y, aes(X, Y, fill = dist))+ #variables
  geom_point()+ #geometry
  #scale_fill_gradientn(colours = rev(col_dist))+ #colors for plotting the distance
  labs(fill = "Distance (km)")+ #legend name
  theme_void()+ #map theme
  coord_quickmap() +
  theme(legend.position = "bottom") #legend position



