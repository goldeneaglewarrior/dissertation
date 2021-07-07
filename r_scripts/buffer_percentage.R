## Categorical measure percentage

## libraries
library(sf) #necesarry for wdpar
library(wdpar)
#library(dplyr)
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
pal5 <- wes_palette("Zissou1", 5, type = "continuous")
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
  as.data.frame(xy = T) #%>% 
 # na.omit()

tan_agc_df_na <- tan_agc_df[rowSums(is.na(tan_agc_df[c(3:6)])) != 4, ]


## contour


tan_pop_contour <- rasterToContour(tanpop, nlevels = 50) 
tan_pop_sf <- st_as_sf(tan_pop_contour)
tan_1500 <- dplyr::filter(tan_pop_sf, level == "1500")


## 1500 ppkm buffers ----
# 20km
tan_bfr_20 <- st_buffer(tan_1500, dist = .2) # approximately 20km
tan_smo_bfr_20 <- fill_holes(tan_bfr_20, threshold = 1000000000000)

# 50km
tan_bfr_50 <- st_buffer(tan_1500, dist = .5) # approximately 50km
tan_smo_bfr_50 <- fill_holes(tan_bfr_50, threshold = 1000000000000)

# 80km
tan_bfr_80 <- st_buffer(tan_1500, dist = .8) # approximately 50km
tan_smo_bfr_80 <- fill_holes(tan_bfr_80, threshold = 1000000000000)




##crop buffers?
bfr_20 <- sf:::as_Spatial(tan_smo_bfr_20)
bfr_50 <- sf:::as_Spatial(tan_smo_bfr_50)
bfr_80 <- sf:::as_Spatial(tan_smo_bfr_80)


# crop and df (should pipe all these)
# 20km
tan_agc_bfr_20 <- agc_stack %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) 

agc_20_df <- tan_agc_bfr_20 %>% 
  as.data.frame(xy = T)# %>% 
 # na.omit()


# 50km crop 
tan_agc_bfr_50 <- agc_stack %>% 
  crop(extent(bfr_50)) %>% 
  mask(bfr_50)  %>% 
  crop(bfr_20) %>% 
  mask(bfr_20, inverse = T)

agc_50_df <- tan_agc_bfr_50 %>% 
  as.data.frame(xy = T) #%>% 
 # na.omit()

# 80km
tan_agc_bfr_80 <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) %>% 
  crop(bfr_50) %>% 
  mask(bfr_50, inverse = T)

agc_80_df <- tan_agc_bfr_80 %>% 
  as.data.frame(xy = T)# %>% 
 # na.omit()


# outside buffer
tan_agc_bfr_outside <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80, inverse = T) 

agc_outside_df <- tan_agc_bfr_outside %>% 
  as.data.frame(xy = T)# %>% 
  #na.omit()



# buffer column
agc_20_df$buffer <- (20)
agc_50_df$buffer <- (50)
agc_80_df$buffer <- (80)
agc_outside_df$buffer <- (100)


tan_total <- dplyr::full_join(agc_20_df, agc_50_df)
tan_total <- dplyr::full_join(tan_total, agc_80_df)
tan_total <- dplyr::full_join(tan_total, agc_outside_df)

tan_total$buffer <- as.factor(tan_total$buffer)
levels(tan_total$buffer)

tan_total_na <- tan_total[rowSums(is.na(tan_total[c(3:6)])) != 4, ]


summary(tan_total$agc2007)
str(tan_total_long)


tan_total_long <- tan_total_na %>% 
  dplyr::select(!x & !y) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "20" )


tan_total_long$year <- as.numeric(tan_total_long$year)



#tan_total %>% 
#  mutate(buffer = recode(buffer, 100 = "Beyond buffer"))

1.479*4

tan_total$buffer <- as.factor(tan_total$buffer)

tan_total %>% 
  #dplyr::select(agc2007, buffer) %>% 
  ggplot() +
  geom_violin(mapping = aes(buffer, agc2007), col = "black") +
  geom_violin(mapping = aes(buffer, agc2008), alpha = 0.2, col = "dark grey") +
  geom_violin(mapping = aes(buffer, agc2009), alpha = 0.2, col = "grey") +
  geom_violin(mapping = aes(buffer, agc2010), alpha = 0.2, col = "light grey") 
  

summary(tan_total$agc2007)
count(tan_agc_df)
str(tan_agc_df)


tan_total_long <- tan_total %>% 
  dplyr::select(!x & !y) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "20" )

levels(tan_total_long$buffer)

tan_total_long$year <- as.numeric(tan_total_long$year)


tan_total_na <- tan_total_na %>% 
  dplyr::mutate(perc_08 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  dplyr::mutate(perc_09 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  dplyr::mutate(perc_10 = ((agc2010 - agc2009) / agc2009)* 100)

tan_perc <- tan_total_na %>% 
  dplyr::select(7:10) %>% 
  dplyr::mutate(perc_07 = 0) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" )

tan_perc$year <- as.numeric(tan_perc$year)


tan_perc <- tan_perc %>% 
  dplyr::filter(perc_change <= 100, perc_change >= -100)

tan_perc %>%
  #dplyr::filter(year == 10) %>% 
  ggplot(aes(x = factor(year), y = perc_change)) +
  geom_boxplot() +
  facet_wrap(~buffer)



tan_perc %>% 
  dplyr::filter(perc_change > 0) %>% 
  ggplot(aes(x = factor(year), y = perc_change)) +
  geom_violin() +
  geom_smooth(method = "lm") +
  facet_wrap(~buffer)

tan_perc %>% 
  dplyr::filter(perc_change < 0) %>% 
  ggplot(aes(x = buffer, y = perc_change)) +
  geom_violin() +
  #geom_smooth(method = "lm") +
  facet_wrap(~year)


tan_perc %>% 
  dplyr::filter(perc_change < 0) %>% 
  #dplyr::filter(year == 08) %>% 
  ggplot(aes(x = perc_change, group = buffer)) +
  geom_density(aes(col = buffer)) %>% 
  facet_wrap(~year)


tan_perc %>% 
  dplyr::filter(perc_change < 0) %>% 
  dplyr::filter(buffer == 100) %>% 
  ggplot(aes(x = perc_change, group = factor(year))) +
  geom_density(aes(col = factor(year))) #%>% 
 # facet_wrap(~buffer)



tan_perc %>% 
  dplyr::filter(!perc_change == 07) %>% 
  ggplot() +
  geom_bar(aes(x = buffer)) +
  facet_wrap(~year)



tan_perc %>% 
  dplyr::filter(perc_change > 0) %>% 
  ggplot(aes(x = factor(year), y = perc_change)) +
  geom_boxplot(aes(fill = buffer)) +
  scale_fill_manual(values = c("#EE3700", "#E1AF00", "#EBCC2A","#78B7C5")) + # red, yellow, blue
  theme_classic()

tan_perc %>% 
  dplyr::filter(perc_change > 0) %>% 
  ggplot(aes(x = buffer, y = perc_change)) +
  geom_boxplot(aes(fill = factor(year))) +
  scale_fill_manual(values = c("#EE3700", "#EBCC2A", "#78B7C5", )) + # red, yellow, blue
  theme_bw() +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "year")


tan_perc %>% 
  dplyr::filter(perc_change < 0) %>% 
  ggplot(aes(x = buffer, y = perc_change)) +
  geom_boxplot(aes(fill = factor(year))) +
  scale_fill_manual(values = c("#EE3700", "#EBCC2A", "#78B7C5")) + # red, yellow, blue
  theme_bw()



head(tan_perc)

levels(tan_total_long$buffer) <- list("<20km"="20","20-50km"="50", "50-80km"="80", ">80km"="100")

tan_total_long %>% 
 # dplyr::filter(buffer == 20) %>% 
  ggplot(aes(x = year, y = biomass, fill = buffer)) +
  stat_smooth(method="lm", aes(fill = buffer, col = buffer)) +
  labs(title = "Tanzania biomass change in buffer area",
       y = "Average biomass (MgC/ha/km)",
       caption = "McNicol et al. 2018\ngeom_smooth(method = lm)") +
  theme_bw()





tan_total_long %>% 
  dplyr::filter(year == 10) %>% 
  ggplot(aes(x = buffer, y = biomass)) +
  stat_smooth(method="lm")



  



# 20km percentage change
agc_20_df <- agc_20_df %>% 
  mutate(perc_chng07_20 = (agc2008 - agc2007)/agc2007 )


# 50km percentage change
agc_50_df <- agc_50_df %>% 
  mutate(perc_chng07_50 = (agc2008 - agc2007)/agc2007 )


# 80km percentage change
agc_80_df <- agc_80_df %>% 
  mutate(perc_chng07_80 = (agc2008 - agc2007)/agc2007 )


ggplot() +
  geom_tile(agc_outside_df, mapping = aes(x=x, y=y, fill = agc2007))


agc_perc20 <- agc_20_df %>% 
  dplyr::select(perc_chng07_20, agc2007) %>% 
  na.omit()
agc_perc50 <- agc_50_df %>% 
  dplyr::select(perc_chng07_50)
agc_perc80 <- agc_80_df %>% 
  dplyr::select(perc_chng07_80)


perc_chng07<- merge(agc_perc20, agc_perc50, by =0, all=TRUE)
perc_chng07 <- merge(perc_chng07, agc_perc80, by =0)
bfr_chng <- left_join(bfr_chng, agc_8bf, by = "sustained_change")




ggplot() +
  geom_density(agc_perc20, mapping = aes(perc_chng07_20), col = "#EE3700", size = 1, alpha = 0.5) + # red
  geom_density(agc_perc50, mapping = aes(perc_chng07_50), col = "#EBCC2A", size = 1, alpha = 0.5) + # yellow
  geom_density(agc_perc80, mapping = aes(perc_chng07_80), col = "#78B7C5", size = 1, alpha = 0.5) + # blue
  labs(x = "Percentage change from 2007 to 2008",
       title = "Biomass change within buffer zone of urban area",
       subtitle = "red = 20km, yellow = 50km, blue = 80km") +
  xlim(-1, 2) +
  theme_bw()
  


### I've miss out cropping out the other buffer zones!!!
