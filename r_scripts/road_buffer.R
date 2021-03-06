## tanzania roads 

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
#library(ggridges)

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")
country_outline <-  map_data("world", regions = "Tanzania")

ggplot(bfr_20) +
  geom_sf(aes(fill = CONDITION)) +
  geom_polygon(country_outline, mapping = aes(x = long, y = lat, group = group), 
               fill = NA, colour = "black")

africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")


bfr_20 <- tan_roads %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000)# %>% 
  as_Spatial()


bfr_10 <- tan_roads %>% 
  st_buffer(dist = .1) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


agc_20 <- africa_07 %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T)


agc_20 <- africa_07 %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T)



ggplot() +
  geom_tile(agc_20, mapping = aes(x=x, y=y, fill = mcnicol_AGC2007_1km)) +
  geom_polygon(country_outline, mapping = aes(x = long, y = lat, group = group), 
                fill = NA, colour = "black") +
  geom_sf(tan_roads, mapping = aes(group = SURFTYPE), col = "red")+
  scale_fill_viridis_c(na.value = NA) +
  theme_classic()





## load data ----
africa_07 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif")
africa_08 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif")
africa_09 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif")
africa_10 <- raster("Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif")

world_pop <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')
tan_roads <- st_read("data/roads/Tanzania_Roads.shp")

pal_cava <- wes_palette("Cavalcanti1", 21, type = "continuous")
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


pop_1500 <- pop %>% 
  rasterToContour(nlevels = 40) %>% 
  st_as_sf() %>% 
  dplyr::filter(level == "1500")


bfr_20 <- pop_1500 %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_50 <- pop_1500 %>% 
  st_buffer(dist = .5) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

bfr_80 <- pop_1500 %>% 
  st_buffer(dist = .8) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()


road_20 <- tan_roads %>% 
  st_buffer(dist = .2) %>%  # approximately 20km
  fill_holes(threshold = 1000000000000) %>% 
  as_Spatial()

# crop and df 
# road crop
agc_stack <- agc_stack %>% 
  crop(extent(road_20)) %>% 
  mask(road_20)

# 20km
agc_20 <- agc_stack %>% 
  crop(extent(bfr_20)) %>% 
  mask(bfr_20) %>% 
  as.data.frame(xy = T)# %>% 
#na.omit()


# 50km crop 
agc_50 <- agc_stack %>% 
  crop(extent(bfr_50)) %>% 
  mask(bfr_50)  %>% 
  crop(bfr_20) %>% 
  mask(bfr_20, inverse = T) %>% 
  as.data.frame(xy = T)# %>% 
#na.omit()



# 80km
agc_80 <- agc_stack %>% 
  crop(extent(bfr_80)) %>% 
  mask(bfr_80) %>% 
  crop(bfr_50) %>% 
  mask(bfr_50, inverse = T) %>% 
  as.data.frame(xy = T)# %>% 
#na.omit()#



# outside buffer 
agc_country <- agc_stack %>% 
  #crop(extent(bfr_80)) %>% 
  #mask(bfr_80, inverse = T) %>% 
  as.data.frame(xy = T) #%>% 
#na.omit()


ab <- anti_join(agc_80, agc_50)

ab %>% 
  ggplot() +
  geom_polygon(aes(x=x,y=y, fill = agc2007), na.rm = T) +
  theme_classic()


ggplot() +
  geom_tile(agc_20, mapping = aes(x=x,y=y, fill = agc2007))+
  #geom_tile(agc_50, mapping = aes(x=x,y=y, fill = agc2007)) +
  #geom_tile(agc_80, mapping = aes(x=x,y=y, fill = agc2007)) +
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  # scale_fill_continuous(na.value = NA) +
  coord_quickmap() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_viridis_c(na.value = NA) 



bfr_20
# buffer column
agc_20$buffer <- (20)
agc_50$buffer <- (50)
agc_80$buffer <- (80)
agc_country$buffer <- (100)




total <- full_join(agc_20, agc_50)
total <- full_join(total, agc_80)
total <- full_join(total, agc_country)

#summary(total_long)
total <- total[rowSums(is.na(total[c(3:6)])) != 4, ]



total <- total %>% 
  dplyr::mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  dplyr::mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  dplyr::mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)


total_biomass_perc <- total %>% 
  dplyr::select(3,8:10) %>% 
  pivot_longer(!agc2007, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" )


total_biomass_perc <- na.omit(total_biomass_perc)


total_long <- total %>% 
 # filter(agc2007 > 20) %>% 
  dplyr::select(7:10) %>%
  #dplyr::mutate(perc_07 = 0) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" )


total_long_agc <- total %>% 
  # filter(agc2007 > 20) %>% 
  dplyr::select(3:7) %>%
  #dplyr::mutate(perc_07 = 0) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "agc") %>% 
  separate(year, c(NA, "year"), sep = "c" )


total_long$year <- as.numeric(total_long$year)


total_long <- total_long %>% 
  dplyr::filter(perc_change <= 100, perc_change >= -100)



total_long %>% 
  dplyr::filter(perc_change < 0) %>% 
  ggplot(aes(x = factor(year), y = perc_change)) +
  geom_boxplot(aes(fill = factor(buffer))) +
  scale_fill_manual(name = "Distance from\nurban area",
                    values = c("#EE3700", "#E1AF00", "#EBCC2A","#78B7C5"),
                    labels = c("< 20km ", "20 - 50km", "50 - 80km", "whole country")) + # red, yellow, blue
  theme_classic() +
  labs(title = "Mozambique annual AGB loses around urban area",
       caption = "Source: McNicol et al. 2018",
       x = "Year",
       y = "Annual biomass loses (%)") +
  scale_x_discrete(labels=c("2008" = "'07 to '08", "2009" = "'08 to '09",
                            "2010" = "'09 to '10")) +
  scale_y_reverse()


total_long_agc %>% 
 # dplyr::filter(perc_change < 0) %>% 
  ggplot(aes(x = factor(year), y = agc)) +
  geom_boxplot(aes(fill = factor(buffer))) +
  scale_fill_manual(name = "Distance from\nurban area",
                    values = c("#EE3700", "#E1AF00", "#EBCC2A","#78B7C5"),
                    labels = c("< 20km ", "20 - 50km", "50 - 80km", "whole country")) + # red, yellow, blue
  theme_classic() +
  labs(title = "AGC around urban area",
       caption = "Source: McNicol et al. 2018",
       x = "Year")



#gains
total_long %>% 
  dplyr::filter(perc_change < -50) %>% 
  dplyr::filter(perc_change > 50) %>% 
  ggplot(aes(x = factor(buffer), y = perc_change)) +
  geom_boxplot(aes(fill = factor(year))) +
  scale_fill_manual(name = "Years",
                    values = c("#D8B70A", "#A2A475", "#81A88D"),
                    labels = c("'07 to '08 ", "'08 to '09", "'09 to '10")) + # red, yellow, blue
  theme_classic() +
  labs(title = "Mozambique annual AGB loses around urban area",
       caption = "Source: McNicol et al. 2018",
       x = "Buffer from urban area (1500 people per km2)",
       y = "Annual aboveground biomass loses (%)") +
  scale_x_discrete(labels=c("20" = "< 20km", "50" = "20 - 50km",
                            "80" = "50 - 80km", "100" = "Whole country"))
# ylim(-50,50)



total_biomass_perc %>% 
  ggplot(aes(x = agc2007, y = perc_change, col = factor(year))) +
  geom_smooth(method = "lm") +
  ylim(-100, 100)


total_biomass_perc %>%
  filter(year == 2008) %>%
  filter(agc2007 > 20) %>% # actually forest at the start
  filter(perc_change < 0) %>% 
  ggplot(aes(x = agc2007, y = perc_change)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  ylim(-100, 0) +
  scale_y_reverse()





total_long %>% 
  # filter(!year == 7) %>% 
  ggplot(aes(factor(buffer))) +
  geom_bar(aes(fill = factor(year)))

total %>% 
  filter(is.na(buffer)) %>% 
  ggplot() +
  geom_tile(aes(x=x, y=y, fill = agc2007))




levels(total_long$buffer)

total_long$buffer <- as.factor(total_long$buffer)
total_long$buffer <- factor(total_long$buffer, levels=rev(levels(total_long$buffer)))

total_long %>% 
  dplyr::filter(perc_change < 0) %>% 
  dplyr::filter(!is.na(buffer)) %>% 
  ggplot(aes(x = factor(year), y = perc_change, fill = (factor(buffer)))) +
  geom_flat_violin(position = position_nudge(x = -.25, y = 0), alpha = 0.5) +
  scale_fill_manual(name = "Distance from\nurban area",
                    values = c("#EE3700", "#E1AF00", "#EBCC2A","#78B7C5"),
                    labels = c("< 20km ", "20 - 50km", "50 - 80km", "whole country")) + # red, yellow, blue
  theme_classic() +
  labs(title = "Mozambique annual AGB loses around urban area",
       caption = "Source: McNicol et al. 2018",
       x = "Year",
       y = "Annual biomass loses (%)") +
  scale_x_discrete(labels=c("2008" = "'07 to '08", "2009" = "'08 to '09",
                            "2010" = "'09 to '10")) +
  scale_y_reverse() +
  coord_flip()


total_long %>% 
  dplyr::filter(perc_change < 0) %>% 
  dplyr::filter(!is.na(buffer)) %>% 
  ggplot(aes(x = factor(buffer), y = perc_change, col = factor(year))) +
  geom_flat_violin(position = position_nudge(x = -.25, y = 0), alpha = 0) +
  #scale_fill_manual(name = "Distance from\nurban area",
  #                  values = c("#EE3700", "#E1AF00", "#EBCC2A","#78B7C5"),
  #                  labels = c("< 20km ", "20 - 50km", "50 - 80km", "whole country")) + # red, yellow, blue
  theme_classic() +
  labs(title = "Mozambique annual AGB loses around urban area",
       caption = "Source: McNicol et al. 2018",
       x = "Year",
       y = "Annual biomass loses (%)") +
  # scale_x_discrete(labels=c("2008" = "'07 to '08", "2009" = "'08 to '09",
  #                           "2010" = "'09 to '10")) +
  scale_y_reverse() +
  coord_flip()









print("hello")





agc_50$xy <- agc_50$x * agc_50$y
agc_20$xy <- agc_20$x * agc_20$y

length(unique(agc_20$xy))

a <- agc_50[!(agc_20$xy %in% agc_50$xy),]

a <- anti_join(agc_50, agc_20 ,by = "xy")




a <- total_to50 %>% 
  arrange(buffer) %>% 
  distinct(x, y, .keep_all=TRUE)

152403 -24474





a <- aggregate(buffer ~ agc2007 +agc2008 +agc2009 +agc2010, total_to50, max)

b <- agc_50 %>% 
  dplyr::select(x, y, agc2007) %>% 
  na.omit()

agc_50
ab <- right_join(a, b, by = c("agc2007"))
ungroup(a)





total_long <- total %>% 
  dplyr::select(!x & !y) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" )


total_long$year <- as.numeric(total_long$year)



hello

agc_outside <- agc_outside %>% 
  mutate(fill = ifelse(agc2007 >= 0, "yes", NA))

ggplot() +
  geom_tile(agc_outside, mapping = aes(x=x,y=y, fill = fill)) +
  theme_classic()#+ #red
geom_tile(agc_80, mapping = aes(x=x,y=y, fill = agc2007), col = "#E1AF00") + # gold
  geom_tile(agc_50, mapping = aes(x=x,y=y, fill = agc2007), col = "#EBCC2A") + # yellow
  geom_tile(agc_20, mapping = aes(x=x,y=y, fill = agc2007), col = "#78B7C5") + # blue
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  # scale_fill_continuous(na.value = NA) +
  coord_quickmap() +
  theme_classic() +
  theme(legend.position = "none")




