#22nd july saving road buffer


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
library(Rmisc) #summarySE
library(tidyr)


# set working directory
#github goldeneaglewarrior, dissertation


## load data ----
agc_road_hi <- read.csv(file ="data/agc/agc_road_hi.csv")
agc_road_lo <- read.csv(file ="data/agc/agc_road_lo.csv")
agc_road_rural <- read.csv(file ="data/agc/agc_road_rural.csv")

## country ----
country_outline <-  map_data("world", regions = "Tanzania")

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")


ggplot() +
  geom_tile(agc_road_lo, mapping = aes(x=x,y=y, fill = agc2008)) +
  geom_tile(agc_road_hi, mapping = aes(x=x,y=y, fill = agc2008)) + #no buffer
  geom_tile(agc_road_rural, mapping = aes(x=x,y=y, fill = agc2008)) +
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  theme_classic() 


# z column

agc_road_hi <- agc_road_hi %>% 
  unite("z", x:y, remove = FALSE)

agc_road_lo <- agc_road_lo %>% 
  unite("z", x:y, remove = FALSE)

agc_road_rural <- agc_road_rural %>% 
  unite("z", x:y, remove = FALSE)




# hi
agc_road_hi <- agc_road_hi %>% 
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_road_hi <- agc_road_hi %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# lo
agc_road_lo <- agc_road_lo %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_road_lo <- agc_road_lo %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# rural
agc_road_rural <- agc_road_rural %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_road_rural <- agc_road_rural %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 


# joining hi
hi_road_bio <- agc_road_hi %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

hi_road_perc <- agc_road_hi %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

hi_road_flux <- agc_road_hi %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

road_join_hi <- merge(hi_road_bio, hi_road_perc, by=c("z","year", "buffer"))
road_join_hi <- merge(road_join_hi, hi_road_flux, by=c("z","year", "buffer"))


# joining lo
lo_road_bio <- agc_road_lo %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

lo_road_perc <- agc_road_lo %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

lo_road_flux <- agc_road_lo %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

road_join_lo <- merge(lo_road_bio, lo_road_perc, by=c("z","year", "buffer"))
road_join_lo <- merge(road_join_lo, lo_road_flux, by=c("z","year", "buffer"))


# joining rural
rural_road_bio <- agc_road_rural %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

rural_road_perc <- agc_road_rural %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

rural_road_flux <- agc_road_rural %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

road_join_rural <- merge(rural_road_bio, rural_road_perc, by=c("z","year", "buffer"))
road_join_rural <- merge(road_join_rural, rural_road_flux, by=c("z","year", "buffer"))



write.csv(road_join_lo,file="data/agc/road_join_lo.csv", row.names = FALSE)
write.csv(road_join_hi,file="data/agc/road_join_hi.csv", row.names = FALSE)
write.csv(road_join_rural,file="data/agc/road_join_rural.csv", row.names = FALSE)
