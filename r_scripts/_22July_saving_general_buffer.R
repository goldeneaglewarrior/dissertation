# Saving joined general buffer 






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
agc_total_hi <- read.csv(file ="data/agc/agc_total_hi.csv")
agc_total_lo <- read.csv(file ="data/agc/agc_total_lo.csv") # doesn't have buffer <cry>
agc_rural <- read.csv(file ="data/agc/agc_rural.csv")

agc_rural <- agc_rural[rowSums(is.na(agc_rural[c(3:6)])) != 4, ]

## country ----
country_outline <-  map_data("world", regions = "Tanzania")

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")


ggplot() +
  geom_tile(agc_total_hi, mapping = aes(x=x,y=y, fill = agc2008)) +
  #  geom_tile(agc_total_lo, mapping = aes(x=x,y=y, fill = factor(buffer))) + #no buffer
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  theme_classic() 


# z column

agc_total_hi <- agc_total_hi %>% 
  unite("z", x:y, remove = FALSE)

agc_total_lo <- agc_total_lo %>% 
  unite("z", x:y, remove = FALSE)

agc_rural <- agc_rural %>% 
  unite("z", x:y, remove = FALSE)




# hi
agc_total_hi <- agc_total_hi %>% 
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_total_hi <- agc_total_hi %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# lo
agc_total_lo <- agc_total_lo %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_total_lo <- agc_total_lo %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# rural
agc_rural <- agc_rural %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_rural <- agc_rural %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 


# joining hi
hi_agc_bio <- agc_total_hi %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

hi_agc_perc <- agc_total_hi %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

hi_agc_flux <- agc_total_hi %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

agc_join_hi <- merge(hi_agc_bio, hi_agc_perc, by=c("z","year", "buffer"))
agc_join_hi <- merge(agc_join_hi, hi_agc_flux, by=c("z","year", "buffer"))


# joining lo
lo_agc_bio <- agc_total_lo %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

lo_agc_perc <- agc_total_lo %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

lo_agc_flux <- agc_total_lo %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

agc_join_lo <- merge(lo_agc_bio, lo_agc_perc, by=c("z","year", "buffer"))
agc_join_lo <- merge(agc_join_lo, lo_agc_flux, by=c("z","year", "buffer"))


# joining rural
rural_agc_bio <- agc_rural %>% 
  dplyr::select(1,4:7) %>% 
  pivot_longer(!z, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

rural_agc_perc <- agc_rural %>% 
  dplyr::select(1,8:11) %>% 
  pivot_longer(!z, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

rural_agc_flux <- agc_rural %>% 
  dplyr::select(1, 12:15) %>% 
  pivot_longer(!z, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

agc_join_rural <- merge(rural_agc_bio, rural_agc_perc, by=c("z","year"))
agc_join_rural <- merge(agc_join_rural, rural_agc_flux, by=c("z","year"))



write.csv(agc_join_lo,file="data/agc/agc_join_lo.csv", row.names = FALSE)
write.csv(agc_join_hi,file="data/agc/agc_join_hi.csv", row.names = FALSE)
