#22nd july saving pa buffer
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
agc_pa_hi <- read.csv(file ="data/agc/agc_pa_hi.csv")
agc_pa_lo <- read.csv(file ="data/agc/agc_pa_lo.csv")
agc_pa_rural <- read.csv(file ="data/agc/agc_pa_rural.csv")

## country ----
country_outline <-  map_data("world", regions = "Tanzania")

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")


ggplot() +
  geom_tile(agc_pa_rural, mapping = aes(x=x,y=y, fill = agc2008)) +
  #  geom_tile(agc_total_lo, mapping = aes(x=x,y=y, fill = factor(buffer))) + #no buffer
  geom_polygon(data = country_outline, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  theme_classic() 


# z column

agc_pa_hi <- agc_pa_hi %>% 
  unite("z", x:y, remove = FALSE)

agc_pa_lo <- agc_pa_lo %>% 
  unite("z", x:y, remove = FALSE)

agc_pa_rural <- agc_pa_rural %>% 
  unite("z", x:y, remove = FALSE)




# hi
agc_pa_hi <- agc_pa_hi %>% 
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_pa_hi <- agc_pa_hi %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# lo
agc_pa_lo <- agc_pa_lo %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_pa_lo <- agc_pa_lo %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 

# rural
agc_pa_rural <- agc_pa_rural %>%
  mutate(perc_2007 = NA) %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_pa_rural <- agc_pa_rural %>% 
  mutate(agcflux_2007 = NA) %>% 
  mutate(agcflux_2008 = agc2008 - agc2007) %>% 
  mutate(agcflux_2009 = agc2009 - agc2008) %>% 
  mutate(agcflux_2010 = agc2010 - agc2009) 


# joining hi
hi_pa_bio <- agc_pa_hi %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

hi_pa_perc <- agc_pa_hi %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

hi_pa_flux <- agc_pa_hi %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

pa_join_hi <- merge(hi_pa_bio, hi_pa_perc, by=c("z","year", "buffer"))
pa_join_hi <- merge(pa_join_hi, hi_pa_flux, by=c("z","year", "buffer"))


# joining lo
lo_pa_bio <- agc_pa_lo %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

lo_pa_perc <- agc_pa_lo %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

lo_pa_flux <- agc_pa_lo %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

pa_join_lo <- merge(lo_pa_bio, lo_pa_perc, by=c("z","year", "buffer"))
pa_join_lo <- merge(pa_join_lo, lo_pa_flux, by=c("z","year", "buffer"))


# joining rural
rural_pa_bio <- agc_pa_rural %>% 
  dplyr::select(1,4:8) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

rural_pa_perc <- agc_pa_rural %>% 
  dplyr::select(1,8:12) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

rural_pa_flux <- agc_pa_rural %>% 
  dplyr::select(1,8, 13:16) %>% 
  pivot_longer(!z & !buffer, names_to = "year", values_to = "agc_flux") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

pa_join_rural <- merge(rural_pa_bio, rural_pa_perc, by=c("z","year", "buffer"))
pa_join_rural <- merge(pa_join_rural, rural_pa_flux, by=c("z","year", "buffer"))



write.csv(pa_join_lo,file="data/agc/pa_join_lo.csv", row.names = FALSE)
write.csv(pa_join_hi,file="data/agc/pa_join_hi.csv", row.names = FALSE)
write.csv(pa_join_rural,file="data/agc/pa_join_rural.csv", row.names = FALSE)
