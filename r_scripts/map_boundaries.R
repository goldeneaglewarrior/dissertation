# tanzania regional boundaries
# 11/5/21

library(sf)
library(ggplot2)
library(dplyr)
#install.packages("ggsn")
library(ggsn)
library(spsurvey) # read.dbf

getwd()

tan_dist <- st_read("data/tan_district/Districts.shp")

tan_reg <- st_read("data/tan_regions/Regions.shp")

drc_prov <- st_read("data/drc_province/COD_admbnda_adm1_20170407.shp")

## polygon outline of tanzania -----
tanza <-  map_data("world", regions = "Tanzania")


lakes <- st_read("data/african_waterbodies/Africa_waterbody.shp")

rivers <- st_read("data/african_rivers/rivers_africa_37333.shp")
rivers <- read.dbf("african_rivers/rivers_africa_37333.dbf")



tan_dist

ggplot(tan_dist) +
  geom_sf() +
  theme_classic() +
  labs(caption = "Shape source from \ndatacatalog.worldbank.org")

ggplot(tan_reg) +
  geom_sf() +
  theme_classic() +
  labs(caption = "Shape source from \ndatacatalog.worldbank.org")

ggplot(drc_prov) +
  geom_sf() +
  geom_sf(katanga, mapping = aes(fill = NOM)) +
  theme_bw() +
  geom_sf_text(katanga, mapping = aes(label = NOM), size = 3) +
  
  labs(caption = "Shape source from \ndatacatalog.worldbank.org") +
  theme(panel.background = element_rect(fill = "azure")) +
  scalebar(drc_prov,
           x.min = 15, x.max = 25, y.min = -12, y.max = -10, 
           dist = 50, dist_unit = "km", transform = FALSE, model = "WGS84")+
  north(drc_prov, 12)

drc_prov

katanga <- drc_prov %>% 
  filter(NOM %in% c("Lualaba", "Lomami", "Haut-Lomami", "Haut-Katanga", "Tanganyika"))





####
## Lakes ----

ggplot(lakes) +
  geom_sf(colour = "black", fill = "azure") +
  theme_classic() +
  labs(caption = "Shape source from \ndatacatalog.worldbank.org") +
  xlim(30, 40) +
  ylim(-12, 0) +
  geom_polygon(data = tanza, 
               aes(x=long, y = lat, group = group), 
               fill = NA, size = 1, colour = "black") +
  coord_sf()


## rivers


ggplot(rivers) +
  geom_sf(colour = "blue") +
  theme_classic() +
  labs(caption = "Shape source from \nfao.org/geonetwork") +
  xlim(30, 40) +
  ylim(-12, 0) +
  geom_polygon(data = tanza, 
               aes(x=long, y = lat, group = group), 
               fill = NA, size = 2, colour = "black") 


unique(rivers$SUB_NAME)

