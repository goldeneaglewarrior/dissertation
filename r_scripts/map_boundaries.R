# tanzania regional boundaries
# 11/5/21

library(sf)
library(ggplot2)
library(dplyr)
#install.packages("ggsn")
library(ggsn)

getwd()

tan_dist <- st_read("data/tan_district/Districts.shp")

tan_reg <- st_read("data/tan_regions/Regions.shp")

drc_prov <- st_read("data/drc_province/COD_admbnda_adm1_20170407.shp")


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


