## World Database on Protected Areas ----
# Dougie Brunton
# s1748349@ed.ac.uk


## Load packages ----
#install.packages("packages")

#install.packages("wdpar", repos = "https://cran.rstudio.com/")
#install.packages("curl")
library(curl)
library(sf)
library(wdpar)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rnaturalearthdata)
library(rnaturalearth)
#install.packages("processx")
library(processx)
#if (!require(devtools))
#  install.packages("devtools")
#devtools::install_github("prioritizr/wdpar")
curl::has_internet()

## tanzania spdf ----
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale, i think
                         country = "United Republic of Tanzania")

## malawi spdf ----
malawi_SPDF <- ne_countries(scale = 50, #1:50 million scale, i think
                            country = "Malawi")

tanza <-  map_data("world", regions = "Tanzania")
malawi <-  map_data("world", regions = "Malawi")



tza_raw_pa_data <- wdpa_fetch("TZA", wait = TRUE)

tza_pa_data <- wdpa_clean(tza_raw_pa_data)

# print preview
head(tza_pa_data, 1)


# reproject data to longitude/latitude for plotting
tza_pa_data <- st_transform(tza_pa_data, 4326)

# download basemap imagery
bg <- get_stamenmap(unname(st_bbox(tza_pa_data)), zoom = 8,
                    maptype = "watercolor", force = TRUE)


names(tza_pa_data)
str(tza_pa_data)

# make map

tza_pa_data %>% 
  filter(DESIG == "National Park") %>% 
  ggplot() +
  geom_polygon(data = tanza, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  geom_sf(aes(fill = IUCN_CAT), col = "black", alpha = 0.7, inherit.aes = FALSE) +
#  geom_sf_text(aes(label=NAME, color = IUCN_CAT),
#               size= 2, fontface = "bold") +
  theme(axis.title = element_blank(), legend.position = "bottom") +
  scale_fill_viridis_d("IUCN \nCategory") +
  labs(title = "Tanzania National Parks",
       x = "Longitude",
       y = "Latitude",
       caption = "World Database on \nProtected Areas") +
  theme_classic()

hist(tza_pa_data$STATUS_YR, main = "Tanzania's protected areas", xlab = "Year established")

serengenti <- tza_pa_data %>% 
  filter(NAME == "Serengeti National Park" & DESIG == "National Park")

forest_reserve <- tza_pa_data %>% 
  filter(DESIG == "Forest Reserve")


ggplot(serengenti) +
  geom_sf(aes(fill = IUCN_CAT))


seren07 <- crop(africa_07, extent(serengenti))



statistic <-
  tza_pa_data %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  group_by(IUCN_CAT) %>%
  summarize(area_km = sum(AREA_KM2)) %>%
  ungroup() %>%
  mutate(percentage = (area_km / sum(area_km)) * 100) %>%
  arrange(desc(area_km))

print(statistic)





#
#
#
#malawi - have to restart sessionfor some reason

MWI_raw_pa_data <- wdpa_fetch("MWI", wait = TRUE)

mwi_pa_data <- wdpa_clean(MWI_raw_pa_data)

# print preview
head(tza_pa_data, 1)


# reproject data to longitude/latitude for plotting
mwi_pa_data <- st_transform(mwi_pa_data, 4326)

# download basemap imagery
bg <- get_stamenmap(unname(st_bbox(mwi_pa_data)), zoom = 8,
                    maptype = "watercolor", force = TRUE)

# make map
ggmap(bg) +
  geom_sf(aes(fill = IUCN_CAT), data = mwi_pa_data, inherit.aes = FALSE) +
  theme(axis.title = element_blank(), legend.position = "bottom")


ggplot() +
  geom_polygon(data = malawi, 
               aes(x=long, y = lat, group = group), 
               fill = NA, colour = "black") +
  geom_sf(aes(fill = IUCN_CAT), data = mwi_pa_data, inherit.aes = FALSE) +
  theme(axis.title = element_blank(), legend.position = "bottom") +
  coord_sf()+
  theme_classic()

hist(tza_pa_data$STATUS_YR, main = "Tanzania's protected areas", xlab = "Year established")

names()

