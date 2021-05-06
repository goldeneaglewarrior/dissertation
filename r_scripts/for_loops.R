## For loop africa
# 05/05/21
# Dougie Brunton
# 


countries <- c("Tanzania", "Mozambique", "Malawi", "Zimbabwe")

# Tanzania spdf map
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")
#load data
africa_07 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif')



for(i in list(countries)){
  print(i)
}
