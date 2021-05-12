## For loop africa
# 05/05/21
# Dougie Brunton
# 



data = iris
head(iris)

lapply(data[,1:4], mean)

lapply(data[,1:4], FUN = function(x) max(x) - min(x))

orange_list = split(Orange, f = Orange$Tree)

lapply(orange_list, '[', 1,)

lapply(orange_list, '[', , 3)

lapply(orange_list, "[", 2,3)

csv_list = dir()

files = lapply(csv_list, read.csv)

data2 = do.call(rbind, files)






countries <- c("Tanzania", "Mozambique", "Malawi", "Zimbabwe")

# Tanzania spdf map
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")
#load data
africa_07 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif')



#lapply is faster and better

for(i in list(countries)){
  print(i)
}
