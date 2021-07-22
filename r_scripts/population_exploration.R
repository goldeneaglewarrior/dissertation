## Population exploration

pop_rural <- read.csv(file ="data/pop/pop_rural.csv")
pop_hi <- read.csv(file ="data/pop/pop_total_hi.csv")
pop_lo <- read.csv(file ="data/pop/pop_total_lo.csv")

pop_rural <- pop_rural %>% 
  na.omit()

sum(pop_rural$ppp_2007_1km_Aggregated)


pop_hi %>% 
  dplyr::filter(buffer == 80) %>% 
  sum(.$pop_km)
  
pop_hi %>% 
  sum(.$pop_km)

pop_hi %>% 
  ggplot() +
  geom_tile(aes(x=x, y=y, fill = pop_km))

pop_lo %>% 
  ggplot() +
  geom_tile(aes(x=x, y=y, fill = pop_km))

sum(pop_lo$pop_km)
sum(pop_hi$pop_km)
sum(pop_rural$ppp_2007_1km_Aggregated)
