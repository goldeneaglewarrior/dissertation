## Tanzania animated 4 year densities ----
# github diss
# 25/4/21
# Dougie Brunton
# s1748349@ed.ac.uk



## libraries ----
library(sp)
library(raster)
library(tidyverse)
library(rnaturalearthdata)
library(rnaturalearth)
library(gganimate)
library(gifski) # to make gifs work for me
library(ggridges)
library(ggtext)
library(plotly)
library(ggpubr) #ggarrange
library(magick)



theme_doug <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          plot.subtitle = element_text(size = 14, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.5, 0.8))
}





## load data ----
africa_07 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif')
africa_08 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2008_1km.tif')
africa_09 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2009_1km.tif')
africa_10 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2010_1km.tif')

wpop10 <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')


# Tanzania spdf map
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")


## Crop to Tanzania ----
#agc
tanagc07 <- crop(africa_07, extent(tan_SPDF))
tanagc07 <- mask(tanagc07, tan_SPDF)

tanagc08 <- crop(africa_08, extent(tan_SPDF))
tanagc08 <- mask(tanagc08, tan_SPDF)

tanagc09 <- crop(africa_09, extent(tan_SPDF))
tanagc09 <- mask(tanagc09, tan_SPDF)

tanagc10 <- crop(africa_10, extent(tan_SPDF))
tanagc10 <- mask(tanagc10, tan_SPDF)

#pop
tanpop10 <- crop(wpop10, extent(tan_SPDF))
tanpop10 <- mask(tanpop10, tan_SPDF)



# project the pop raster to agc resoltuon
tanpop_new_r <- projectRaster(from = tanpop10, to = tanagc07, method = "ngb") 
# same resolution
# moves to nearest neighbour whilst keeping the z value unchanged (which interpolate would do)

# Stack covariates
rast_stack <- stack(tanpop_new_r, 
                    tanagc07,
                    tanagc08,
                    tanagc09,
                    tanagc10)

names(rast_stack) <- c("pop2010", "agc2007", "agc2008", "agc2009", "agc2010")

tan_df <- rast_stack %>% 
  as.data.frame(xy = T)
# still has na values



tan_df <- tan_df %>% 
  mutate(roundedpop = round(pop2010, digits = 0))
length(unique(tan_df$roundedpop))




tan_df$xy <- paste(tan_df$x, tan_df$y, sep = "_")

tan_stack_xy <- select(tan_df, -"x", -"y")

tan_longer <- tan_stack_xy %>% 
  pivot_longer(
    cols = starts_with("agc"),
    names_to = "year",
    names_prefix = "agc",
    values_to = "MgC_km"
  ) # values_drop_na = T, not added


tan_longer %>% 
  filter(year == 2007) %>%
  filter(roundedpop %in% 1:10) %>% 
  group_by(roundedpop) %>% 
  cumsum(roundedpop)# %>% 
#ungroup() %>% 
#mutate(perc_land = (n/land_t)*100)


tan_longer %>% 
  filter(year == 2007) %>%
  filter(roundedpop %in% 1:10) %>% 
  group_by(MgC_km, roundedpop) %>% 
  sum(MgC_km, na.rm = T)

tan_longer %>% 
  filter(roundedpop %in% 1:10) %>% 
  group_by(roundedpop) %>% 
  count(MgC_km, roundedpop) %>% 
  ungroup()

(cumareaplot <- tan_longer %>%
    filter(roundedpop %in% 1:60) %>% 
    group_by(roundedpop) %>% 
    summarise(sum_agc = sum(!is.na(MgC_km))) %>% 
    ggplot(aes(x = roundedpop, y = (sum_agc))) +
    geom_line(size = 1.5) +
    labs(title = "Tanzania 2007 - 2010, population density 0 - 60",
         subtitle = 'Population Density: **{as.integer(frame_time)}** people per km2', 
         y = "cumulative area of agc measured (km2)") +
    theme_doug() +
    transition_reveal(roundedpop))


tan_longer %>% 
  filter(roundedpop %in% 1:60) %>% 
  group_by(year, roundedpop) %>% 
  summarise(sum_agc = sum(!is.na(MgC_km))) %>% 
  ggplot(aes(x = roundedpop, y = sum_agc, group = year)) +
  geom_line(aes(colour = year), size= 1, alpha = 0.4) +
  geom_point(aes(colour = year)) +
  scale_colour_viridis_d() +
  theme_doug() +
  transition_reveal(roundedpop) +
  labs(title = "Tanzania, population density 0 - 60",
      # subtitle = 'Population Density: **{as.integer(frame_time)}** people per km2', 
       y = "total area of agc measured (km2)",
       x = "population density, people per km2",
       caption = "McNicol et al. 2018, \n Worldpop.org") #+
  #theme(plot.subtitle = element_markdown())
  


library(plotly)


p <- tan_longer %>% 
  filter(roundedpop %in% 1:10) %>% 
  group_by(year, roundedpop) %>% 
  summarise(sum_agc = sum(!is.na(MgC_km))) %>% 
  ggplot(aes(x = roundedpop, y = sum_agc, group = year, frame = roundedpop)) +
  geom_line(aes(colour = year), size= .5, alpha = 0.4) +
  geom_point(aes(colour = year)) +
  scale_colour_viridis_d() +
  theme_doug()

fig = ggplotly(p)

fig




land_t <- length(tan_df$pop2010[!is.na(tan_df$pop2010)])

# wiki says tanzania has 883,750 land area km2
# this dataset says... 1,479,320
# 897178
#
#
#
#
#
#

fouryears <- tan_longer %>% 
  filter(roundedpop %in% c(1:60)) %>%  
  ggplot() +
  geom_line(aes(x = MgC_km, color=year), 
            stat="density", size=1.3, alpha=0.6) +
  scale_colour_viridis_d() +
  theme_doug() +
  transition_time(roundedpop) +
  labs(title = "Tanzania 2007 - 2010, population density 0 - 60",
       subtitle = 'Population Density: **{as.integer(frame_time)}** people per km2', 
       y = '\nDensity\n',
       x = "\nAbove Ground Carbon (MgC km-2)\n",
       caption = "McNicol et al. 2018, \n Worldpop.org") +
  theme(plot.subtitle = element_markdown()) 
fouryears


fouryears_freqpoly <- tan_longer %>% 
  filter(roundedpop %in% c(1:60)) %>%  
  ggplot() +
  geom_freqpoly(aes(x = MgC_km, color=year), 
                binwidth = 1, size=1.3, alpha=0.6) +
  scale_colour_viridis_d() +
  theme_doug()

fouryears_freqpoly +
  transition_time(roundedpop) +
  labs(title = "Tanzania 2007 - 2010, population density 0 - 60",
       subtitle = 'Population Density: **{as.integer(frame_time)}** people per km2', 
       y = '
       total area (km2 per km2)',
       x = "\n Above Ground Carbon (MgC km-2) \n",
       caption = "McNicol et al. 2018, \n Worldpop.org") +
  theme(plot.subtitle = element_markdown()) +
  view_follow(fixed_x = T)








#anim_save("fouryear.gif")

a_gif <- animate(cumareaplot, width = 600, height = 600,  renderer = gifski_renderer("images/line_animation.gif"))
b_gif <- animate(fouryears, width = 600, height = 600,  renderer = gifski_renderer("images/den_animation.gif"))


a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)


new_gif <- image_append(c(a_mgif[1], b_mgif[1]), stack = TRUE)
for(i in 2:250){
  combined <- image_append(c(a_mgif[i], b_mgif[i]), stack = TRUE)
  new_gif <- c(new_gif, combined)
}






new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif





#animate(a, renderer = gifski_renderer())

#anim_save(filename = a,path = "images/20april_anim_den07.png")


text_df <- data.frame(
  x = 60,
  y = 0.05,
  label =  "As the human population density increases, 
           the distribution of AGC occupying the same area
           shifts from broadly high carbon to very low carbon."
)



ab <- tan_more %>% 
  filter(roundedpop %in% c(1:100)) %>%  
  ggplot(aes(x = agc2007, fill = factor(roundedpop))) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  scale_fill_viridis_d() +
  theme(legend.position = "none") +
  geom_richtext(data = text_df, aes(x = x, y = y, 
                                    label = label), size = 4) +
  geom_curve(
    aes(x = 55, y = 0.042, xend = 27, yend = 0.035), 
    curvature = -.3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = 90, y = 0.042, xend = 5, yend = 0.075, 
    curvature = .7, arrow = arrow(length = unit(2, "mm")))


ab + 
  transition_time(roundedpop) +
  labs(title = "Tanzania, 2007"  , 
       subtitle = 'Population Density: {as.integer(frame_time)} people per km2', 
       y = 'Density', 
       x = "Aboveground Carbon (MgC per km2)") #+
#  theme(plot.subtitle = element_text(colour = roundedpop))




tan_more %>% 
  filter(pop_den_lvl %in% c(1,2, 3, 4)) %>% 
  ggplot(aes(x = agc2007, y = pop_den_lvl, fill = factor(stat(quantile)))) + 
  #geom_density_ridges(fill = "#00AFBB", rel_min_height = 0.001, scale = 4, alpha = 0.7) +
  theme_classic() +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE) +
  scale_fill_viridis_d(name = "Quartiles")

#anim_save("output.gif")













#(den_long07 <- tan_longer %>%  
#    select(pop_den_lvl = 2) %>%
ggplot() + 
  geom_density(tan_longer, mapping = aes(x = MgC_km, color = year), na.rm = T) +
  scale_fill_viridis_c("Year", labels = c("07", "08", "09", "10")) +
  theme_classic()










(tan_stack %>% 
    na.omit(pop_den_lvl) %>%
    ggdensity(tan_stack, x = "agc2007", add = "mean", rug = TRUE, color = "pop_den_lvl", palette = c("#00AFBB", "#E7B800")))



x <- ggplot() + # 767,431
  geom_density(tanagc07_df, mapping = aes(agc2007), na.rm = T) +
  labs(x = "AGC 2007 (Gt C / km)",
       y = "Frequency (km2)", 
       caption = "binwidth = 1 \nSource: McNicol et al (2018))") +
  theme_classic()


(den07 <- tan_stack %>%  
    na.omit(pop_den_lvl) %>%
    ggplot(aes(x = agc2007, fill = pop_den_lvl))+
    geom_density(alpha = 0.4) +
    scale_fill_viridis_d("Population density", labels = c("Zero", "Sparse (<15 km2)", "Medium (15-50 km2)", "High (>50 km2)")) +
    theme_classic() +
    labs(x = "AGC in 2007 (GgC / km)"))
#ggsave("images/31mar_dens07.png", width = 6, height = 6, dpi = 500, scale = 1)

(den08 <- tan_stack %>% 
    na.omit(pop_den_lvl) %>%
    ggplot(aes(x = agc2008, fill = pop_den_lvl))+
    geom_density(alpha = 0.4) +
    scale_fill_viridis_d("Population density", labels = c("Zero", "Sparse (<15 km2)", "Medium (15-50 km2)", "High (>50 km2)")) +
    theme_classic() +
    labs(x = "AGC in 2008 (GgC / km)"))

(den09 <- tan_stack %>% 
    na.omit(pop_den_lvl) %>%
    ggplot(aes(x = agc2009, fill = pop_den_lvl))+
    geom_density(alpha = 0.4) +
    scale_fill_viridis_d("Population density", labels = c("Zero", "Sparse (<15 km2)", "Medium (15-50 km2)", "High (>50 km2)")) +
    theme_classic() +
    labs(x = "AGC in 2009 (GgC / km)"))

(den10 <- tan_stack %>% 
    na.omit(pop_den_lvl) %>%
    ggplot(aes(x = agc2010, fill = pop_den_lvl))+
    geom_density(alpha = 0.4) +
    scale_fill_viridis_d("Population density", labels = c("Zero", "Sparse (<15 km2)", "Medium (15-50 km2)", "High (>50 km2)")) +
    theme_classic() +
    labs(x = "AGC in 2010 (GgC / km)"))

ggarrange(den07, den08, den09, den10, ncol = 2, nrow = 2, common.legend = TRUE)
ggsave("images/31mar_tan_den_arrange.png", width = 6, height = 6, dpi = 500, scale = 1)

