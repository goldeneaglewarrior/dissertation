## half violin plots
# 11/05/21
# We will use a function by Ben Marwick
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

## libraries ----
library(sp)
library(raster)
library(tidyverse)
#library(rgeos) # look at in the future
#library(maptools)  ## For wrld_simpl
library(rnaturalearthdata)
library(rnaturalearth)
library(gganimate)
library(gifski) # to make gifs work for me
library(ggridges)
library(ggtext)
library(wesanderson)


theme_niwot <- function(){
  theme_bw() +
    theme(text = element_text(family = "Helvetica Light"),
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 18),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
          plot.title = element_text(size = 18, vjust = 1, hjust = 0),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.95, 0.15),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 2, linetype = "blank"))
}






## load data ----
africa_07 <- raster('Z:/mcnicol_agc_layers/1km/mcnicol_AGC2007_1km.tif')
wpop10 <- raster('Z:/Worldpop/global_mosaics/ppp_2010_1km_Aggregated.tif')

# Tanzania spdf map
tan_SPDF <- ne_countries(scale = 50, #1:50 million scale
                         country = "United Republic of Tanzania")

## Crop to Tanzania ----
#agc
tanagc07 <- crop(africa_07, extent(tan_SPDF))
tanagc07 <- mask(tanagc07, tan_SPDF)

#pop
tanpop10 <- crop(wpop10, extent(tan_SPDF))
tanpop10 <- mask(tanpop10, tan_SPDF)



# project the pop raster to agc resoltuon
tanpop_new_r <- projectRaster(from = tanpop10, to = tanagc07, method = "ngb") 
# same resolution
# moves to nearest neighbour whilst keeping the z value unchanged (which interpolate would do)

# Stack covariates
rast_stack <- stack(tanpop_new_r, 
                    tanagc07)#,
                   # tanagc08,
                   # tanagc09,
                   # tanagc10)

names(rast_stack) <- c("pop2010", "agc2007")#, "agc2008", "agc2009", "agc2010")

tan_df <- rast_stack %>% 
  as.data.frame(xy = T)
# still has na values




tan_more <-  tan_df %>%  ## testing out other categories
  mutate(
    pop_den_lvl = as.factor(
      case_when(
        pop2010 == 0 ~ 0,
        pop2010 > 0 & pop2010 < 2 ~ 1,
        pop2010 >= 2 & pop2010 < 5 ~ 2,
        pop2010 >= 5 & pop2010 < 15 ~ 3,
        pop2010 >= 15 & pop2010 < 50 ~ 4,
        pop2010 >= 50 ~ 5)
    )
  )



tan_less <- tan_df %>%  ## testing out other categories
  mutate(
    pop_den_lvl = as.factor(
      case_when(
        pop2010 == 0 ~ 0,
        pop2010 > 0 & pop2010 < 2 ~ 1,
        pop2010 >= 2 & pop2010 < 12 ~ 2,
        pop2010 >= 12  ~ 3
        )
    )
  )

tan_more %>% 
  group_by(pop_den_lvl) %>% 
  tally(agc2007, name = "agc 2007") %>% 
  ungroup()


tan_less %>% 
  group_by(pop_den_lvl) %>% 
  tally(agc2007, name = "agc 2007") %>% 
  ungroup()

## density plot and boxplot 2007
tan_more %>% 
  filter(pop_den_lvl %in% c(1:5)) %>% 
  ggplot(aes(x = agc2007, y = -0.005)) + # y = coordinates of boxplot
  geom_boxplot(aes(fill = pop_den_lvl), width =0.01) + 
  geom_density(aes(x = agc2007, fill = pop_den_lvl), inherit.aes = FALSE) +
  facet_grid(pop_den_lvl ~ .) +
  scale_fill_viridis_d("Population density", 
                       labels = c(">0 to 2 per km2",
                                  ">2 to 5 per km2",
                                  ">5 to 15 per km2",
                                  ">15 to 50 per km2",
                                  ">50 per km2")) +
  labs(x = "AGC (MgC per km) 2007", 
       y = "Density") +
  theme_classic() +
  geom_text(aes(x = 75, y = 0.05, 
                label = paste("(n:", n, ")")), # for some reason I can't use =
            data = labels, parse = T, size = 4) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()
  )


## density plot and boxplot 2007
tan_more %>% 
  filter(pop_den_lvl %in% c(1:5)) %>% 
  ggplot(aes(x = pop_den_lvl, y = agc2007, fill = pop_den_lvl)) + 
  geom_flat_violin(position = position_nudge(x = 0.3, y = 0), alpha = 0.8) +
  geom_point(aes(y = agc2007, color = pop_den_lvl),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
  geom_boxplot(width = 0.20, outlier.shape = NA, alpha = 0.8) +
  labs(y = "\nAGC units", x = "Population density (p.p. km2)\n") +
  guides(fill = FALSE, color = FALSE) +
  #scale_y_continuous(limits = c(0, 30)) +
  scale_fill_manual(values =  wes_palette("Zissou1")) +
  scale_colour_manual(values =  wes_palette("Zissou1")) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels=c("1" = ">0 to 2 per km2", 
                            "2" = ">2 to 5 per km2",
                            "3" = ">5 to 15 per km2",
                            "4" = ">15 to 50 per km2",
                            "5" = ">50 per km2"),
                   expand = c(0, 0))

ggsave(filename = "images/pop_den_11_may.png", height = 7, width = 7)

#add n value
  
 
tan_less %>% 
  filter(pop_den_lvl %in% c(1:3)) %>% 
  ggplot(aes(x = pop_den_lvl, y = agc2007, fill = pop_den_lvl)) + 
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(aes(y = agc2007, color = pop_den_lvl),
             position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
  geom_boxplot(width = 0.20, outlier.shape = NA, alpha = 0.8) +
  labs(y = "\nAGC units", x = "Population density (people per km2)\n") +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(0, 80)) +
  scale_fill_manual(values =  wes_palette("Zissou1", 3)) +
  scale_colour_manual(values =  wes_palette("Zissou1", 3)) +
  coord_flip() +
  theme_niwot() +
  scale_x_discrete(labels=c("1" = ">0 to 2", 
                            "2" = ">2 to 12",
                            "3" = ">12"),
                   expand = c(0.1,0.1)) +
  annotate("text",
           x = c(1.5, 2.5, 3.5),
           y = c(60, 60, 60),
           label = c("n = 3.7 x 10^6", "n = 3.8 x 10^6", "n = 4.0 x 10^6"),
           fontface = "italic", size=4)

ggsave(filename = "images/pop_den_12_may.png", height = 7, width = 7)


sessionInfo()
