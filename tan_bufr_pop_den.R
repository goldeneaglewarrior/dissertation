#tanzania buffer pop density
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


## country ----
country_outline <-  map_data("world", regions = "Tanzania")

tan_roads <- st_read("data/roads/Tanzania_Roads.shp")


ggplot() +
        geom_tile(agc_total_hi, mapping = aes(x=x,y=y, fill = factor(buffer))) +
        geom_tile(agc_total_lo, mapping = aes(x=x,y=y, fill = factor(buffer))) + #no buffer
        geom_polygon(data = country_outline, 
                     aes(x=long, y = lat, group = group), 
                     fill = NA, colour = "black") +
        theme_classic() 


# z column

agc_total_hi <- agc_total_hi %>% 
    unite("z", x:y, remove = FALSE)

agc_total_lo <- agc_total_lo %>% 
    unite("z", x:y, remove = FALSE)

agc_rural <- agc_total_hi %>% 
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

agc_join <- merge(hi_agc_bio, hi_agc_perc, by=c("z","year", "buffer"))
agc_join <- merge(agc_join, hi_agc_flux, by=c("z","year", "buffer"))



lo_agc_bio <- agc_total_lo %>% 
        dplyr::select(3:7) %>% 
        pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
        separate(year, c(NA, "year"), sep = "c" ) 






hi_agc_perc <- agc_total_hi %>% 
        dplyr::select(3,7:10) %>% 
        pivot_longer(!agc2007 & !buffer, names_to = "year", values_to = "perc_change") %>% 
        separate(year, c(NA, "year"), sep = "_" ) 

hi_agc_perc$year <- as.numeric(hi_agc_perc$year)

lo_agc_perc <- agc_total_lo %>% 
        dplyr::select(3,7:10) %>% 
        pivot_longer(!agc2007 & !buffer, names_to = "year", values_to = "perc_change") %>% 
        separate(year, c(NA, "year"), sep = "_" ) 

lo_agc_perc$year <- as.numeric(lo_agc_perc$year)


hi_agc_perc <- hi_agc_perc %>% 
    mutate(lose_type = case_when(perc_change < -20 ~ "deforest",
                                 perc_change >= -20 & perc_change < 0~ "degrade"))


hi_agc_perc %>% 
    filter(agc2007 >20) %>% 
    filter(lose_type == "deforest") %>% 
    group_by(perc_change) %>% 
    summary()

summary(hi_agc_perc)

# GOOD PLOT
lo_agc_perc %>% 
    filter(agc2007 >25) %>% 
    filter(perc_change < 0) %>%
    ggplot(aes(x = perc_change, group = factor(year), col = factor(year))) +
    geom_density() +
    facet_wrap(~buffer, ncol = 1, strip.position="left") +
    geom_vline(xintercept = -20, linetype="dotted", 
               color = "blue", size=0.5) +
    theme_classic() 










lo_agc_perc %>% 
        filter(perc_change < 0) %>% 
        filter(agc2007 > 15) %>% 
        ggplot(aes(x = factor(year), y = perc_change)) +
        geom_boxplot(aes(fill = factor(buffer))) +
        scale_fill_manual(name = "Distance from\nurban area",
                          values = c("#EE3700", "#E1AF00", "#EBCC2A"),
                          labels = c("< 20km ", "20 - 50km", "50 - 80km")) + # red, yellow, blue
        theme_classic() +
        labs(title = "Tanzania annual AGB loses around urban area",
             subtitle = "Degradation defined here as >20% annual lose from pixel \nthat was forested in 2007 (> 20 AGC value)",
             caption = "Source: McNicol et al. 2018",
             x = "Year",
             y = "Annual biomass loses (%)") +
        scale_x_discrete(labels=c("2008" = "'07 to '08", "2009" = "'08 to '09",
                                  "2010" = "'09 to '10")) +
        geom_hline(yintercept=-20, linetype="dashed", color = "blue") +
        scale_y_reverse()





lo_agc_perc %>% 
        dplyr::filter(perc_change < 0) %>% 
        ggplot(aes(x = factor(buffer), y = perc_change)) +
        geom_violin() 


hi_agc_perc %>% 
        dplyr::filter(perc_change < 0) %>% 
        ggplot(aes(x = factor(buffer), y = perc_change)) +
        geom_violin() 

hi_agc_perc %>% 
        dplyr::filter(perc_change < 0) %>% 
        ggplot(aes(y = perc_change, x =factor(year))) +
        geom_violin(aes(fill = factor(buffer)))



hi_agc_perc <- hi_agc_perc %>% 
        mutate(simp_type = case_when(
            perc_change <= 0 ~ "loss",
            perc_change >= 0 ~ "gain"
        ))


lo_agc_perc <- lo_agc_perc %>% 
        mutate(simp_type = case_when(
                perc_change <= 0 ~ "loss",
                perc_change >= 0 ~ "gain"
        ))


los_lo_agc_perc <- lo_agc_perc %>%
        filter(perc_change < 0) %>% 
        mutate(loss_type = case_when(
                perc_change < -20 ~ "Deforestation",
                perc_change >= -20 ~ "Degradation"
        ))


hi_agc_perc %>%
        filter(!is.na(simp_type)) %>% 
        filter(agc2007 > 20) %>% 
        ggplot(aes(factor(buffer),fill = simp_type)) +
        geom_bar(position = "fill") +
        facet_wrap(~factor(year)) +
        theme_classic()

lo_agc_perc %>%
        filter(!is.na(simp_type)) %>% 
        filter(agc2007 > 20) %>% 
        ggplot(aes(factor(buffer),fill = simp_type)) +
        geom_bar(position = "fill") +
        facet_wrap(~factor(year)) +
        theme_classic()




hi_agc_perc %>%
        filter(!is.na(simp_type)) %>% 
        ggplot(aes(factor(year),fill = simp_type)) +
        geom_bar() +
        facet_wrap(~factor(buffer)) 




los_lo_agc_perc <- lo_agc_perc %>%
        filter(perc_change < 0) %>% 
        mutate(loss_type = case_when(
                perc_change < -20 ~ "Deforestation",
                perc_change >= -20 ~ "Degradation"
        ))


los_lo_agc_perc %>%
    #    filter(!is.na(simp_type)) %>% 
        filter(agc2007 > 20) %>% 
        ggplot(aes(factor(year),fill = loss_type)) +
        geom_bar(position = "fill") +
        facet_wrap(~factor(buffer)) 


stats_hi <- summarySE(hi_agc_bio, measurevar="biomass", groupvars=c("buffer","year"), na.rm = T) # only worked with na.rm...
stats_lo <- summarySE(lo_agc_bio, measurevar="biomass", groupvars=c("buffer","year"), na.rm = T) # only worked with na.rm...


# agc per year error bar

stats_hi %>% 
        ggplot(aes(x = factor(year), y = biomass)) +
        geom_bar(stat="identity") +
        facet_wrap(~factor(buffer)) +
        geom_errorbar(aes(ymin=biomass-ci, ymax=biomass+ci), width=.2)

stats_lo %>% 
        ggplot(aes(x = factor(year), y = biomass)) +
        geom_bar(stat="identity") +
        facet_wrap(~factor(buffer)) +
        geom_errorbar(aes(ymin=biomass-ci, ymax=biomass+ci), width=.2)


