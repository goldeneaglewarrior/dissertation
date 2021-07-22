## PA biomass 15july

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


agc_pa_hi <- read.csv(file ="data/agc/agc_pa_hi.csv")
agc_pa_lo <- read.csv(file ="data/agc/agc_pa_lo.csv")
agc_pa_rural <- read.csv(file ="data/agc/agc_pa_rural.csv")



agc_pa_hi <- agc_pa_hi %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_pa_lo <- agc_pa_lo %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)

agc_pa_rural <- agc_pa_rural %>% 
  mutate(perc_2008 = ((agc2008 - agc2007) / agc2007)* 100) %>% 
  mutate(perc_2009 = ((agc2009 - agc2008) / agc2008)* 100) %>% 
  mutate(perc_2010 = ((agc2010 - agc2009) / agc2009)* 100)



hi_pa_bio <- agc_pa_hi %>% 
  dplyr::select(3:7) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 

lo_pa_bio <- agc_pa_lo %>% 
  dplyr::select(3:7) %>% 
  pivot_longer(!buffer, names_to = "year", values_to = "biomass") %>% 
  separate(year, c(NA, "year"), sep = "c" ) 


hi_pa_perc <- agc_pa_hi %>% 
  dplyr::select(3,7:10) %>% 
  pivot_longer(!agc2007 & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

lo_pa_perc <- agc_pa_hi %>% 
  dplyr::select(3,7:10) %>% 
  pivot_longer(!agc2007 & !buffer, names_to = "year", values_to = "perc_change") %>% 
  separate(year, c(NA, "year"), sep = "_" ) 

hi_pa_perc$year <- as.numeric(hi_pa_perc$year)
hi_pa_bio$year <- as.numeric(hi_pa_bio$year)
lo_pa_perc$year <- as.numeric(lo_pa_perc$year)
lo_pa_bio$year <- as.numeric(lo_pa_bio$year)


lo_pa_perc %>% 
  filter(perc_change < 0) %>% 
  #filter(agc2007 > 20) %>% 
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


mapview(agc_pa_hi$agc2007)
