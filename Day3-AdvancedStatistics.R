### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### original code provided by Rachel Pilla


### DAY 3:  Advanced Statistics & Practice ###


## load libraries and data for today

library(dplyr)
library(tidyr)
library(lubridate)


setwd("")

beit.bridge <- read.csv("A7H008YRPK.CSV", skip = 7)

beit.bridge.clean <- beit.bridge[1:28, ] %>%
  mutate(Year = as.numeric(Year))



mutale <- read.csv("Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv")

mutale.clean <- mutale %>%
  mutate(Date_Time = ymd_hm(paste(YEAR, MONT, DAYN, HOUR, MINU))) %>%
  na_if(-9999) %>%
  na_if(-8888) %>%
  na_if(-7777) %>%
  select(-YEAR, -MONT, -DAYN, -HOUR, -MINU) %>%
  rename(Precipitation_mm = PRCP,
         AirTemp_degC = TEMP,
         RelHumidity_percent = RHMD,
         SolarRad_W_m2 = SRAD,
         AirPressure_kPa = APRS,
         WindSpeed_m_s = WSPD,
         WindDir_deg = WDIR,
         RiverStage_m = RIVS,
         WaterTemp_degC = WTMP,
         Conductivity_uS_cm = COND,
         Turbidity_NTU = TRBD)


drainage <- read.csv("Sites-4DrainageRegions.csv")

#################################
### Non-parametric statistics ###
#################################

## non-parametric correlations

cor(x = beit.bridge.clean$Level..m, y = beit.bridge.clean$Flow..cumec.,
    use = "pairwise.complete.obs", method = "spearman")

cor(x = beit.bridge.clean$Level..m, y = beit.bridge.clean$Flow..cumec.,
    use = "pairwise.complete.obs", method = "kendall")


## non-parametric equivalent to 1-sample t-test

wilcox.test(mutale.clean$RiverStage_m)


## non-parametric equivalent to 2-sample t-test

wilcox.test(x = mutale.clean$AirTemp_degC, y = mutale.clean$WaterTemp_degC)



## non-parametric equivalent of ANOVA

riverstage.monthly <- mutale.clean %>%
  mutate(Quarter = factor(quarter(Date_Time)))

riverstage.kruskal <- kruskal.test(RiverStage_m ~ Quarter, data = riverstage.monthly)
riverstage.kruskal


## practice with catchment area across the four drainage regions

drainage.kruskal <- kruskal.test(CatchmentArea_km2 ~ DrainageRegion, data = drainage)
drainage.kruskal


## non-parametric temporal trend test

library(trend)

mk.test(beit.bridge.clean$Level..m.)

sens.slope(beit.bridge.clean$Level..m.)


## practice for Flow data trends

mk.test(beit.bridge.clean$Flow..cumec.)

sens.slope(beit.bridge.clean$Flow..cumec.)


########################
### Making maps in R ###
########################

library(ggplot2)

southafrica.map=map_data("world", region = "South Africa")

head(southafrica.map)


## map of four drainage regions in South Africa

regional.map <- ggplot() +
  geom_polygon(data = southafrica.map, aes(x = long, y = lat, group = group), fill = "grey95") +
  geom_path(data = southafrica.map, aes(x = long, y = lat, group = group), color = "grey50") +
  coord_map(xlim = c(15, 35), ylim = c(-35, -22)) +
  geom_point(data = drainage, aes(x = Longitude, y = Latitude, color = DrainageRegion),
             alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()



## map of catchment size for each site across the four drainage regions

ggplot() +
  geom_density(data = drainage, aes(CatchmentArea_km2))

ggplot() +
  geom_density(data = drainage, aes(log10(CatchmentArea_km2)))


## https://colorbrewer2.org/


catchmentarea.map <- ggplot() +
  geom_polygon(data = southafrica.map, aes(x = long, y = lat, group = group), fill = "grey25") +
  geom_path(data = southafrica.map, aes(x = long, y = lat, group = group), color = "grey50") +
  coord_map(xlim = c(15, 35), ylim = c(-35, -22)) +
  geom_point(data = drainage, aes(x = Longitude, y = Latitude, color = CatchmentArea_km2)) +
  labs(x = "Longitude", y = "Latitude") +
  scale_color_distiller(trans = "log10", na.value = NA,
                        palette = "RdYlBu", direction = 1) +
  theme_bw()



## saving figures and using panels

library(ggpubr)

ggarrange(regional.map, catchmentarea.map,
          nrow = 1, ncol = 2, 
          labels = c("A", "B"), align = "hv")


ggsave(filename = "DrainageRegionsMap.jpeg", 
       plot = regional.map, height = 10, width = 15, units = "cm",
       dpi = 1200)



###############################################
### Additional resources, tips, and wrap-up ###
###############################################

# See PPT

############################
### Tips for teaching R  ###
############################

# See PPT

###########################
### Practice exercises  ###
###########################

# See shared Google folder:
# "PracticeExercises-QUESTIONS_RWorkshopLimpopo.docx"
# and "PracticeExercises-ANSWERS_RWorkshopLimpopo.R"
