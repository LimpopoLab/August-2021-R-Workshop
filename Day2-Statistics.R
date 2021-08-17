### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### original code provided by Rachel Pilla


### DAY 2:  Statistics in R ###


############################
### Statistical analyses ###
############################

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


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


##############################
### Descriptive statistics ###
##############################

ggplot(data = beit.bridge.clean, aes(Level..m.)) +
  geom_density()










#####################
### Analyses in R ###
#####################

# correlation of water level and flow, look at plot too





# re-do correlation after removing some outliers, and re-plot






# t-test - one-sided (river stage)






# try another example to compare air temperature vs. water temperature







# ANOVA (1-way)
## Is there a significant difference in river stage across QUARTERS of the year?





## PRACTICE:  
# Are catchment areas different by drainage region?

drainage <- read.csv("Sites-4DrainageRegions.csv")

head(drainage)

unique(drainage$DrainageRegion)








# linear regression of water level trends over time






## PRACTICE with bottom water temperature (or temperature difference)










################################
### Checking for assumptions ###
################################

?shapiro.test
shapiro.test(beit.bridge.clean$Level..m)

qqnorm(beit.bridge.clean$Level..m)
qqline(beit.bridge.clean$Level..m)


## practice looking at normality and residuals with Flow data






# Functions for common data transformations to achieve normality:

sqrt( X )

log10( X )

log10( X + *constant* )

1 / X 

sqrt(asin( X ) )




##########################
### PRACTICE EXERCISES ###
##########################

# This is a built-in data set with R, and has information about
# sleep cycles and times for a range of different mammals

library(dplyr)
library(ggplot2)

?msleep

head(msleep)

# 1:  Are brain weight and body weight correlated? Are they normally-distributed?





## 2: Subset the above data to remove the 2 outliers and re-run your correlation.




# 3:  Conduct an ANOVA and Tukey HSD (if necessary) 
# to test if animals of different vore have different total sleep amounts,
# and include a graph




# 4:  Conduct a linear regression of sleep total as
# function of sleep cycle for mammal data, and include a graph


