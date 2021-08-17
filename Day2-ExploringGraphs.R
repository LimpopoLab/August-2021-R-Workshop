### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### original code provided by Rachel Pilla


### DAY 2:  Exploring Data with Graphs ###


######################################
### Structure and design of graphs ###
######################################

### PPT:
# - components of a graph
# - understanding/perceiving a graph
# - common types of graphs for 1, 2, 3+ variables
# - keys to good graphs
# - examples of good/bad graphs


# Load libraries and data files from yesterday:

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


##########################
### Base plotting in R ###
##########################



###############################
### Introduction to ggplot2 ###
###############################

### PPT:
# - "Grammar of Graphics" by Hadley Wickham
# - layers of ggplot2
# - geometries, aesthetics, options/customizations
# - building and customizing plots in layers
# - order of the plot layers can be important!
# Recommend keeping the "ggplot()" statement BLANK
# and instead adding the specific data frame(s) in use for each layer


library(ggplot2)


# if using only one dataset
ggplot(data=,aes=(*column names are used here*))+
  geom_***(color,size,shape,...)+
  add_layers(*scaling, formatting, etc.*) +
  adjust_style(*theme, fonts, tick marks, etc.*)


# if using multiple datasets
ggplot() +
  geom_***(data = , mapping = aes(*column names are used here*), 
           *generic/consistent arguments are used here*) +
  add_layers(*scaling, formatting, etc.*) +
  adjust_style(*theme, fonts, tick marks, etc.*)


# simple scatterplot
?geom_point


ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Level..m.)) +
  geom_point()


# add lines to connect points (time series visual)

# add some general customizations (colors, axes labels)





### PRACTICE
# Create a plot of annual maximum flow over time, with
# appropriate title and axis labels,
# points connected by lines,
# BLUE color for points and lines,
# DOTTED lines (HINT: "linetype" argument -- see "help(lines)"),
# OPEN TRIANGLE point symbols (HINT: "shape" argument -- see "help(points)")






###########################
### Customizing ggplot2 ###
###########################

# add linear model and trend/regression line



  

### repeat above for bottom water temperature with trend line







# Can we add multiple lines to the same plot?
# Let's look at DAILY air temperature and water temperature at Mutale Weir

head(mutale.clean)

daily.airtemp.watertemp <- mutale.clean %>%
  mutate(DateOnly = date(Date_Time)) %>%
  group_by(DateOnly) %>%
  summarize(DailyAirTemp_degC = mean(AirTemp_degC, na.rm = T),
            DailyWaterTemp_degC = mean(WaterTemp_degC, na.rm = T))
  





## x-y scatterplot:





daily.temps.long <- daily.airtemp.watertemp %>%
  pivot_longer(cols = c(DailyAirTemp_degC, DailyWaterTemp_degC),
               names_to = "Variable",
               values_to = "Temperature")





## FACETING:






# histograms

ggplot() +
  geom_histogram(data = beit.bridge.clean, mapping = aes(Level..m.))


# adjust binwidth, and add customizations (axes labels, color)




# boxplots

ggplot() +
  geom_boxplot(data = daily.temps.long, mapping = aes(x = Variable, y = Temperature))



# customize (axes labels, color)





# barplots

MonthlyTurbidity <- mutale.clean %>%
  mutate(Date_Only = date(Date_Time),
         Month = month(Date_Time)) %>%
  filter(!is.na(Turbidity_NTU)) %>%
  group_by(Date_Only, Month) %>%
  summarize(DailyTurbidity = sum(Turbidity_NTU, na.rm = T)) %>%
  ungroup() %>%
  group_by(Month) %>%
  summarize(MeanTurbidity = mean(DailyTurbidity),
            StErrTurbidity = sd(DailyTurbidity) / sqrt(NROW(DailyTurbidity)))






# add error bars






# other customizations (color, labels, remove legend)




### PRACTICE
# Create a barplot of average monthly WATER temperature,
# starting from the "daily.airtemp.watertemp" object.
# Include error bars of +/- 1 standard deviation,
# and appropriate names and axis labels









##########################
### PRACTICE EXERCISES ###
##########################

# using these data, make three plots:
# 1) Boxplot of total daily precipitation by month (Mutale Weir)
# 2) Line plots with FACETING of DAILY precipitation and river stage (Mutale Weir)
# 3) Scatterplot of precipitation vs. river stage with trend line (Mutale Weir)

# REMEMBER YOU MIGHT HAVE TO CLEAN SOME DATA BEFORE PLOTTING!





# 1: Boxplot of total daily precipitation by month (Mutale Weir)






# 2: Line plots with FACETING of DAILY precipitation and river stage (Mutale Weir)



# 3: Scatterplot of precipitation vs. river stage with trend line (Mutale Weir)




