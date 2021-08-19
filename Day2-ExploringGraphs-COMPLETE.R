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

# library(dplyr)
# library(tidyr)
library(lubridate)

library(tidyverse)



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

plot(x = beit.bridge.clean$Year, y = beit.bridge.clean$Level..m.,
     type = "o")


boxplot(beit.bridge.clean$Flow..cumec.)


hist(beit.bridge.clean$Flow..cumec.,
     breaks = 20)


barplot(beit.bridge.clean$Level..m.,ylab = 'Level')




plot(x = mutale.clean$Date_Time, y = mutale.clean$WaterTemp_degC,
     type = "l")


hist(mutale.clean$WaterTemp_degC)


boxplot(mutale.clean$WaterTemp_degC ~ month(mutale.clean$Date_Time))



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

ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Level..m.)) +
  geom_point() +
  geom_line()

# add some general customizations (colors, axes labels)

ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Level..m.)) +
  geom_point(color = "blue", size = 3) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "Maximum River Stage (m above)",
       title = "Maximum Yearly River Stage at Beit Bridge") +
  geom_hline(yintercept = 4, color='red')


### PRACTICE
# Create a plot of annual maximum flow over time, with
# appropriate title and axis labels,
# points connected by lines,
# BLUE color for points and lines,
# DOTTED lines (HINT: "linetype" argument -- see "help(lines)"),
# OPEN TRIANGLE point symbols (HINT: "shape" argument -- see "help(points)")

ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Flow..cumec.)) +
  geom_point(color = "blue", size = 3, shape = 2) +
  geom_line(color = "blue", linetype = 3) +
  labs(x = "Year", y = "Maximum Flow (m3/second)",
       title = "Maximum Yearly Flow at Beit Bridge")



###########################
### Customizing ggplot2 ###
###########################

# add linear model and trend/regression line

ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Level..m.),) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "red") +
  labs(x = "Year", y = "Maximum River Stage (m above)",
       title = "Maximum Yearly River Stage at Beit Bridge") +
  geom_smooth(method = "lm", se = TRUE, color='orange')


### repeat above for bottom water temperature with trend line

ggplot(data = beit.bridge.clean, mapping = aes(x = Year, y = Flow..cumec.),) +
  geom_point(color = "blue", size = 3, shape = 2) +
  geom_line(color = "blue", linetype = 3) +
  labs(x = "Year", y = "Maximum Flow (m3/second)",
       title = "Maximum Yearly Flow at Beit Bridge") +
  geom_smooth(method = "loess")



# Can we add multiple lines to the same plot?
# Let's look at DAILY air temperature and water temperature at Mutale Weir

head(mutale.clean)

daily.airtemp.watertemp <- mutale.clean %>%
  mutate(DateOnly = as.Date(Date_Time)) %>% 
  group_by(DateOnly) %>%
  summarize(DailyAirTemp_degC = mean(AirTemp_degC, na.rm = T),
            DailyWaterTemp_degC = mean(WaterTemp_degC, na.rm = T))
  

ggplot() +
  geom_point(data = daily.airtemp.watertemp, mapping = aes(x = DateOnly, y = DailyAirTemp_degC)) +
  geom_point(data = daily.airtemp.watertemp, mapping = aes(x = DateOnly, y = DailyWaterTemp_degC),
             color = "blue") +
  labs(x = "Date", y = "Temperature (C)", title = "Daily Air & Water Temperature at Mutale Weir")



## x-y scatterplot:

ggplot() +
  geom_point(data = daily.airtemp.watertemp, mapping = aes(x = DailyAirTemp_degC, y = DailyWaterTemp_degC)) +
  geom_smooth(data = daily.airtemp.watertemp, mapping = aes(x = DailyAirTemp_degC, y = DailyWaterTemp_degC),
              method = "lm") +
  labs(x = "Air Temperature (C)", y = "Water Temperature (C)", 
       title = "Daily Air vs. Water Temperature at Mutale Weir")




daily.temps.long <- daily.airtemp.watertemp %>%
  pivot_longer(cols = c(DailyAirTemp_degC, DailyWaterTemp_degC),
               names_to = "Variable",
               values_to = "Temperature")


ggplot(data = daily.temps.long, mapping = aes(x = DateOnly, y = Temperature,
                                              color = Variable)) +
  geom_point() +
  geom_line() +
  labs(x = "Date", y = "Temperature (C)", title = "Daily Air & Water Temperature at Mutale Weir")+
  scale_color_manual(values = c("orange", "blue"), labels = c("Air Temp.", "Water Temp."))



## FACETING:

ggplot() +
  geom_point(data = daily.temps.long, mapping = aes(x = DateOnly, y = Temperature)) +
  geom_line(data = daily.temps.long, mapping = aes(x = DateOnly, y = Temperature)) +
  labs(x = "Date", y = "Temperature (C)", title = "Daily Air & Water Temperature at Mutale Weir") +
  scale_color_manual(values = c("black", "blue"), labels = c("Air Temp.", "Water Temp.")) +
  facet_wrap(~Variable)




# histograms

ggplot() +
  geom_histogram(data = beit.bridge.clean, mapping = aes(Level..m.))


# adjust binwidth, and add customizations (axes labels, color)

ggplot() +
  geom_histogram(data = beit.bridge.clean, mapping = aes(Level..m.),
                 binwidth = 0.25, fill = "orange", color = "black") +
  labs(x = "Maximum Annual River Level (m)", y = "Frequency",
       title = "Maximum Annual River Level, 1993-2020")



# boxplots

ggplot() +
  geom_boxplot(data = daily.temps.long, mapping = aes(x = Variable, y = Temperature))



# customize (axes labels, color)

ggplot() +
  geom_boxplot(data = daily.temps.long, mapping = aes(x = Variable, y = Temperature)) +
  labs(x = NULL, y = "Temperature (C)", title = "Daily Air & Water Temperature at Mutale Weir") +
  theme_classic()



# barplots

MonthlyTurbidity <- mutale.clean %>%
  mutate(Date_Only = as.Date(Date_Time),
         Month = month(Date_Time)) %>%
  filter(!is.na(Turbidity_NTU)) %>%
  group_by(Date_Only, Month) %>%
  summarize(DailyTurbidity = sum(Turbidity_NTU, na.rm = T)) %>%
  ungroup() %>%
  group_by(Month) %>%
  summarize(MeanTurbidity = mean(DailyTurbidity),
            StErrTurbidity = sd(DailyTurbidity) / sqrt(NROW(DailyTurbidity)))

ggplot() +
  geom_col(data = MonthlyTurbidity, mapping = aes(x = Month, y = MeanTurbidity))


# add error bars

ggplot() +
  geom_col(data = MonthlyTurbidity, mapping = aes(x = Month, y = MeanTurbidity)) +
  geom_errorbar(data = MonthlyTurbidity, mapping = aes(x = Month, 
                                                    ymin = MeanTurbidity - StErrTurbidity,
                                                    ymax = MeanTurbidity + StErrTurbidity),
                width = 0.25,color='red')



# other customizations (color, labels, remove legend)

ggplot() +
  geom_col(data = MonthlyTurbidity, mapping = aes(x = Month, y = MeanTurbidity)) +
  geom_errorbar(data = MonthlyTurbidity, mapping = aes(x = Month, 
                                                    ymin = MeanTurbidity - StErrTurbidity,
                                                    ymax = MeanTurbidity + StErrTurbidity),
                width = 0.5) +
  labs(x = "Month", y = "Average Turbidity (NTU)", 
       title = "Average Daily Turbidity at Mutale Weir") +
  theme_bw()


### PRACTICE
# Create a barplot of average monthly WATER temperature,
# starting from the "daily.airtemp.watertemp" object.
# Include error bars of +/- 1 standard deviation,
# and appropriate names and axis labels


MonthlyAirTemp <- daily.airtemp.watertemp %>%
  mutate(Month = month(DateOnly)) %>%
  group_by(Month) %>%
  summarize(MeanWaterTemp = mean(DailyWaterTemp_degC, na.rm = T),
            StDevWaterTemp = sd(DailyWaterTemp_degC, na.rm = T))


ggplot() +
  geom_col(data = MonthlyAirTemp, mapping = aes(x = Month, y = MeanWaterTemp),
           position = "dodge")+
  geom_errorbar(data = MonthlyAirTemp, mapping = aes(x = Month, 
                                              ymin = MeanWaterTemp - StDevWaterTemp,
                                              ymax = MeanWaterTemp + StDevWaterTemp),
                width = 0.5) +
  labs(x = "Month", y = "Average Monthly Water Temperature (C)", 
       title = "Average Monthly Water Temperatures at Mutale Weir") 



##########################
### PRACTICE EXERCISES ###
##########################

# using these data, make three plots:
# 1) Boxplot of total daily precipitation by month (Mutale Weir)
# 2) Line plots with FACETING of DAILY precipitation and river stage (Mutale Weir)
# 3) Scatterplot of precipitation vs. river stage with trend line (Mutale Weir)

# REMEMBER YOU MIGHT HAVE TO CLEAN SOME DATA BEFORE PLOTTING!

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

setwd("G:/My Drive/rachel-PC/Miami-OH/R Workshops/US-AID 2021/Prepped Materials (NOT SHARED)")

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



# 1: Boxplot of total daily precipitation by month (Mutale Weir)

MonthlyPrecip <- mutale.clean %>%
  mutate(DateOnly = as.Date(Date_Time),
         Month = month(Date_Time, label = T)) %>%
  group_by(DateOnly, Month) %>%
  summarize(DailyPrecip = sum(Precipitation_mm, na.rm = T))

ggplot() +
  geom_boxplot(data = MonthlyPrecip, aes(x = Month, y = DailyPrecip)) +
  labs(x = "Month", x = "Daily Total Preciptiation (mm)")



# 2: Line plots with FACETING of DAILY precipitation and river stage (Mutale Weir)

daily.precip.riverstage.long <- mutale.clean %>%
  mutate(DateOnly = as.Date(Date_Time)) %>%
  group_by(DateOnly) %>%
  summarize(DailyPrecip = sum(Precipitation_mm, na.rm = T),
            MeanRiverStage = mean(RiverStage_m, na.rm = T)) %>% 
  pivot_longer(cols = c(DailyPrecip, MeanRiverStage),
               names_to = "Variable",
               values_to = "Value")


ggplot() +
  geom_line(data = daily.precip.riverstage.long,
            aes(x = DateOnly, y = Value, color = Variable)) +
  facet_wrap(~Variable, scales = "free_y", nrow = 2) +
  scale_color_manual(values = c("black", "blue"),
                     labels = c("Precipitation (mm/day)", "River Stage (m)"))



# 3: Scatterplot of precipitation vs. river stage with trend line (Mutale Weir)

daily.precip.riverstage.wide <- daily.precip.riverstage.long %>%
  pivot_wider(names_from = "Variable",
              values_from = "Value")


ggplot() +
  geom_point(data = daily.precip.riverstage.wide, aes(x = DailyPrecip, y = MeanRiverStage)) +
  geom_smooth(data = daily.precip.riverstage.wide, aes(x = DailyPrecip, y = MeanRiverStage),
              method = "lm") +
  labs(x = "Precipitation (mm/day)", y = "Mean River Stage (m)")



