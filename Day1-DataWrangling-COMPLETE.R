### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### orginal code created by Rachel Pilla

### DAY 2:  Data Wrangling ###


library(dplyr)
library(tidyr)

#OR

library(tidyverse)


setwd("")

beit.bridge <- read.csv("A7H008YRPK.CSV", skip = 7)

beit.bridge.clean <- beit.bridge[1:28, ]


#########################
### Manipulating data ###
#########################

# dplyr: four main functions and pipe tool
# data frame name is ALWAYS the first argument for these if using alone

?select
?filter
?mutate
?summarize

## choose specific column(s) by name

beit.bridge.level <- select(beit.bridge.clean, Year, Level..m.)
beit.bridge.level


## choose row(s) by condition

Year2000 <- filter(beit.bridge.clean, Year == 2000)
Year2000

#operators for filter
# greater than >
# greater than or equal to >=
# less than <
# less than or equal to <=
# equal ==
# not equal to !=
# and &
# or |
# to filter NA values, use is.na()

Year2010_2015 <- filter(beit.bridge.clean, Year >= 2010 & Year <= 2015)
Year2010_2015

YearExtremes <- filter(beit.bridge.clean, Level..m. < 1 | Level..m. > 3)
YearExtremes

beit.bridge.filterNA <- filter(beit.bridge, !is.na(Date))

# create new column(s)

beit.bridge.LperS <- mutate(beit.bridge.clean, Flow_LperS = Flow..cumec.*1000)
beit.bridge.LperS


# summarize (must become fewer rows than original)
MeanHydrol <- summarize(beit.bridge.LperS, 
                       MeanLevel_m = mean(Level..m.),
                       MeanFlow_LperS = mean(Flow_LperS))
MeanHydrol



# pipe tool for multiple steps at once
# do NOT need to name data frame if using this, since it will automatically start with the 
# data frame remaining from the previous line of code

# Let's say we wanted to find the maximum Level from the past 10 years only

RecentMaxLevel <- beit.bridge.clean %>% 
  filter(Year > 2010) %>% 
  select(Year, Level..m.) %>% 
  summarize(MaxLevel = max(Level..m.))

RecentMaxLevel


# PRACTICE
# filter the Years when the Level was greater than 2 m 
# AND the flow was greater than 1000 m3/s

HighLevelFlowYears <- beit.bridge.clean %>%
  filter(Level..m. > 2,
         Flow..cumec. > 1000) %>%
  select(Year)



# PRACTICE
# using the dplyr tools, create a new object
# that gives the MIN flow in L/s before 2000

MinFlow_pre2000 <- beit.bridge.clean %>%
  filter(Year < 2000) %>%
  mutate(Flow_LperS = Flow..cumec.*1000) %>% 
  summarize(MinFlow = min(Flow_LperS),
            MaxFlow = max(Flow_LperS),
            MeanFlow = mean(Flow_LperS))
MinFlow_pre2000



# DATA FORMATTING -- wide vs. long
# reformatting from wide to long using "pivot_longer"

beit.bridge.long <- beit.bridge.clean %>%
  mutate(Flow_LperS = Flow..cumec.*1000) %>% 
  select(Year, Level..m., Flow..cumec., Flow_LperS)  %>% 
  pivot_longer(cols = c(Level..m., Flow..cumec., Flow_LperS), 
               names_to = "Variable", 
               values_to = "Value")

beit.bridge.long



beit.bridge.wide <- beit.bridge.long %>%
  pivot_wider(names_from = Variable, 
              values_from = Value)

beit.bridge.wide



##########################
### DATES & TIMES IN R ###
##########################

str(beit.bridge.clean)

library(lubridate)

?lubridate
?ymd

ymd(beit.bridge.clean$Date)

hm(beit.bridge.clean$Time)

paste(beit.bridge.clean$Date,beit.bridge.clean$Time)

beit.bridge.datetime <- beit.bridge.clean %>%
  mutate(Date = ymd(beit.bridge.clean$Date),
         Date_Time = ymd_hm(paste(Date,Time)))



## Work with another new data set:  "Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv"

mutale <- read.csv("Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv")

head(mutale)
str(mutale)

# cleaning steps:
# 1) create DateTime column using lubridate
# 2) convert "-9999" and "-8888" to NA
# 3) remove columns Year through Minutes
# 4) rename columns to be clear and include units


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

str(mutale.clean)


# another example of converting wide to LONG format:

mutale.long <- mutale.clean %>%
  pivot_longer(cols=Precipitation_mm:Turbidity_NTU,
               names_to = "Variable",
               values_to = "Value")

str(mutale.long)



# GROUPING data before summarizing

mutale.summary <- mutale.long %>%
  group_by(Variable) %>%
  summarize(MeanValue = mean(Value, na.rm = TRUE))

mutale.summary



##########################
### PRACTICE EXERCISES ###
##########################

## EXPLORE THE MUTALE WEIR DATA WE JUST CLEANED
## using DPLYR functions

library(tidyr)
library(dplyr)

setwd("")

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

mutale.long <- mutale.clean %>%
  pivot_longer(cols=Precipitation_mm:Turbidity_NTU,
               names_to = "Variable",
               values_to = "Value")



## 1) What was the range (minimum, maximum, and max-min difference) of 
##    each weather and hydrology variable measured?

mutale.range <- mutale.long %>%
  group_by(Variable) %>%
  summarize(MinValue = min(Value, na.rm = T),
            MaxValue = max(Value, na.rm = T),
            Range = MaxValue - MinValue)

mutale.range <- mutale.long %>%
  group_by(Variable) %>%
  summarize(MinValue = min(Value, na.rm = T),
            MaxValue = max(Value, na.rm = T)) %>% 
  mutate(Range = MaxValue - MinValue)

mutale.range



## 2) Find the date when the river stage was the highest and the lowest.

HighLowRiverStage <- mutale.clean %>%
  filter(RiverStage_m == max(RiverStage_m, na.rm = T) |
           RiverStage_m == min(RiverStage_m, na.rm = T)) %>%
  select(Date_Time, RiverStage_m)

HighLowRiverStage



## 3) Calculate the total cumulative precipitation for each month of the data set.

MonthlyPrecip <- mutale.clean %>%
  mutate(Year = year(Date_Time),
         Month = month(Date_Time)) %>% 
  group_by(Year, Month) %>%
  summarize(TotalPrecip = sum(Precipitation_mm, na.rm = T))

MonthlyPrecip

