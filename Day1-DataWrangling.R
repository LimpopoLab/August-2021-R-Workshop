### Limpopo Resilience Lab Virtual R Workshop
### August 18-20, 2021
### Instructor:  Max Glines
### original code provided by Rachel Pilla



### DAY 1:  Data Wrangling ###


library(dplyr)
library(tidyr)
#OR
library(tidyverse)


setwd("path to folder in which your data is stored")






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





# PRACTICE
# filter the Years when the Level was greater than 2 m 
# AND the flow was greater than 1000 m3/s






# PRACTICE
# using the dplyr tools, create a new object
# that gives the MIN flow in L/s before 2000






# DATA FORMATTING -- wide vs. long
# reformatting from wide to long using "pivot_longer"

beit.bridge.long <- beit.bridge.clean %>%
  select(Year, Level..m., Flow..cumec.) %>%
  pivot_longer(cols = c(Level..m., Flow..cumec.), 
               names_to = "Variable", 
               values_to = "Value")

beit.bridge.long







##########################
### DATES & TIMES IN R ###
##########################

str(beit.bridge.clean)

library(lubridate)

?lubridate
?ymd





## Work with another new data set:  "Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv"

mutale <- read.csv("Limpopo_Resilience_Lab__Mutale_Weir_Dataset.csv")

head(mutale)
str(mutale)
glimpse(mutale)
view(mutale)

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






# GROUPING data before summarizing





##########################
### PRACTICE EXERCISES ###
##########################

## EXPLORE THE MUTALE WEIR DATA WE JUST CLEANED
## using DPLYR functions






## 1) What was the range (minimum, maximum, and max-min difference) of 
##    each weather and hydrology variable measured?





## 2) Find the date when the river stage was the highest and the lowest.





## 3) Calculate the total cumulative precipitation for each month of the data set.



