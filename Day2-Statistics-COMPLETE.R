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

mean.waterlevel <- mean(beit.bridge.clean$Level..m., na.rm = T)

median.waterlevel <- median(beit.bridge.clean$Level..m, na.rm = T)

n.waterlevel <- length(which(!is.na(beit.bridge.clean$Level..m)))

sd.waterlevel <- sd(beit.bridge.clean$Level..m, na.rm = T)

sterr.waterlevel <- sd.waterlevel / sqrt(n.waterlevel)

cv.waterlevel <- sd.waterlevel / mean.waterlevel

range.waterlevel <- range(beit.bridge.clean$Level..m, na.rm = T)

confint.waterlevel <- quantile(beit.bridge.clean$Level..m, c(0.05, 0.95), na.rm = T)

summary(beit.bridge.clean)

#####################
### Analyses in R ###
#####################

# correlation of water level and flow, look at plot too

cor(x = beit.bridge.clean$Level..m, y = beit.bridge.clean$Flow..cumec.,
    use = "pairwise.complete.obs")

cor.test(x = beit.bridge.clean$Level..m, y = beit.bridge.clean$Flow..cumec.)

ggplot(data = beit.bridge.clean, mapping = aes(x = Level..m., y = Flow..cumec.)) +
  geom_point() +
  geom_smooth(method='lm')



# re-do correlation after removing some outliers, and re-plot

beit.bridge.subset <- beit.bridge.clean %>%
  filter(Level..m. < 6)

cor(x = beit.bridge.subset$Level..m, y = beit.bridge.subset$Flow..cumec., 
    use = "pairwise.complete.obs")

cor.test(x = beit.bridge.subset$Level..m, y = beit.bridge.subset$Flow..cumec.)


ggplot(data = beit.bridge.subset, mapping = aes(x = Level..m., y = Flow..cumec.)) +
  geom_point() +
  geom_smooth()




# t-test - one-sided (river stage)

t.test(mutale.clean$RiverStage_m)

ggplot(data = mutale.clean, aes(RiverStage_m)) +
  geom_density() +
  geom_vline(xintercept = 0, linetype = 2)



# try another example to compare air temperature vs. water temperature

t.test(x = mutale.clean$AirTemp_degC, y = mutale.clean$WaterTemp_degC)

ggplot() +
  geom_boxplot(data = mutale.clean, aes(x = "Air Temp.", y = AirTemp_degC), fill = "grey50") +
  geom_boxplot(data = mutale.clean, aes(x = "Water Temp.", y = WaterTemp_degC), fill = "blue")




# ANOVA (1-way)
## Is there a significant difference in river stage across QUARTERS of the year?

riverstage.monthly <- mutale.clean %>%
  mutate(Quarter = factor(quarter(Date_Time)))

levels(riverstage.monthly$Quarter)

riverstage.anova <- aov(RiverStage_m ~ Quarter, data = riverstage.monthly)
summary(riverstage.anova)

riverstage.tukey <- TukeyHSD(riverstage.anova)
riverstage.tukey


## PRACTICE:  
# Are catchment areas different by drainage region?

drainage <- read.csv("Sites-4DrainageRegions.csv")

head(drainage)

unique(drainage$DrainageRegion)

drainage.anova <- aov(CatchmentArea_km2 ~ DrainageRegion, data = drainage)

summary(drainage.anova)





# linear regression of water level trends over time

mod1 <- lm(Level..m. ~ Year, data = beit.bridge.clean)
mod1

summary(mod1)

ggplot(data = beit.bridge.clean, aes(x = Year, y = Level..m.)) +
  geom_line() +
  geom_smooth(method = "lm")


## PRACTICE with bottom water temperature (or temperature difference)

mod2 <- lm(Flow..cumec. ~ Level..m., data = beit.bridge.clean)
mod2

summary(mod2)

ggplot(data = beit.bridge.clean, aes(x = Level..m., y = Flow..cumec.)) +
  geom_point() +
  geom_smooth(method = "lm")






################################
### Checking for assumptions ###
################################

?shapiro.test
shapiro.test(beit.bridge.clean$Level..m)

ggplot(data = beit.bridge.clean,aes(Level..m.))+
  geom_density()

qqnorm(beit.bridge.clean$Level..m)
qqline(beit.bridge.clean$Level..m)



shapiro.test(beit.bridge.clean$Flow..cumec.)

ggplot(data = beit.bridge.clean,aes(log(Flow..cumec.)))+
  geom_density()

qqnorm(beit.bridge.clean$Flow..cumec.)
qqline(beit.bridge.clean$Flow..cumec.)


# Functions for common data transformations to achieve normality:

sqrt( X )

log10( X )

log( x )

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
view(msleep)

# 1:  Are brain weight and body weight correlated? Are they normally-distributed?


cor(x = msleep$brainwt, y = msleep$bodywt, use = "pairwise.complete.obs")

cor.test(msleep$brainwt,msleep$brainwt, use = 'pairwise.complete.obs')

ggplot(data = msleep, aes(x = brainwt, y = bodywt)) +
  geom_point()

shapiro.test(msleep$brainwt)
shapiro.test(msleep$bodywt)

ggplot(data = msleep, aes(brainwt)) +
  geom_density()


## 2:  Subset the above data to remove the 2 outliers and re-run your correlation.

msleep.subset <- msleep %>%
  filter(brainwt < 4)

cor(x = msleep.subset$brainwt, y = msleep.subset$bodywt, use = "pairwise.complete.obs")

ggplot(data = msleep.subset, aes(x = brainwt, y = bodywt)) +
  geom_point()




# 3:  Conduct an ANOVA and Tukey HSD (if necessary) 
# to test if ins of different vore have different total sleep amounts,
# and include a graph

sleep.anova <- aov(data = msleep, sleep_total ~ vore)
summary(sleep.anova)

TukeyHSD(sleep.anova)

ggplot(data = msleep, aes(x = vore, y = sleep_total)) +
  geom_boxplot()



# 4:  Conduct a linear regression of sleep total as
# function of sleep cycle for mammal data, and include a graph

sleep.mod <- lm(data = msleep, sleep_total ~ sleep_cycle)
summary(sleep.mod)

ggplot(data = msleep, aes(x = sleep_cycle, y = sleep_total)) +
  geom_point() +
  geom_smooth(method = "lm")



