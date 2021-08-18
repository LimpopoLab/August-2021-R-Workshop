#tidying data
library(tidyverse)
library(lubridate)

#Data Wrangling--------------------------------------------

secchi_data <- read.csv('Secchi data 20210317.csv')
glimpse(secchi_data)

unique(secchi_data$Located.on.Feature.Name)

#make a dataframe of annual mean secchi depth values
secchi_clean <- secchi_data %>% 
  mutate(Date = ymd(Sample.Start.Date),
         Year = year(Date)) %>% 
  filter(Located.on.Feature.Name == "A2R009 ROODEPLAAT DAM (PIENAARS RIVER) AT ROODEPLAAT") %>% 
  group_by(Year) %>% 
  summarise(Secchi = mean(SECCHI.Phys.Water..SECCHI...m..Result,na.rm=TRUE))


#using summarise_all()
water <- read.csv('water_quality.csv') 
glimpse(water)

water_clean <- water %>% 
  mutate(Date = ymd(Sample.Start.Date),
         Year = year(Date)) %>% 
  filter(Located.on.Feature.Name == "A2R009 ROODEPLAAT DAM (PIENAARS RIVER) AT ROODEPLAAT") %>% 
  select(Year,contains('Result')) %>% 
  group_by(Year) %>% 
  summarise_all(.funs = mean,na.rm=TRUE) %>% 
  rename(Chlorophyll = CHL.A.Susp.Water..CHLOROPHYLL.A...ug.L..Result,
         Turbidity = TURB.Phys.Water..TURBIDITY...NTU..Result,
         TOC = OC.Total.Water..ORGANIC.CARBON.TOTAL...mg.L..Result,
         DOC = OC.Diss.Water..ORGANIC.CARBON...mg.L..Result,
         TSS = SOLIDS.Susp.Water..TOTAL.SUSPENDED.SOLIDS...mg.L..Result)


#using merge()  
water_all <- merge(x = secchi_clean, y = water_clean,
                   by.x = 'Year', by.y = 'Year',
                   all = TRUE)



#Plotting---------------------------------------------------------------------


#plot all variables
water_plot <- water_all %>% 
  pivot_longer(cols = 2:7,
               names_to = "Variable",
               values_to = "Value") %>% 
  filter(!is.nan(Value))

ggplot(water_plot,aes(Year,Value))+
  geom_point()+
  geom_line()+
  facet_wrap(~Variable,scales='free')



secchi_data <- read.csv('Secchi data 20210317.csv')
glimpse(secchi_data)

unique(water$Located.on.Feature.Name)

#make a dataframe of annual mean secchi depth values
secchi_clean <- secchi_data %>% 
  mutate(Date = ymd(Sample.Start.Date),
         Year = year(Date)) %>% 
  filter(Located.on.Feature.Name %in% unique(water$Located.on.Feature.Name)[1:5]) %>% 
  group_by(Year,Located.on.Feature.Name) %>% 
  summarise(Secchi = mean(SECCHI.Phys.Water..SECCHI...m..Result,na.rm=TRUE))


#using summarise_all()
water <- read.csv('water_quality.csv') 
glimpse(water)

water_clean <- water %>% 
  mutate(Date = ymd(Sample.Start.Date),
         Year = year(Date)) %>% 
  filter(Located.on.Feature.Name %in% unique(water$Located.on.Feature.Name)[1:5]) %>% 
  select(Year,Located.on.Feature.Name,contains('Result')) %>% 
  group_by(Year,Located.on.Feature.Name) %>% 
  summarise_all(.funs = mean,na.rm=TRUE) %>% 
  rename(Chlorophyll = CHL.A.Susp.Water..CHLOROPHYLL.A...ug.L..Result,
         Turbidity = TURB.Phys.Water..TURBIDITY...NTU..Result,
         TOC = OC.Total.Water..ORGANIC.CARBON.TOTAL...mg.L..Result,
         DOC = OC.Diss.Water..ORGANIC.CARBON...mg.L..Result,
         TSS = SOLIDS.Susp.Water..TOTAL.SUSPENDED.SOLIDS...mg.L..Result)

water_all <- merge(x = secchi_clean, y = water_clean,
                   by.x = c('Year','Located.on.Feature.Name'), by.y = c('Year','Located.on.Feature.Name'),
                   all = TRUE)

water_plot <- water_all %>% 
  pivot_longer(cols = 3:8,
               names_to = "Variable",
               values_to = "Value") %>% 
  filter(!is.nan(Value))

ggplot(water_plot,aes(Year,Value,color=Located.on.Feature.Name))+
  geom_point()+
  geom_line()+
  facet_wrap(~Variable,scales='free')+
  theme_bw()+
  theme(legend.position = 'bottom')



