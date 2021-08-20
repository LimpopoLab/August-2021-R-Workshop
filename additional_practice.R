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



water_cor <- water_all %>% 
  select(-DOC,-TOC,-Turbidity) %>% 
  filter(!is.na(Secchi),!is.nan(Chlorophyll),!is.nan(TSS))

install.packages('GGally')
library(GGally)

ggpairs(water_cor %>% select(-Year))


library(adklakedata)
library(corrplot)

lakes <- adk_data('chem')

lakes_cor <- lakes %>% 
  select(-PERMANENT_ID,-lake.name,-date,-year,-month)

cor_matrix <- cor(lakes_cor,use='pairwise.complete.obs')

corrplot(cor_matrix,type='lower')
ggpairs(lakes_cor[complete.cases(lakes_cor),1:10])


#Practice solution-----------------------
library(adklakedata)
library(tidyverse)
library(lubridate)
library(trend)

lake_data <- adk_data('chem')

lake_plot <- lake_data %>% 
  group_by(year,lake.name) %>% 
  summarise(pH = mean(pH, na.rm=TRUE))

ggplot(lake_plot,aes(year,pH))+
  geom_point()+
  geom_line()+
  facet_wrap(~lake.name)

ggplot(lake_data,aes(month,DOC,color=lake.name))+
  geom_line()+
  facet_wrap(~year,scales='free')






library(adklakedata)
library(tidyverse)
library(lubridate)
library(trend)


###PRACTICE###

#Is there a significant correlation between average Secchi depth and max depth across lakes? 
#Show the relationship with a plot

secchi <- adk_data('secchi')
meta <- adk_data('meta')

?merge

#In Big Moose lake, is there a significant difference in the mean org.l across taxa? Include a visual

crust <- adk_data('crustacean')


#Is there a significant trend in mean annual surface water temperature (depth of 1 m and less) over time
#in Windfall lake? Include a visual

temp <- adk_data('tempdo')









#Is there a significant correlation between average Secchi depth and max depth across lakes? 
#Show the relationship with a plot

secchi <- adk_data('secchi')
meta <- adk_data('meta')

?merge

avg_secchi <- secchi %>% 
  group_by(lake.name) %>% 
  summarise(secchi = mean(secchi,na.rm=TRUE))

secchi.depth <- merge(avg_secchi,meta)

ggplot(secchi.depth,aes(max.depth, secchi))+
  geom_point()+
  geom_smooth(method='lm')

cor(secchi.depth$secchi,secchi.depth$max.depth)
cor.test(secchi.depth$secchi,secchi.depth$max.depth)

mod <- lm(secchi ~ max.depth, data = secchi.depth)
summary(mod)

#In Big Moose lake, is there a significant difference in the mean org.l across taxa? Include a visual
crust <- adk_data('crustacean')

unique(crust$Taxa)

crust_clean <- crust %>% 
  filter(lake.name == 'Big Moose')

ggplot(crust_clean,aes(org.l))+
  geom_density()+
  facet_wrap(~Taxa,scales='free')

kruskal <- kruskal.test(org.l ~ Taxa, data = crust_clean)
kruskal

library(FSA)

dunnTest(org.l ~ Taxa,
         data=crust_clean,
         method="bonferroni")

ggplot(crust_clean, aes(x = Taxa, y = log(org.l)))+
  geom_boxplot()

#Is there a significant trend in mean annual surface water temperature (depth of 1 m and less) over time
#in Windfall lake? Include a visual

temp <- adk_data('tempdo')

temp_surface <- temp %>% 
  filter(depth <= 1, lake.name == 'Windfall') %>% 
  mutate(Date = as.Date(date),
         Year = year(Date)) %>% 
  group_by(Year) %>% 
  summarise(temp = mean(temp,na.rm=T))

library(trend)

sens.slope(temp_surface$temp)

ggplot(temp_surface,aes(Year,temp))+
  geom_point()+
  geom_smooth(method='lm')

mod <- lm(temp ~ Year, data = temp_surface)
summary(mod)
