# get all the unique counties

# --------------------
# import packages
# --------------------

library(tidyverse)
library(lubridate)

# --------------------
# for reference
# --------------------
names(covid_data)
# [1] "country"        "subregion"      "date"           "lat"            "long"           "confirmed"     
# [7] "dead"           "recovered"      "new_cases"      "new_deaths"     "new_recoveries"

# --------------------
# examine variables
# --------------------
covid_data %>% distinct(country) %>% print(n=200)
covid_data %>% distinct(country) %>% count()

covid_data %>% distinct(country, subregion) %>% print(n=300)
covid_data %>% distinct(country, subregion) %>% count()

covid_data %>% distinct(date) %>% print()
covid_data %>% distinct(date) %>% count()

covid_data %>% distinct(country, subregion, date) %>% count()

# check on lat, long
# way 1 -- nothing works because of the NAs
# update -- works ow after I added na.rm=TRUE
covid_data %>% select(lat) %>% summarize(min(lat, na.rm=TRUE), max(lat, na.rm=TRUE)) 
covid_data %>% select(long) %>% summarize(min(long, na.rm=TRUE), max(long, na.rm=TRUE)) 

# how to find the darn NAs
# turns out to be repatriated folks in Canada, also just unknown in China
covid_data %>% filter(is.na(lat)) -> lat_na
table(lat_na$subregion)
# Repatriated Travellers                Unknown 
# 502                                   502 
table(lat_na$country)
# Canada  China 
# 502    502 

# way 2
covid_data %>% select(lat, long) %>% gather() %>% group_by(key) %>% summarize('min'=min(value, na.rm=TRUE), 'max'=max(value, na.rm=TRUE))
# gyaan
# "The valid range of latitude in degrees is -90 and +90 for the southern and northern hemisphere respectively. Longitude is in the range -180 and +180 specifying coordinates west and east of the Prime Meridian, respectively." 

# similarly in way 2, check dead, revcoveredetc.

covid_data %>% select(confirmed, dead, recovered) %>% gather() %>% group_by(key) %>% summarize('min'=min(value, na.rm=TRUE), 'max'=max(value, na.rm=TRUE))

covid_us %>% filter(confirmed==33362600) #US
covid_data %>% filter(dead==597628) #US
covid_in %>% filter(recovered==27159180) #India

#-------------------------------
# SPOT CHECK IMPORTANT COUNTRIES
#-------------------------------

covid_data %>% 
  select(country, date, confirmed, dead, recovered) %>% 
  filter(country %in% c('US', 'Italy', 'Sweden', 'China')) %>% 
  filter(date == as_date('2021-05-01')) %>% 
  group_by(country, date) %>% 
  summarise(total_confirmed = sum(confirmed))
































































