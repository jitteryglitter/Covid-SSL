# --------------------
# multi-part series on visualizing Covid data from JHU
# JHU on Github: https://github.com/CSSEGISandData/COVID-19
#
# Part 1: Retrieve a simple covid19 csv file
# Part 1: https://www.sharpsightlabs.com/blog/r-data-analysis-covid-19-part1-data-wrangling/
#
# Part 2: Retrieve several related covid19 datafiles, clean them up, and merge them into a single “master file”
# https://www.sharpsightlabs.com/blog/r-data-analysis-covid-19-part-2-merge-datasets/
#
# Part 3: Explore the data, largely using dplyr and subsetting techniques
# Part 3: https://www.sharpsightlabs.com/blog/r-data-exploration-covid19-part3/
#
# Part 4: Visualize the data for exploratary analysis
# Part 4: https://www.sharpsightlabs.com/blog/r-data-visualization-covid19-part4/
#
# Part 5: Find problems with the data
# Part 5: https://www.sharpsightlabs.com/blog/r-covid19-analysis-part5-data-issues/
#
# Part 6:
# Part 6: https://www.sharpsightlabs.com/blog/r-data-analysis-covid19-part6-successful-countries/
# -------------------- 



# --------------------
# get raw data
# --------------------

url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

covid_data_RAW <- read_csv(url_confirmed)

# --------------------
# import packages
# --------------------

library(tidyverse)
library(lubridate)

# --------------------
# inspect dataframe
# --------------------

glimpse(covid_data_RAW)

# --------------------
# rename columns
# --------------------

covid_data_RAW %>% 
  rename('subregion' = 'Province/State'
         ,'country' = 'Country/Region'
         ,'lat' = 'Lat'
         ,'long' = 'Long'
         ) ->
  covid_data

# --------------------
# reshape data
# unpivot dates
# --------------------

covid_data %>% 
  pivot_longer(cols = -one_of('country', 'subregion', 'lat', 'long')
               , names_to = 'date'
               , values_to = 'confirmed'
               ) ->
  covid_data

# --------------------
# reorder columns
# --------------------

covid_data %>% 
  select('country', 'subregion', everything()) ->
  covid_data

# --------------------
# convert dates (from chr to date)
# --------------------

covid_data %>% select(date)

covid_data %>% 
  mutate(date = mdy(date)) ->
  covid_data

# --------------------
# sort & rearrange data
# --------------------

covid_data %>% 
  select(country, subregion, date, lat, long, confirmed) %>% 
  arrange(country, subregion, date) ->
  covid_data

# --------------------
# general check on data
# --------------------
covid_data %>% 
  select(country) %>% 
  unique() %>% 
  print(n=200)

# --------------------
# pull US records
# --------------------

covid_data %>% 
  select(country, date, confirmed) %>% 
  filter(country == "US") %>% 
  group_by(country, date) %>% 
  summarise(confirmed = sum(confirmed)) ->
  covid_us

covid_data %>% 
  select(country, date, confirmed) %>% 
  filter(country == "India") %>% 
  group_by(country, date) %>% 
  summarise(confirmed = sum(confirmed)) ->
  covid_in


# --------------------
# pull extras: some don't work!!!!!!
# --------------------

#covid_data %>% select(country, lat, long, confirmed) %>% group_by(country) %>% mutate(confirmed = sum(confirmed)) %>% group_by(country) -> temp_all

# extra dataset to inspect bizarre behavior
# do not use

# covid_data %>% 
#   select(country, date, confirmed) %>% 
#   filter(country == c("US", "India", "Singapore")) %>% 
#   group_by(date, country) %>% 
#   summarise(confirmed = sum(confirmed)) ->
#   covid_both














