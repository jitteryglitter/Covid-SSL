# part 5 small multiples

# --------------------
# import packages
# --------------------

library(tidyverse)
library(lubridate)
library(plotly) # my addition

options(scipen=999) #remove scientific notation in axis

# id top 12
covid_data %>% 
  filter(date==as_date('2021-06-07')) %>% 
  group_by(country) %>% 
  summarize(confirmed = sum(confirmed)) %>% 
  arrange(-confirmed) %>% 
  top_n(15, confirmed) ->
  covid_top_15

#--------------------------
# PLOT SMALL MULTIPLE CHART
# first as a combied chart
#--------------------------
covid_data %>% 
  filter(country %in% covid_top_15$country) %>% 
  group_by(country, date) %>% 
  summarize(new_cases = sum(new_cases)) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line(aes(color = country))

ggplotly()

#--------------------------
# PLOT SMALL MULTIPLE CHART
# fnow as multiples
#--------------------------
covid_data %>% 
  filter(country %in% covid_top_15$country) %>% 
  group_by(country, date) %>% 
  summarize(new_cases = sum(new_cases)) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line() + 
  facet_wrap(~country, ncol = 4)

ggplotly()












