#================
# IMPORT PACKAGES
#================
library(tidyverse)
library(lubridate)
library(plotly) # my addition

options(scipen=999) #remove scientific notation in axis

covid_data %>% filter(date==as_date('2021-06-07')) %>% 
  select(country, confirmed, dead, recovered) %>% 
  group_by(country) %>% 
  summarise(dead=sum(dead), confirmed=sum(confirmed)) %>% 
  ggplot(aes(x=confirmed, y=dead)) + geom_point() 

today_data <- covid_data %>% filter(date==as_date('2021-06-07'))

model_linear1 <- lm(dead~confirmed, today_data)
summary(model_linear1)
model_intercept <- coef(model_linear1) [1]
model_slope <- coef(model_linear1) [2]

#ggplot(data = today_data, aes(x=confirmed, y=dead))+
  #geom_point() +
  #demo_continuous(c(0, 10000000), breaks = breaks_width(500000)) +
  #scale_x_continuous(breaks = breaks_width(20000000)) +
  #geom_abline(intercept = model_intercept, slope = model_slope, color="red")
ggplotly()


# scale_y_continuous(labels=function(n){format(n, scientific = FALSE)})

# create a bar chart of the top 15 countries with the most confirmed cases
covid_data %>% 
  select(country, confirmed) %>% 
  group_by(country) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  arrange(desc(confirmed)) %>% 
  top_n(15) %>% 
  ggplot(aes(x= country, y = confirmed)) +
  geom_bar(stat = 'identity', fill = "darkred")

# create a bar chart of the top 15 countries with the most confirmed cases
# same as above but horizontal
# and in desc order
covid_data %>% 
  select(country, confirmed) %>% 
  group_by(country) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  arrange(desc(confirmed)) %>% 
  top_n(15) %>% 
  ggplot(aes(x= confirmed, y = fct_reorder(country, confirmed))) +
  geom_bar(stat = 'identity', fill = "darkred") +
  labs(y = '')

#Line chart of world covid-19 cases over time (excluding China)
covid_data %>% 
  filter(country!="China") %>% 
  group_by(date) %>% 
  summarize(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(color='red')

#Line chart of world covid-19 cases over time, China vs World
# create a line chart that shows two lines.
# one line for China, and another line for the rest of the world.

#use the mutate() function to create 
#an indicator variable called china_ind 
#that will distinguish between China vs Not-China.

covid_data %>% 
  #mutate(china_ind = if_else(country == 'China', 'China', 'Not China')) %>% 
  mutate(india_ind = if_else(country == 'India', 'India', 'Not India')) %>% 
  #group_by(china_ind, date) %>% 
  group_by(india_ind, date) %>% 
  summarise(dead = sum(dead)) %>% 
  ggplot(aes(x = date, y = dead)) +
  #geom_line(aes(color = china_ind)) +
  geom_line(aes(color = india_ind)) +
  scale_color_manual(values = c('navy', 'red'))
  
ggplotly()


# covid_data %>% 
#   mutate(india_ind = if(country=='India', 'India')) %>% 
#   mutate(china_ind = if(country=='China', 'China')) %>% 
#   mutate(us_ind = if(country=='US', 'US'))

# line chart of a few major countries that have been in the news with major covid outbreaks.
covid_data %>% 
  filter(country %in% c('US', 'UK', 'India', 'Brazil', 'Spain', 'Italy', 'France', 'China')) %>% 
  group_by(country, date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = date, y = confirmed)) +
  geom_line(aes(color = country)) +
  scale_color_manual(values = c('navy', 'lightblue', 'orange', 'green', 'red', 'darkgreen', 'gray', 'darkred'))

ggplotly()
















  











