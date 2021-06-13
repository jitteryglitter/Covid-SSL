# succcessful countries

#================
# IMPORT PACKAGES
#================
library(tidyverse)
library(lubridate)
library(zoo)


# top 19 countries with most cases 
covid_data %>% 
  filter(date==as_date('2021-06-07')) %>% 
  group_by(country) %>% 
  summarize(confirmed = sum(confirmed)) %>% 
  arrange(-confirmed) %>% 
  top_n(16, confirmed) ->
  covid_top_16

# create a function to categorize countries with "Winning", "Almost there", and "Needs work".

covid_data %>% 
  filter(country %in% covid_top_16$country) %>% 
  group_by(country, date) %>% 
  summarise(new_cases = sum(new_cases)) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line(aes(color = country))

covid_data %>% 
  filter(country %in% covid_top_16$country) %>% 
  group_by(country, date) %>% 
  summarise(new_cases = sum(new_cases)) %>% 
  ggplot(aes(x = date, y = new_cases)) +
  geom_line() +
  facet_wrap(~country, ncol = 4)

covid_success <- function(input_value){
  result = case_when(input_value=='Argentina' ~ 'Needs work'
                     ,input_value=='Brazil' ~ 'Needs work'
                     ,input_value=='Colombia' ~ 'Needs work'
                     ,input_value=='France' ~ 'Almost there'
                     ,input_value=='Germany' ~ 'Winning'
                     ,input_value=='India' ~ 'Almost there'
                     ,input_value=='Iran' ~ 'Almost there'
                     ,input_value=='Italy' ~ 'Almost there'
                     ,input_value=='Mexico' ~ 'Needs work'
                     ,input_value=='Poland' ~ 'Almost there'
                     ,input_value=='Russia' ~ 'Almost there'
                     ,input_value=='Spain' ~ 'Almost there'
                     ,input_value=='Turkey' ~ 'Almost there'
                     ,input_value=='Ukraine' ~ 'Almost there'
                     ,input_value=='United Kingdom' ~ 'Needs work'
                     ,input_value=='US' ~ 'Winning'
                     ,TRUE ~ 'Other'
                     )
  return(result)
}

covid_success('Germany')

# filter our data down to the top 16 countries
# calculate the 10 day rolling average by country and date.

covid_data %>% 
  filter(country %in% covid_top_16$country, date > as_date('2021-01-01')) %>% 
  group_by(country, date) %>% 
  summarise(new_cases = sum(new_cases)) %>% 
  mutate(new_case_rollmean_10 = rollmean(new_cases, k=10, na.pad = TRUE, align = 'right')) %>% 
  mutate(covid_success = covid_success(country)) %>% 
  select(country, date, covid_success, new_cases, new_case_rollmean_10) %>% 
  ggplot(aes(x = date, y = new_case_rollmean_10)) +
  geom_line(aes(color = covid_success)) +
  labs(color = 'Covid\nSuccess'
       ,title = 'Covid-19 New Cases\n10 day rolling average'
       ) + 
  theme(axis.text.y = element_blank()
        ,axis.ticks.y = element_blank()
        ,panel.background = element_blank()
        ,text = element_text(family = 'SimSun')
        ,strip.background = element_blank()
        ,strip.text = element_text(size = 14)
        ,axis.line.x = element_line(color = '333333')
        ,axis.title.y = element_blank()
        ,axis.title.x = element_blank()
        ,plot.title = element_text(size = 22, hjust = .5)
        ,legend.key = element_rect(fill = 'white')
        ) +
  facet_wrap(~country, scales = 'free') +
  scale_color_manual(values = c('Winning' = '#107F12'
                                ,'Almost there' = '#FDA520'
                                ,'Needs work' = '#FC101D'
                                )
                     )
ggplotly()




























