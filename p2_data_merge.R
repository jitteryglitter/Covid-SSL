# merge datasets

# --------------------
# import packages
# --------------------

library(tidyverse)
library(lubridate)

# --------------------
# define func 1 to rename columns
# --------------------

covid_rename_cols <- function(input_data){
  input_data %>% 
    rename('subregion' = 'Province/State'
           ,'country' = 'Country/Region'
           ,'lat' = 'Lat'
           ,'long' = 'Long'
           ) ->
    output_data
  return(output_data)
}

# --------------------
# define func 2 to pivot data
# --------------------

covid_pivot_data <- function(input_data, value_var_name){
  input_data %>% 
    pivot_longer(cols = -one_of('subregion', 'country', 'lat', 'long')
                 , names_to = 'date'
                 , values_to = value_var_name
                 ) ->
    output_data
  return(output_data)
}

# --------------------
# define func 3 to convert dates
# --------------------

covid_convert_dates <- function(input_data){
  input_data %>% 
    mutate(date = mdy(date)) ->
    output_data
  return(output_data)
}

# --------------------
# define func 4 to rearrange data
# --------------------

covid_rearrange_data <- function(input_data){
  input_data %>% 
    select(country, subregion, date, lat, long, everything()) %>% 
    arrange(country, subregion, date) ->
    output_data
  return(output_data)
}

# --------------------
# define func 5 MASTER FUNC 
# --------------------

covid_get_data <- function(input_url, value_var_name){
  covid_data_inprocess <- read_csv(input_url)
  covid_data_inprocess <- covid_rename_cols(covid_data_inprocess)
  covid_data_inprocess <- covid_pivot_data(covid_data_inprocess, value_var_name)
  covid_data_inprocess <- covid_convert_dates(covid_data_inprocess)
  covid_data_inprocess <- covid_rearrange_data(covid_data_inprocess)
  return(covid_data_inprocess)
}

# --------------------
# BEGIN: get data 
# --------------------

url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

covid_confirmed <- covid_get_data(url_confirmed,"confirmed")

covid_deaths <- covid_get_data(url_deaths,"dead")

covid_recovered <- covid_get_data(url_recovered,"recovered")

# --------------------
# BEGIN: drop unnecessary columns 
# --------------------

#covid_deaths <- covid_deaths %>% select(-lat, -long)
names(covid_deaths)
table(covid_deaths$country)

#covid_recovered <- covid_recovered %>% select(-lat, -long)

# --------------------
# BEGIN: merge all three datasets 
# --------------------

covid_confirmed %>% 
  left_join(covid_deaths, on = c(country, subregion, date)) %>% 
  left_join(covid_recovered, on = c(country, subregion, date)) ->
  covid_data

#print(covid_data)
#hist(covid_recovered$country)
covid_data %>%
  arrange(country, subregion, date) %>% 
  group_by(country, subregion) %>% 
  mutate(new_cases = confirmed - lag(confirmed)) %>% 
  mutate(new_deaths = dead - lag(dead)) %>% 
  mutate(new_recoveries = recovered - lag(recovered)) %>% 
  ungroup() ->
  covid_data

# --------------------
# EXTRAS 
# --------------------
covid_data %>% filter(country=="India") -> covid_in
covid_data %>% filter(country=="US") -> covid_us


# India & few others
# coind <- covid_data %>% filter(country==c("US", "Brazil", "United Kingdom", "France", "Spain", "Vietnam") & year(date)==2021)
# ggplot(coind, aes(x=date, y=new_cases, color=country)) + geom_line()
# ggplotly()
# 
# couk1 <- covid_data %>% filter(country=="United Kingdom" & is.na(subregion)==T)
# ggplot(couk1, aes(x=date, y=new_cases)) + geom_line()
# ggplotly()
# 
# couk2 <- covid_data %>% filter(country=="United Kingdom" & year(date)==2021)
# ggplot(couk2, aes(x=date, y=new_cases, color=subregion)) + geom_line()
# ggplotly()

# couk3 <- covid_data %>% filter(country=="United Kingdom" & is.na(subregion)==F)
# ggplot(couk3, aes(x=date, y=new_cases, color=subregion)) + geom_line()
# ggplotly()


# any other country not tracking recoveries?
# look for a negative number in new recoveries

















