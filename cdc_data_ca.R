
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
#                 CDC data via socrata
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''




# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 1: Load libraries                                                 ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

library(RSocrata)
library(tidyverse)
library(lubridate)
library(scales)
library(zoo)       # calculate moving averages
library(patchwork)
library(grid)      # for tableGrobs
library(gridExtra) # for multiple plots
library(ggthemes)  # for theme_clean

#library(plotly)
#library(ggrepel)   # for label plots


# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 2: Get raw data from CDC via API                                   ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# https://data.cdc.gov/Case-Surveillance/United-States-COVID-19-Cases-and-Deaths-by-State-o/9mfq-cb36
# United States COVID-19 Cases and Deaths by State over Time (Case Surveillance)
# API endpoint: https://data.cdc.gov/resource/9mfq-cb36.csv

# import CALIFORNIA data via api

source("masked_variables.R")

df_ca_raw <- read.socrata(
  "https://data.cdc.gov/resource/9mfq-cb36.csv?state=CA",
  app_token = hide_token,
  email     = hide_email,
  password  = hide_pwd
)




# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 3: Clean & transform raw data                                      ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

df_ca <- df_ca_raw %>% 

  # don't need all columns
  select(submission_date, tot_cases, tot_death, new_case, new_death) %>% 
  
  # arrange chronologically
  arrange(submission_date) %>% 

  # convert "submission date" (from datetime) into date
  # convert into numeric vectors (from character)
  mutate(submission_date = date(submission_date),
         tot_cases = as.numeric(tot_cases),
         tot_death = as.numeric(tot_death),
         new_case = as.numeric(new_case),
         new_death = as.numeric(new_death)) %>% 
  
  # extract month, month_name, day
  mutate(month = month(submission_date), 
         month_name = month(submission_date, label = TRUE, abbr = TRUE), 
         day = day(submission_date)) %>% 
  
  # add 7-day average for new cases & deaths
  mutate(avg7_case = zoo::rollmean(new_case, k = 7, align = "right", fill = NA),
         avg7_death = zoo::rollmean(new_death, k = 7, align = "right", fill = NA)) 




# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 4: Specify date variable & related                                 ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

# when was this data last updated?
(ca_latest_update <- paste("Source: CDC, last updated:", max(df_ca$submission_date)))

# whats the earliest date we have data for?
(pandemic_start <- min(df_ca$submission_date))

# when was the CA lockdown lifted?
(ca_lift_lockdown <- as.Date("2021-06-15"))

# cases on the last day of the lockdown
ca_lift_lockdown_cases <- df_ca$new_case[df_ca$submission_date == ca_lift_lockdown]

# deaths on the last day of the lockdown
ca_lift_lockdown_deaths <- df_ca$new_death[df_ca$submission_date == ca_lift_lockdown]

# 2 weeks after lockdown was lifted
ca_lift_lockdown_plus <- as.Date("2021-06-29")

# last 60 days
(last_60_days <- ymd(today()) - 60)
# (end_date <- ymd(today()) - 1) 
(end_date = max(df_ca$submission_date))

# first vaccine in CA was Dec 14, 2020
# two weeks after first dose
ca_first_vax_immunity <- as.Date("2020-12-28")
# two weeks after second dose
ca_second_vax_immunity <- as.Date("2021-01-21")

# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 5: ggplot theme & other formatting                                 ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

options(scipen = 999) # remove scientific notation in axes





# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# STEP 6: START VIZZES                                                    ----
#
# ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  



# ``````````````````````````````````````````````````````````````
#
#   LINE CHART: Daily new cases in CA (2020-2021)           ----
#               with 7-day rolling average
#
# 
# ``````````````````````````````````````````````````````````````

start_date <- pandemic_start

cases1 <- 
  df_ca %>% 
  select(submission_date, month, new_case, avg7_case) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_case)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  
  geom_col(fill = "#bcbcbc") +
  geom_line(aes(y = avg7_case), color = "blue", size = 2) +
  geom_area(aes(y = avg7_case), fill = "blue", alpha = 0.2) +
  
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 month", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n=4),
                     #breaks = breaks_width(500), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  
  labs(x = NULL, y = NULL,
       title = "Daily cases in California",
       subtitle = "Each gray column represents a day.\nThe yellow block highlights dates after June 15. \n",
       caption = ca_latest_update) +
  
  theme_clean() +
  theme(legend.position = "none") 

# x x x x

start_date <- last_60_days

cases2 <- 
  df_ca %>% 
  select(submission_date, month, new_case, avg7_case) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_case)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  
  geom_col(fill = "#bcbcbc") +
  geom_line(aes(y = avg7_case), color = "blue", size = 2) +
  geom_area(aes(y = avg7_case), fill = "blue", alpha = 0.2) +
  
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4),
                     #breaks = breaks_width(500), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  
  labs(x = NULL, y = NULL,
       title = "Last 60 days in California",
       subtitle = "Each gray column represents a day.\nThe yellow block highlights dates after June 15. \n",
       caption = ca_latest_update) +
  
  theme_clean() +
  theme(legend.position = "none") 


# x x x x 

start_date <- pandemic_start

cases3 <- 
  df_ca %>% 
  select(submission_date, month, tot_cases, avg7_case) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = tot_cases)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  
  geom_col(fill = "#bcbcbc") +
  #geom_line(aes(y = avg7_case), color = "blue", size = 2) +
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "3 months", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n=4),
                     #breaks = breaks_width(500), 
                     labels = label_comma(),
                     expand = c(0,0)) +
  
  labs(x = NULL, y = NULL,
       title = "Cumulative cases to date in California",
       subtitle = "Each gray column represents a day.\nThe yellow block highlights dates after June 15. \n",
       caption = ca_latest_update) +
  
  theme_clean() +
  theme(legend.position = "none") 

# x x x x

cases4 <- 
  df_ca %>% 
  select(Date = submission_date, Cases = new_case, `Cummulative Cases` = tot_cases, Deaths = new_death, `Cummulative Deaths` = tot_death ) %>% 
  arrange(desc(Date)) 

cases4 <- tableGrob(head(cases4, 15))



options(repr.plot.width = 16, repr.plot.height = 8)
#composite <- grid.arrange(death1, death2, death3, nrow = 3)
(everything_cases <- cases1 + cases3 + cases2 + cases4 + plot_layout(ncol = 2, widths = c(3, 2)))


# ```````````````````````````E N D `````````````````````````````




# ``````````````````````````````````````````````````````````````
#
#   LINE CHART: Daily new deaths in CA (2020-2021)          ----
#               with 7-day rolling average
#
# 
# ``````````````````````````````````````````````````````````````

start_date <- pandemic_start

death1 <- 
  df_ca %>% 
  select(submission_date, month, new_death, avg7_death) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_death)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  
  geom_col(fill = "#bcbcbc") +
  geom_line(aes(y = avg7_death), color = "red", size = 2) +
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 month", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  
  labs(x = "", y = "",
       title = "Daily deaths in California", 
       subtitle = "Each gray column represents a day. \nThe yellow block highlights dates after June 15. ",
       caption = ca_latest_update) +
  
  theme_clean() + 
  theme(legend.position = "none") 

# x x x x

start_date <- last_60_days

death2 <- 
  df_ca %>% 
  select(submission_date, month, new_death, avg7_death) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_death)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  
  geom_col(fill = "#bcbcbc") +
  geom_line(aes(y = avg7_death), color = "red", size = 2) +
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "15 days", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec_axis = dup_axis()) +
  
  labs(x = "", y = "",
       title = "Last 60 days in California", 
       subtitle = "Each gray column represents a day. \nThe yellow block highlights dates after June 15. ",
       caption = ca_latest_update) +
  
  theme_clean() + 
  theme(legend.position = "none")

# x x x x

start_date <- pandemic_start

death3 <- 
  df_ca %>% 
  select(submission_date, month, tot_death, avg7_death) %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = tot_death)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  
  geom_col(fill = "#bcbcbc") +
  #geom_line(aes(y = avg7_death), color = "red", size = 2) +
  geom_vline(xintercept = ca_first_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_second_vax_immunity, color = "black", linetype = "dashed") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 month", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec_axis = dup_axis()) +
  
  labs(x = "", y = "",
       title = "Cummulative death toll in California", 
       subtitle = "Each gray column represents a day. \nThe yellow block highlights dates after June 15. ",
       caption = ca_latest_update) +
  
  theme_clean() + 
  theme(legend.position = "none")

# x x x x 

death4 <- 
  df_ca %>% 
  select(Date = submission_date, 
         Cases = new_case, 
         `Cummulative Cases` = tot_cases, 
         Deaths = new_death, 
         `Cummulative Deaths` = tot_death) %>% 
  arrange(desc(Date)) 

death4 <- tableGrob(head(death4, 15))



options(repr.plot.width = 16, repr.plot.height = 8)
#composite <- grid.arrange(death1, death2, death3, nrow = 3)
(everything_deaths <- death1 + death3 + death2 + death4 + plot_layout(ncol = 2, widths = c(3, 2)))



# ```````````````````````````E N D `````````````````````````````



# ``````````````````````````````````````````````````````````````
#
#   Last 60 days                                            ----
#
# 
# ``````````````````````````````````````````````````````````````

start_date <- last_60_days

g1 <- 
  df_ca %>% 
  filter(submission_date >= as.Date(start_date)) %>%
  
  ggplot(aes(x = submission_date, y = tot_cases)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  #annotate("text", x = ca_lift_lockdown, y = 25000000, label = "Freedom") +
  #annotate("text", x = ca_lift_lockdown_plus, y = 35000000, label = "Freedom\n+14 days") +
  
  geom_line(stat = "identity", color = "blue") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "2 weeks", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4), 
                     labels = label_comma(),
                     expand = c(0,0)) +
  
  labs(x = "", y = "",
       title = "Total Cases") +
  
  theme_clean() + 
  theme(legend.position = "none")


g2 <- 
  df_ca %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_case)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  annotate("text", x = ca_lift_lockdown, y = 1500, label = "Freedom") +
  annotate("text", x = ca_lift_lockdown_plus, y = 1700, label = "Freedom\n+14 days") +
  
  geom_col(fill = "#ababab") +
  geom_line(aes(y = avg7_case), color = "blue", size = 1.2) +
  geom_area(aes(y = avg7_case), fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 5), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  
  labs(x = "", y = "",
       title = "Daily New Cases") +
  
  theme_clean() + 
  theme(legend.position = "none")


g3 <- 
  df_ca %>% 
  filter(submission_date >= as.Date(start_date)) %>%
  
  ggplot(aes(x = submission_date, y = tot_death)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  #annotate("text", x = ca_lift_lockdown, y = 25000, label = "Freedom") +
  #annotate("text", x = ca_lift_lockdown_plus, y = 45000, label = "Freedom\n+14 days") +
  
  geom_line(stat = "identity", color = "red") +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "2 weeks", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 4), 
                     labels = label_comma(),
                     expand = c(0,0)) +
  
  labs(x = "", y = "",
       title = "Total Deaths") +
  
  theme_clean() + 
  theme(legend.position = "none")


g4 <-
  df_ca %>% 
  filter(submission_date >= as.Date(start_date)) %>% 
  
  ggplot(aes(x = submission_date, y = new_death)) +
  
  annotate("rect", 
           xmin = as.Date('2021-06-15'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "yellow", alpha = 0.3) + 
  annotate("rect", 
           xmin = as.Date('2021-06-29'), xmax = end_date, 
           ymin = -Inf, ymax = Inf, 
           fill = "red", alpha = 0.2) +
  annotate("text", x = ca_lift_lockdown, y = 25, label = "Freedom\nJune 15") +
  annotate("text", x = ca_lift_lockdown_plus, y = 45, label = "14 days\nlater") +
  
  
  geom_col(fill = "#bcbcbc") +
  geom_line(aes(y = avg7_death), color = "red", size = 1.2) +
  geom_area(aes(y = avg7_death), fill = "red", alpha = 0.4) +
  geom_vline(xintercept = ca_lift_lockdown, color = "red", linetype = "dashed") +
  
  scale_x_date(date_breaks = "1 week", 
               labels = label_date_short(),
               expand = c(0,0)) +
  scale_y_continuous(breaks = breaks_extended(n = 5), 
                     labels = label_comma(),
                     expand = c(0,0),
                     sec.axis = dup_axis()) +
  
  labs(x = "", y = "",
       title = "Daily New Deaths") +
  
  theme_clean() + 
  theme(legend.position = "none")



options(repr.plot.width = 16, repr.plot.height = 8)
#composite <- grid.arrange(g1, g2, g3, g4, ncol = 2)
(everything_last60 <- g1 + g2 + g3 + g4 + plot_layout(ncol = 2, widths = c(1, 3)))






#
