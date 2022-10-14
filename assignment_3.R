# ------------------------------------------------------------
# Assignment 3 - Data Wrangling
# Nicolas Herrera
# ------------------------------------------------------------

# Libraries
library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)

# Question 1
uof <- tibble(read_csv("uof_louisville.csv"))

#Question 2.a
uof <- uof %>%  
  mutate( hour= hour(time_of_occurrence))  

frequency <- uof %>%  
  group_by(hour) %>% 
  count(hour) %>% 
  arrange(desc(n))

frequent_hour <- frequency$hour[1]


#Question 2.b
uof <- uof %>%  
  mutate( month= month(date_of_occurrence))  

frequency <- uof %>%  
  group_by(month) %>% 
  count(month) %>% 
  arrange(n)

least_frequent_month <- frequency$month[1]

#Question 2.c
uof <- uof %>%  
  mutate( day= wday(date_of_occurrence ,  label = TRUE))  

frequency <- uof %>%  
  group_by(day) %>% 
  count(day) %>% 
  arrange(desc(n))

most_frequent_day <- frequency$day[1]

#Question 2.d
day_distribution <- uof %>% 
  mutate( day= mday(date_of_occurrence)) %>%   
  group_by(day) %>% 
  count(day) %>% 
  arrange(desc(n)) %>% 
  adorn_totals(where = "row")  %>%
  mutate( frequency = n / day_distribution$n[32])  

#Question 3.a  
force_used_1 <- uof %>% 
  distinct(force_used_1)

#Question 3.b  
force_used_2 <- uof %>% 
  distinct(force_used_2) 

#Question 3.c  
all_force <- uof %>%
  select(force_used_1:force_used_8) %>%
  t() %>%
  c() %>%
  unique()

#Question 3.d  
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used") 

#Question 3.e  
uof <- uof %>% 
  mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force, 1, 0))

#Question 3.f 
violent_force_service_table <- uof %>% 
  filter(violent_uof_1 == 1) %>% 
  count(service_rendered ,sort = T) %>% 
  mutate( frequency = n / sum(n)) %>%  
  adorn_totals(where = "row") 

#Question 4.a
uof_filtered <- uof %>% 
  filter( citizen_gender =="male" | citizen_gender =="female" ) %>%
  filter( !is.na(citizen_race) ) %>% 
  mutate()
  

unique(uof_filtered$citizen_race)

?filter