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
uof <- read_csv("uof_louisville.csv")
uof <- tibble(uof)

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
  mutate( day= day(date_of_occurrence)) %>% 
  count(day ,sort = T) %>% 
  mutate( fraction = n / sum(n)) %>%  
  adorn_totals(where = "row") 

#Question 3.a  
force_used_1 <- unique(uof$force_used_1)

#Question 3.b  
force_used_2 <- unique(uof$force_used_2) 

#Question 3.c  
all_force <- uof %>%
  select(force_used_1,
         force_used_2,
         force_used_3,
         force_used_4,
         force_used_5,
         force_used_6,
         force_used_7,
         force_used_8) %>%
  t() %>%
  c() %>%
  unique()

#Question 3.d  
violent_force <- c("take down", "hobble", "ecw cartridge deployed", "knee strike(s)",
                   "12 ga. sock round", "take-down", "impact weapon",
                   "kick", "deadly force used") 

#Question 3.e  
uof <- read_csv("uof_louisville.csv")
uof <- tibble(uof)
uof <- uof %>% mutate(violent_uof_1 = ifelse(force_used_1 %in% violent_force, 1, 0))

#Question 3.f 

violent_force_service_table <- uof %>%
  filter(violent_uof_1 == 1) %>%
  count(service_rendered, sort = T) %>%
  mutate(fraction = n/sum(n)) %>%
  adorn_totals()

#Question 4.a
uof_filtered <- uof %>% 
  filter( citizen_gender =="male" | citizen_gender =="female" ) %>%
  filter( !is.na(citizen_race) ) %>% 
  mutate(force_used_1_effective_binary = ifelse(force_used_1_effective == "yes" , 1 , 0))
  
#Question 4.b
uof_filtered_table <- uof_filtered %>%
  group_by(citizen_gender, citizen_race) %>%
  summarize(effective_1 = sum(force_used_1_effective_binary, na.rm = T), counts = n() ) %>%
  adorn_totals() %>%
  mutate(fraction_effective = effective_1/counts)


