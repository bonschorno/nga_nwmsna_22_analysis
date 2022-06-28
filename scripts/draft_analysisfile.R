# Analysis prototype

# clear environment
rm(list = ls())

# disable scientific notation
options(scipen = 999)

# load packages and helper function
library(tidyverse)
source("freq_numeric_onevar.R")
source("freq_dummyvars.R")
source("freq_selectone.R")
library(srvyr)

# read in cleaned kobo data
kobo_data <- read_csv("datavalidation_report/pre_clean_data.csv") %>% 
  mutate(idp = ifelse(hh_displacement_status == "yes", "IDP", "Non-IDP")) %>% 
  drop_na(idp) %>% 
  filter(idp == "Non-IDP")

# create fake weights

n_vec <-sample(1:100, nrow(kobo_data), replace = TRUE)
weights <- n_vec/sum(n_vec)

kobo_data$weights_var <- weights

# start of analysis


# Demographics ------------------------------------------------------------

freq_numeric_onevar(kobo_data, age_respondent)

# info_hh_mem: How do I make sense of that?
kobo_data %>% 
  group_by(info_hh_mem) %>% 
  count()

# age_hh_mem and speak_hh_mem NAs in Kobo data

freq_selectone_vars(kobo_data, gender, kobo_data)

freq_numeric_onevar(kobo_data, age_hoh)

freq_selectone_vars(kobo_data, gender_hoh, kobo_data)

freq_dummyvars(kobo_data, "vunerability_type_hh/")

freq_dummyvars(kobo_data, "difficulty_type_hh/")

freq_selectone_vars(kobo_data, seeing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/seeing` == 1))

freq_selectone_vars(kobo_data, hearing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/hearing` == 1))

freq_selectone_vars(kobo_data, walking_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/walking` == 1))

freq_selectone_vars(kobo_data, remembering_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/remembering` == 1))

freq_selectone_vars(kobo_data, dressing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/selfcare` == 1))

freq_selectone_vars(kobo_data, communication_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/communication` == 1))

freq_dummyvars(kobo_data, "hh_situation/")

freq_dummyvars(kobo_data, "education_level_hoh/")

# Loop for individual household members is still missing


# Movement Dynamics -------------------------------------------------------

freq_selectone_vars(kobo_data, area_of_origin, kobo_data) %>% 
  view

freq_selectone_vars(kobo_data, origin_nigeria_outside, kobo_data %>% 
                      filter(area_of_origin == "no")) 

freq_selectone_vars(kobo_data, state_origin_hh, kobo_data %>% 
                      filter(area_of_origin == "no")) 


