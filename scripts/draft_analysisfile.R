# Analysis prototype

# clear environment
rm(list = ls())

# disable scientific notation
options(scipen = 999)

# load packages and helper function
library(tidyverse)
source("scripts/freq_numeric_onevar.R")
source("scripts/freq_dummyvars.R")
source("scripts/freq_selectone_vars.R")
library(srvyr)

# read in cleaned kobo data
kobo_data <- read_csv("data/pre_clean_data.csv") %>% 
  mutate(idp = ifelse(hh_displacement_status == "yes", TRUE, FALSE)) %>% 
  drop_na(idp) %>% 
  filter(idp == FALSE)

# create fake weights

n_vec <-sample(1:100, nrow(kobo_data), replace = TRUE)
weights <- n_vec/sum(n_vec)

kobo_data$weights_var <- weights


# Demographics ------------------------------------------------------------

freq_numeric_onevar(kobo_data, age_respondent)

# info_hh_mem: How do I make sense of that?
kobo_data %>% 
  group_by(info_hh_mem) %>% 
  count()

# age_hh_mem and speak_hh_mem all NAs in Kobo data

freq_selectone_vars(kobo_data, gender, kobo_data) %>% 
  view

freq_numeric_onevar(kobo_data, age_hoh)

freq_selectone_vars(kobo_data, gender_hoh, kobo_data)

freq_dummyvars(kobo_data, "vunerability_type_hh/")

freq_dummyvars(kobo_data, "difficulty_type_hh/")

freq_selectone_vars(kobo_data, seeing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/seeing` == 1)) %>% 
  view

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

movement_dynamics_section <- bind_rows(

freq_selectone_vars(kobo_data, area_of_origin, kobo_data),

freq_selectone_vars(kobo_data, hh_displacement_status, kobo_data),

freq_selectone_vars(kobo_data, hh_member_migration, kobo_data),

freq_dummyvars(kobo_data %>% 
                 filter(hh_member_migration == "yes"),
               name_dummy_variables = "hh_mem_seasonal_migration/"),

freq_dummyvars(kobo_data %>% 
                 filter(hh_member_migration == "yes"),
               name_dummy_variables = "hh_mem_seasonal_migration_01/"),

freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_01/"),

freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_02/"),

freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_03/"),

freq_selectone_vars(kobo_data, hh_currently_hosting_01, kobo_data)

)
