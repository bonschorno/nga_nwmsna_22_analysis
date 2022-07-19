# Analysis prototype

# clear environment
rm(list = ls())

# disable scientific notation
options(scipen = 999)

# load packages and helper function
library(tidyverse)
source("scripts/idp_freq_numeric_onevar.R")
source("scripts/idp_freq_dummyvars.R")
source("scripts/idp_freq_selectone_vars.R")
library(srvyr)

# read in cleaned kobo data
kobo_data <- read_csv("data/pre_clean_data.csv") %>% 
  mutate(idp = ifelse(hh_displacement_status == "yes", TRUE, FALSE)) %>% 
  drop_na(idp) %>% 
  filter(idp == TRUE)

# create fake weights
n_vec <-sample(1:100, nrow(kobo_data), replace = TRUE)
weights <- n_vec/sum(n_vec)

kobo_data$weights_var <- weights


# Demographics ------------------------------------------------------------

idp_freq_numeric_onevar(kobo_data, age_respondent)

# info_hh_mem: How do I make sense of that?
kobo_data %>% 
  group_by(info_hh_mem) %>% 
  count()

# age_hh_mem and speak_hh_mem all NAs in Kobo data

idp_freq_selectone_vars(kobo_data, gender, kobo_data)

idp_freq_numeric_onevar(kobo_data, age_hoh)

idp_freq_selectone_vars(kobo_data, gender_hoh, kobo_data)

idp_freq_dummyvars(kobo_data, "vunerability_type_hh/") %>% 
  view

idp_freq_dummyvars(kobo_data, "difficulty_type_hh/")

idp_freq_selectone_vars(kobo_data, seeing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/seeing` == 1)) %>% 
  view

idp_freq_selectone_vars(kobo_data, hearing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/hearing` == 1))

idp_freq_selectone_vars(kobo_data, walking_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/walking` == 1))

idp_freq_selectone_vars(kobo_data, remembering_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/remembering` == 1))

idp_freq_selectone_vars(kobo_data, dressing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/selfcare` == 1))

idp_freq_selectone_vars(kobo_data, communication_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/communication` == 1))

idp_freq_dummyvars(kobo_data, "hh_situation/")

idp_freq_dummyvars(kobo_data, "education_level_hoh/")

# Loop for individual household members is still missing


# Movement Dynamics -------------------------------------------------------

movement_dynamics_section <- bind_rows(
  
  idp_freq_selectone_vars(kobo_data, area_of_origin, kobo_data),
  
  idp_freq_selectone_vars(kobo_data, origin_nigeria_outside, kobo_data %>% 
                        filter(area_of_origin == "no")),
  
  idp_freq_selectone_vars(kobo_data, state_origin_hh, kobo_data %>% 
                        filter(area_of_origin == "no")),
  
  # do we include this one? -> a lot of NAs
  # freq_selectone_vars(kobo_data, lga_original_hh, kobo_data %>% 
  #                       filter(!is.na(lga_original_hh))) %>% 
  #   view
  
  # same here
  # freq_selectone_vars(kobo_data, ward_of_origin, kobo_data) %>% 
  #   view
  
  idp_freq_selectone_vars(kobo_data, hh_displacement_status, kobo_data),
  
  idp_freq_selectone_vars(kobo_data, times_hh_displaced, kobo_data %>% 
                        filter(hh_displacement_status == "yes")),
  
  # to be cleaned: year_first_displacement & year_first_displacement_recent
  
  #lga_displacement_from: Again, a lot of empty values
  
  idp_freq_dummyvars(kobo_data %>% filter(idp == TRUE), "reaseon_for_displacement/"),
  
  idp_freq_dummyvars(kobo_data %>% filter(idp == TRUE), "current_area_displaced/"),
  
  idp_freq_selectone_vars(kobo_data, mem_family_sep, kobo_data %>% filter(idp == TRUE)),
  
  idp_freq_selectone_vars(kobo_data, hh_mem_reuninted, kobo_data %>% filter(idp == TRUE,
                                                                        mem_family_sep == "yes")),
  
  idp_freq_selectone_vars(kobo_data, area_of_origin_visited, kobo_data %>% filter(idp == TRUE)),
  
  idp_freq_selectone_vars(kobo_data, visit_taken_place, kobo_data %>% filter(idp == TRUE,
                                                                         area_of_origin == "yes")),
  
  idp_freq_dummyvars(kobo_data %>% filter(idp == TRUE,
                                      area_of_origin == "yes"),
                 "visit_return_history/"),
  
  idp_freq_selectone_vars(kobo_data, hh_member_returned, kobo_data %>% filter(idp == TRUE)),
  
  idp_freq_dummyvars(kobo_data %>% filter(idp == TRUE,
                                      hh_member_returned == "yesreturn"),
                 "primary_hh_return/"),
  
  idp_freq_dummyvars(kobo_data %>% filter(idp == TRUE,
                                      hh_member_returned == "yesresettled"),
                 "resettle_hh_after_displacement/"),
  
  idp_freq_selectone_vars(kobo_data, times_hh_new_location, kobo_data %>%  
                        filter(idp == TRUE,
                               hh_member_returned %in% c("yesreturn", "yesresettled"))),
  
  idp_freq_selectone_vars(kobo_data, visit_returned_history, kobo_data %>% filter(idp == TRUE)),
  
  idp_freq_selectone_vars(kobo_data, future_migration_hh, kobo_data %>% filter(idp == TRUE)),
  
  idp_freq_selectone_vars(kobo_data, hh_plan_retured, 
                      kobo_data %>% filter(idp == TRUE,
                                           future_migration_hh %in% c("intend_to_return", "intend_to_resettle"))),
  
  idp_freq_selectone_vars(kobo_data, hh_member_migration, kobo_data),
  
  idp_freq_dummyvars(kobo_data %>% 
                   filter(hh_member_migration == "yes"),
                 name_dummy_variables = "hh_mem_seasonal_migration/"),
  
  idp_freq_dummyvars(kobo_data %>% 
                   filter(hh_member_migration == "yes"),
                 name_dummy_variables = "hh_mem_seasonal_migration_01/"),
  
  idp_freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_01/"),
  
  idp_freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_02/"),
  
  idp_freq_dummyvars(kobo_data, name_dummy_variables = "mobility_restriction_03/"),
  
  idp_freq_selectone_vars(kobo_data, hh_currently_hosting_01, kobo_data),
  
  idp_freq_selectone_vars(kobo_data, hh_currently_hosting_02, kobo_data %>% 
                        filter(hh_currently_hosting_01 == "yes"))
  
)

# Early Recovery and Livelihoods ----

bind_rows(

idp_freq_dummyvars(kobo_data, name_dummy_variables = "source_of_income/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "secondary_income/"),

idp_freq_selectone_vars(kobo_data, overall_income_estimate, kobo_data),
  
idp_freq_selectone_vars(kobo_data, own_asset_hh, kobo_data),

idp_freq_numeric_onevar(kobo_data, expenditure_shelter_repair),

idp_freq_numeric_onevar(kobo_data, expenditure_non_food_items),

idp_freq_numeric_onevar(kobo_data, expenditure_health_related),

idp_freq_numeric_onevar(kobo_data, expenditure_education_related),

idp_freq_numeric_onevar(kobo_data, expenditure_debt_related),

idp_freq_numeric_onevar(kobo_data, expenditure_other_needs),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "earn_income_month/"),

idp_freq_selectone_vars(kobo_data, challenges_faced_enough_money, kobo_data),

idp_freq_dummyvars(kobo_data %>% filter(challenges_faced_enough_money == "yes"), 
                   name_dummy_variables = "challenges_faced_hh_needs/"),


idp_freq_dummyvars(kobo_data %>% filter(challenges_faced_enough_money == "yes"), 
                   name_dummy_variables = "lack_income_cope/"),

idp_freq_selectone_vars(kobo_data, market_access_foot, kobo_data),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "social_physical_barriers/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "public_facility_services/"),

idp_freq_numeric_onevar(kobo_data %>% filter(expenditure_debt_related > 0),
                        amount_of_debt),

# filter condition in the DAP seems to be off -> clarify.
idp_freq_dummyvars(kobo_data, name_dummy_variables = "reason_for_debt/")

)

# Food Security and Nutrition ---------------------------------------------

# Questions from breast_milk_yesterday to feeding_programme not coded so far as they depend on a loop

idp_freq_selectone_vars(kobo_data, prim_source_of_food, kobo_data) %>% 
  view

idp_freq_selectone_vars(kobo_data, sec_source_of_food, kobo_data)

idp_freq_dummyvars(kobo_data, "diff_or_shocks/")

idp_freq_selectone_vars(kobo_data, shock_reduced_ability, kobo_data %>% filter(`diff_or_shocks/no_shocks` == 0))

idp_freq_selectone_vars(kobo_data, shock_impact_ability, kobo_data %>% filter(`diff_or_shocks/no_shocks` == 0))

idp_freq_selectone_vars(kobo_data, hoh_own_farm_animal, kobo_data)

idp_freq_selectone_vars(kobo_data, hoh_number_farm_animal, kobo_data %>% filter(hoh_own_farm_animal == "yes"))

idp_freq_dummyvars(kobo_data %>% 
                     filter(hoh_own_farm_animal == "yes" & hoh_number_farm_animal %in% c("minor_decrease", "major_decrease")), 
                   "hoh_livestock_decrease/")

idp_freq_selectone_vars(kobo_data, land_access_for_cultivation, kobo_data)

idp_freq_selectone_vars(kobo_data, reason_for_nonaccess, kobo_data %>% 
                          filter(land_access_for_cultivation == "no"))

idp_freq_selectone_vars(kobo_data, planting_coming_season, kobo_data %>% 
                          filter(land_access_for_cultivation == "yes"))

idp_freq_selectone_vars(kobo_data, reason_for_not_planting, kobo_data %>% 
                          filter(land_access_for_cultivation == "yes",
                                 planting_coming_season == "no"))

idp_freq_selectone_vars(kobo_data, food_of_anykind, kobo_data)

idp_freq_selectone_vars(kobo_data, often_eat_nothing, kobo_data %>% 
                          filter(food_of_anykind == "yes"))


# Health ------------------------------------------------------------------

# child_recieve_vaccination and many_times_recieve missing due to loop


idp_freq_selectone_vars(kobo_data, nearest_health_care, kobo_data)

idp_freq_selectone_vars(kobo_data, nearest_health_care_children, kobo_data)

idp_freq_selectone_vars(kobo_data, nearest_health_care_adult, kobo_data)

idp_freq_selectone_vars(kobo_data, current_location_healthcare, kobo_data)

# again, a lot of variables missing due to loop

idp_freq_selectone_vars(kobo_data, heard_of_covid, kobo_data) %>% 
  view

idp_freq_dummyvars(kobo_data %>% filter(heard_of_covid == "yes"), 
                  name_dummy_variables = "covid_action_taken/")

idp_freq_dummyvars(kobo_data %>% filter(heard_of_covid == "yes"), 
                   name_dummy_variables = "mem_with_covid/")

idp_freq_selectone_vars(kobo_data, covid_vaccinne_availability, kobo_data %>% 
                     filter(heard_of_covid == "yes"))

idp_freq_selectone_vars(kobo_data, adult_take_vaccine, kobo_data %>% 
                          filter(heard_of_covid == "yes",
                                 covid_vaccinne_availability == "yes"))



# WASH --------------------------------------------------------------------

idp_freq_numeric_onevar(kobo_data, water_used_day)

idp_freq_dummyvars(kobo_data, name_dummy_variables = "main_source_water/")

idp_freq_selectone_vars(kobo_data, time_taken_to_fetch, kobo_data)

idp_freq_dummyvars(kobo_data, name_dummy_variables = "enough_water_needs/")

idp_freq_dummyvars(kobo_data, name_dummy_variables = "problems_water_access/")

idp_freq_dummyvars(kobo_data, name_dummy_variables = "cope_lack_water/")

idp_freq_selectone_vars(kobo_data, sanitation_facility_hh, kobo_data)

idp_freq_selectone_vars(kobo_data, share_sanitation_facility, kobo_data)

idp_freq_numeric_onevar(kobo_data %>% 
                          filter(share_sanitation_facility == "yes"), number_hh_share_facility)

idp_freq_dummyvars(kobo_data, name_dummy_variables = "latrine_fautures_hh/")

idp_freq_dummyvars(kobo_data, name_dummy_variables = "problems_sanitation_facicility/")

idp_freq_dummyvars(kobo_data, name_dummy_variables = "adaptation_to_sanitation/")

idp_freq_selectone_vars(kobo_data, traces_dead_animals, kobo_data)

idp_freq_selectone_vars(kobo_data, soap_in_hh, kobo_data)

idp_freq_selectone_vars(kobo_data, handwashing_facility, kobo_data)

idp_freq_dummyvars(kobo_data, name_dummy_variables = "lack_of_hygiene_items/")

# menstrual questions missing due to loop

