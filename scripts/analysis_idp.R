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


# 1. Demographics ------------------------------------------------------------

demographics_battery <- bind_rows(

idp_freq_numeric_onevar(kobo_data, age_respondent),

# info_hh_mem: How do I make sense of that?
# kobo_data %>% 
#   group_by(info_hh_mem) %>% 
#   count()

# age_hh_mem and speak_hh_mem all NAs in Kobo data

idp_freq_selectone_vars(kobo_data, gender, kobo_data),

idp_freq_numeric_onevar(kobo_data, age_hoh),

idp_freq_selectone_vars(kobo_data, gender_hoh, kobo_data),

idp_freq_dummyvars(kobo_data, "vunerability_type_hh/"),

idp_freq_dummyvars(kobo_data, "difficulty_type_hh/"),

idp_freq_selectone_vars(kobo_data, seeing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/seeing` == 1)),

idp_freq_selectone_vars(kobo_data, hearing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/hearing` == 1)),

idp_freq_selectone_vars(kobo_data, walking_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/walking` == 1)),

# throwing an error
# idp_freq_selectone_vars(kobo_data, remembering_extent_difficulty, kobo_data %>% 
#                       filter(`difficulty_type_hh/remembering` == 1)),

idp_freq_selectone_vars(kobo_data, dressing_extent_difficulty, kobo_data %>% 
                      filter(`difficulty_type_hh/selfcare` == 1)),

# deactivated because too little data
# idp_freq_selectone_vars(kobo_data, communication_extent_difficulty, kobo_data %>% 
#                       filter(`difficulty_type_hh/communication` == 1)),

idp_freq_dummyvars(kobo_data, "hh_situation/"),

idp_freq_dummyvars(kobo_data, "education_level_hoh/")

)

# Loop for individual household members is still missing

# 2. Movement Dynamics -------------------------------------------------------

movement_dynamics_battery <- bind_rows(
  
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

# 3. Early Recovery and Livelihoods ----

early_recovery_livelihood_battery <- bind_rows(

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

# 4. Food Security and Nutrition ---------------------------------------------

# Questions from breast_milk_yesterday to feeding_programme not coded so far as they depend on a loop

food_security_nutrition_battery <- bind_rows(

idp_freq_selectone_vars(kobo_data, prim_source_of_food, kobo_data),

idp_freq_selectone_vars(kobo_data, sec_source_of_food, kobo_data),

idp_freq_dummyvars(kobo_data, "diff_or_shocks/"),

idp_freq_selectone_vars(kobo_data, shock_reduced_ability, kobo_data %>% filter(`diff_or_shocks/no_shocks` == 0)),

idp_freq_selectone_vars(kobo_data, shock_impact_ability, kobo_data %>% filter(`diff_or_shocks/no_shocks` == 0)),

idp_freq_selectone_vars(kobo_data, hoh_own_farm_animal, kobo_data),

idp_freq_selectone_vars(kobo_data, hoh_number_farm_animal, kobo_data %>% filter(hoh_own_farm_animal == "yes")),

idp_freq_dummyvars(kobo_data %>% 
                     filter(hoh_own_farm_animal == "yes" & hoh_number_farm_animal %in% c("minor_decrease", "major_decrease")), 
                   "hoh_livestock_decrease/"),

idp_freq_selectone_vars(kobo_data, land_access_for_cultivation, kobo_data),

idp_freq_selectone_vars(kobo_data, reason_for_nonaccess, kobo_data %>% 
                          filter(land_access_for_cultivation == "no")),

idp_freq_selectone_vars(kobo_data, planting_coming_season, kobo_data %>% 
                          filter(land_access_for_cultivation == "yes")),

idp_freq_selectone_vars(kobo_data, reason_for_not_planting, kobo_data %>% 
                          filter(land_access_for_cultivation == "yes",
                                 planting_coming_season == "no")),

idp_freq_selectone_vars(kobo_data, food_of_anykind, kobo_data),

idp_freq_selectone_vars(kobo_data, often_eat_nothing, kobo_data %>% 
                          filter(food_of_anykind == "yes"))

)

# 5. Health ------------------------------------------------------------------

health_battery <- bind_rows(

# child_recieve_vaccination and many_times_recieve missing due to loop

idp_freq_selectone_vars(kobo_data, nearest_health_care, kobo_data),

idp_freq_selectone_vars(kobo_data, nearest_health_care_children, kobo_data),

idp_freq_selectone_vars(kobo_data, nearest_health_care_adult, kobo_data),

idp_freq_selectone_vars(kobo_data, current_location_healthcare, kobo_data),

# again, a lot of variables missing due to loop

idp_freq_selectone_vars(kobo_data, heard_of_covid, kobo_data),

idp_freq_dummyvars(kobo_data %>% filter(heard_of_covid == "yes"), 
                  name_dummy_variables = "covid_action_taken/"),

idp_freq_dummyvars(kobo_data %>% filter(heard_of_covid == "yes"), 
                   name_dummy_variables = "mem_with_covid/"),

idp_freq_selectone_vars(kobo_data, covid_vaccinne_availability, kobo_data %>% 
                     filter(heard_of_covid == "yes")),

idp_freq_selectone_vars(kobo_data, adult_take_vaccine, kobo_data %>% 
                          filter(heard_of_covid == "yes",
                                 covid_vaccinne_availability == "yes"))

)

# 6. WASH --------------------------------------------------------------------

wash_battery <- bind_rows(

idp_freq_numeric_onevar(kobo_data, water_used_day),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "main_source_water/"),

idp_freq_selectone_vars(kobo_data, time_taken_to_fetch, kobo_data),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "enough_water_needs/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "problems_water_access/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "cope_lack_water/"),

idp_freq_selectone_vars(kobo_data, sanitation_facility_hh, kobo_data),

idp_freq_selectone_vars(kobo_data, share_sanitation_facility, kobo_data),

idp_freq_numeric_onevar(kobo_data %>% 
                          filter(share_sanitation_facility == "yes"), number_hh_share_facility),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "latrine_fautures_hh/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "problems_sanitation_facicility/"),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "adaptation_to_sanitation/"),

idp_freq_selectone_vars(kobo_data, traces_dead_animals, kobo_data),

idp_freq_selectone_vars(kobo_data, soap_in_hh, kobo_data),

idp_freq_selectone_vars(kobo_data, handwashing_facility, kobo_data),

idp_freq_dummyvars(kobo_data, name_dummy_variables = "lack_of_hygiene_items/")

# menstrual questions missing due to loop

)


# 7. Shelter/NFI -------------------------------------------------------------

shelter_battery <- bind_rows(

idp_freq_selectone_vars(kobo_data, type_of_shelter, kobo_data),

idp_freq_dummyvars(kobo_data %>% 
                     filter(!is.na(shelter_damages_defects)), 
                   name_dummy_variables = "shelter_damages_defects/"), 

idp_freq_dummyvars(kobo_data, "shelter_enclosure_issues/"),

idp_freq_dummyvars(kobo_data, "issues_in_shelter/"),

idp_freq_selectone_vars(kobo_data, occupancy_arrangement_currently, kobo_data),

idp_freq_selectone_vars(kobo_data, documentation_prove_occupancy, kobo_data),

idp_freq_dummyvars(kobo_data, "problems_relates_housing/"),

idp_freq_dummyvars(kobo_data, "personal_items_needs/"),

idp_freq_dummyvars(kobo_data, "general_items_needs/"),

idp_freq_selectone_vars(kobo_data, source_cooking_fuel, kobo_data),

idp_freq_selectone_vars(kobo_data, main_source_electricity, kobo_data),

idp_freq_selectone_vars(kobo_data, hours_of_electricity, kobo_data)

)


# 8. Protection --------------------------------------------------------------

protection_battery <- bind_rows(

idp_freq_selectone_vars(kobo_data, members_affected_security, kobo_data),

idp_freq_dummyvars(kobo_data %>% 
                     filter(members_affected_security == "yes"), "nature_safety_incident/"),


idp_freq_numeric_onevar(kobo_data %>% 
                          filter(members_affected_security == "yes"), hh_members_affected_security),


idp_freq_selectone_vars(kobo_data, damage_stolen_property, kobo_data),

idp_freq_selectone_vars(kobo_data, child_protection_services, kobo_data),

# idp_freq_selectone_vars(kobo_data, over_18_signs_distress, kobo_data) missing due to loop

idp_freq_selectone_vars(kobo_data, harm_physical_threaths, kobo_data),

idp_freq_dummyvars(kobo_data, "access_law_enforement_authorities/"),

idp_freq_selectone_vars(kobo_data, hh_passport_id, kobo_data),

idp_freq_numeric_onevar(kobo_data, other_missing_id),

idp_freq_selectone_vars(kobo_data, members_birth_certificate, kobo_data),

idp_freq_numeric_onevar(kobo_data, missing_birth_certificate),

idp_freq_numeric_onevar(kobo_data, children_working)

#under_18_employment missing due to loop

)


# 9. Education ------------------------------------------------------------

education_battery <- bind_rows(

# filtering conditions not applied yet due to loop

idp_freq_dummyvars(kobo_data, "children_access_learning_materials/"),

idp_freq_selectone_vars(kobo_data, protection_incident_school, kobo_data),

idp_freq_numeric_onevar(kobo_data, sum_school_age_boys),

idp_freq_numeric_onevar(kobo_data, sum_school_age_girls),

idp_freq_selectone_vars(kobo_data, children_stop_schooling, kobo_data),

idp_freq_dummyvars(kobo_data, "barriers_boys_faced/"),

idp_freq_dummyvars(kobo_data, "barriers_girls_faced/"),

idp_freq_dummyvars(kobo_data, "support_regular_learning/"),

idp_freq_dummyvars(kobo_data, "support_home_learning/")

)


# 10. AAP & Communication -------------------------------------------------

time_period_assistance <- c("6mo_to_year", "last_6_months", "last_30_days", "above_year")

aap_communication_battery <- bind_rows(

idp_freq_selectone_vars(kobo_data, assistance_received_yn, kobo_data),

idp_freq_dummyvars(kobo_data %>% 
                     filter(assistance_received_yn %in% time_period_assistance), "hh_assitance_recieved/"),

idp_freq_dummyvars(kobo_data %>% 
                     filter(!is.na(hh_recieved_assistance_from)), "hh_recieved_assistance_from/"),

# didn't want to comment again on this issue after all the progress was lost

idp_freq_selectone_vars(kobo_data, satisfied_with_assistance, 
                        kobo_data %>% filter(assistance_received_yn %in% time_period_assistance)),

idp_freq_dummyvars(kobo_data %>% filter(satisfied_with_assistance == "no"), "why_not_satisfied_assistance/"),

idp_freq_selectone_vars(kobo_data, feedback_from_provider, kobo_data),

idp_freq_selectone_vars(kobo_data, hh_recieved_response, kobo_data %>% filter(feedback_from_provider == "yes")),

idp_freq_selectone_vars(kobo_data, hh_taken_consideration, kobo_data %>% filter(feedback_from_provider == "yes")),

idp_freq_dummyvars(kobo_data, "agencies_about_assistance/"),

idp_freq_dummyvars(kobo_data, "barriers_humanitarian_assistance/"),

idp_freq_selectone_vars(kobo_data, assistance_like_recieve, kobo_data),

idp_freq_dummyvars(kobo_data, "type_assistance_futrure/"),

idp_freq_dummyvars(kobo_data %>% 
                     filter(str_detect(type_assistance_futrure, "food|nfi|shelter_materials|sanitation") == TRUE),
                   "prefers_material_assistance/"),

idp_freq_dummyvars(kobo_data %>% 
                     filter(str_detect(type_assistance_futrure, "cash_prep|cash_mobile|physical_cash") == TRUE),
                   "prefers_cash_assistance/"),

idp_freq_dummyvars(kobo_data %>% 
                     filter(str_detect(type_assistance_futrure, "vouchers") == TRUE),
                   "prefers_vouchers_assistance/"),

idp_freq_dummyvars(kobo_data, "info_asssitance_providers/"),

idp_freq_dummyvars(kobo_data, "prefer_info_assistance/"),

idp_freq_dummyvars(kobo_data, "prefer_info_means/"),

idp_freq_selectone_vars(kobo_data, main_language_home, kobo_data),

idp_freq_selectone_vars(kobo_data, literate_speak_write, kobo_data),

idp_freq_selectone_vars(kobo_data, which_language, kobo_data %>% filter(literate_speak_write == "yes")),

idp_freq_selectone_vars(kobo_data, mainly_language_service_providers, kobo_data),

idp_freq_selectone_vars(kobo_data, preferred_language_service_providers, kobo_data),

idp_freq_selectone_vars(kobo_data, written_info_language, kobo_data),

idp_freq_selectone_vars(kobo_data, preferred_language_spoken_info, kobo_data),

idp_freq_selectone_vars(kobo_data, hh_first_priority_need, kobo_data),

idp_freq_selectone_vars(kobo_data, hh_second_priority_need, kobo_data),

idp_freq_selectone_vars(kobo_data, hh_third_priority_need, kobo_data)

)


# Complete IDP dataset ----------------------------------------------------

idp_dataset <- bind_rows(demographics_battery,
          movement_dynamics_battery,
          early_recovery_livelihood_battery,
          food_security_nutrition_battery,
          health_battery,
          wash_battery,
          shelter_battery,
          protection_battery,
          education_battery,
          aap_communication_battery)

rm(demographics_battery,
   movement_dynamics_battery,
   early_recovery_livelihood_battery,
   food_security_nutrition_battery,
   health_battery,
   wash_battery,
   shelter_battery,
   protection_battery,
   education_battery,
   aap_communication_battery)

# change column order
col_order <- c("question_name", "answer", "state_face", "value",
               "ci_low", "ci_upp", "moe", "total_subgroup", "total_participants")

idp_dataset <- idp_dataset[, col_order]

idp_dataset <- idp_dataset %>% 
  mutate(answer = ifelse(is.na(answer), "numeric", answer))

write_csv(idp_dataset, "output/final_dataset_idp.csv")


# To do -----------------------------------------------------------------

# add battery names
# clean answers and figure out a solution how to feed in the cleaned answers


length(unique(idp_dataset$answer))

unique(idp_dataset$answer)

