# Title: Primary analysis for NW MSNA 2022
# Main author: Colin Walder
# Contributors:
# Contact: colin.walder@reach-initiative.org


# Setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
options(scipen = 999)

data_raw <- readxl::read_xlsx("data/NGA2105_HH_MSNA2022_-_latest_version_-_False_R_Clean.xlsx")

data <- data_raw %>% 
  mutate(start = ymd_hms(start),
         end = ymd_hms(end),
         start_to_end = end-start)

# plot duration
data %>% 
  filter(start_to_end < 120) %>% 
  group_by(organisation_of_enumerator) %>% 
  ggplot(aes(x = organisation_of_enumerator, y = as.numeric(start_to_end))) +
  geom_boxplot()

data %>% 
  filter(start_to_end < 120) %>% 
  group_by(lga_face) %>% 
  summarise(mean_duration = mean(start_to_end),
            n_surveys = n()) %>% 
  view()

#how many wards?
unique(data$lga_face)

#how many per enumerator organisation
data %>% 
  group_by(organisation_of_enumerator) %>% 
  count()


# Outlier check for integers ----------------------------------------------

numeric_vars <- data %>% 
  select(where(is.numeric), -id, -contains("_gps"), -location_id_face, -"_id", -"_index") %>% 
  summarise_all(n_distinct) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "unique_values") %>% 
  filter(unique_values > 3) %>% 
  pull(variable)

numeric_vars_lookup <- data %>% 
  select(numeric_vars) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>% 
  group_by(variable) %>% 
  summarise(mean = mean(value, na.rm = TRUE))
  
  
# lower bound

lower_bound <- as.numeric(quantile(data["age_hoh"], 0.025, na.rm = TRUE))

length(unique(data$prefer_info_means_tel))
