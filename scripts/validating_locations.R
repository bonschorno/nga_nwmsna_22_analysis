library(tidyverse)
library(sf)
library(lubridate)

data_raw <- read_csv("data/pre_clean_data.csv") %>% 
  mutate(state_face = ifelse(state_face == "kastina", "katsina", state_face))

# how many interviews per state?
data_raw %>% 
  group_by(state_face) %>% 
  count(name = "total_per_state") %>% 
  ungroup() %>% 
  mutate(rel_freq = total_per_state/sum(total_per_state))

# duration of interviews
data_raw %>% 
  filter(start_to_end < quantile(data_gps$start_to_end, probs = 0.90)) %>% 
  ggplot() +
  geom_histogram(aes(x = start_to_end)) +
  labs(x = "Duration (in mins)",
       y = "n Inteviews",
       title = "Duration of interviews (highest decile excluded)")

quantile(data_raw$start_to_end, probs = seq(0,1,0.05))
median(data_raw$start_to_end)

# age respondent
data_raw %>% 
  ggplot() +
  geom_histogram(aes(x = age_respondent), bins = 140) +
  labs(x = "\nAge Respondent",
       y = "Number of respondents\n")
#       title = "An example of round number bias: Respondents' age")

data_raw %>% 
  group_by(age_respondent) %>% 
  count() %>% 
  ggplot(aes(x = fct_reorder(as.factor(age_respondent), n), y = n)) +
  geom_col() +
  coord_flip()

data_gps <- data_raw %>% 
  drop_na("_gps_longitude", "_gps_latitude") %>% 
  st_as_sf(coords = c("_gps_longitude", "_gps_latitude"), crs = 4326)

st_geometry(data_gps)

# precision of gps data
data_gps %>% 
  filter(`_gps_precision` < quantile(data_gps$`_gps_precision`, probs = 0.99)) %>% 
  ggplot(aes(x = `_gps_precision`)) +
  geom_histogram(bins = 30)

quantile(data_gps$`_gps_precision`, probs = seq(0,1,0.05))

# load in admin2 boundaries
boundaries <- st_read("data/shapefiles/nga_admbnda_adm2_osgof_20190417.shp") %>% 
  filter(ADM1_EN %in% c("Sokoto", "Katsina", "Zamfara")) %>% 
  select(ADM1_EN, ADM2_EN)

data_augmented <- data_gps %>% 
  st_join(boundaries)

adm2_count <- data_augmented %>% 
  group_by(ADM2_EN) %>% 
  count(name = "count") %>% 
  st_drop_geometry()

boundaries %>% 
  left_join(adm2_count) %>% 
  mutate(present = as.factor(ifelse(is.na(count), 0, 1))) %>% 
  ggplot() +
  geom_sf(aes(fill = present), color = "white")


# Varia -------------------------------------------------------------------

data_raw %>% 
  select(contains("gps"))

enu_ids <- data_raw %>% 
  group_by(enumerator_id) %>% 
  count() 

data_raw %>% 
  select(location_id_face)

names(data_raw)[0:100]

# several vars test
vulnerability <- data_raw %>% 
  select(contains("vunerability_type_hh/"))

names_vulnerability <- names(vulnerability)
names_vulnerability <- sub("vunerability_type_hh/", "", names_vulnerability)

names(vulnerability) <- names_vulnerability

vulnerability %>% 
  pivot_longer(everything(), names_to = "type_vulnerability", values_to = "value") %>% 
  group_by(type_vulnerability) %>% 
  summarise(avg = mean(value, na.rm = TRUE))