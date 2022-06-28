# Create map

extrafont::loadfonts()
library(tidyverse)
library(sf)
source("calc_margins.R")

extrafont::fonts()

raw_data <- read_csv("datavalidation_report/pre_clean_data.csv")

lga_shapefile <- st_read("datavalidation_report/shapefiles/new_lga_nigeria_2003.shp") %>% 
  filter(STATE %in% c("Sokoto", "Katsina", "Zamfara")) %>% 
  mutate(STATE = tolower(STATE),
         LGA = tolower(LGA)) %>% 
  rename(lga_face = LGA)

names_shapefile <- unique(lga_shapefile$lga_face)
names_shapefile <- gsub(pattern = " ", x = names_shapefile, replacement = "_")
names_shapefile <- gsub(pattern = "/", x = names_shapefile, replacement = "_")
names_shapefile <- gsub(pattern = "-", x = names_shapefile, replacement = "_")

# setdiff(names_kobo, names_shapefile)

hours_electricity <- create_freq_table_selectone(raw_data, hours_of_electricity) %>% 
  filter(answer == "1_2_hrs")

# map 

g <- lga_shapefile %>% 
  left_join(hours_electricity) %>% 
  filter(answer == "1_2_hrs" |  is.na(answer)) %>% 
  ggplot() +
  geom_sf(aes(fill = frequency), color = "white") +
  labs(title = "Title",
       subtitle = "Subtitle",
       x = "",
       y = "") +
  #scale_fill_gradient(low = "#ae10f9", high = "#e2db1f", labels = scales::percent) +
  scale_fill_binned(low = "#EE5859", high = "#161032", labels = scales::percent,
                    breaks = seq(0,0.5,.1),
                    guide = guide_coloursteps(even.steps = FALSE)) +
  #guides(fill = guide_colorsteps()) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(family = "Arial Narrow"),
        panel.grid = element_line(linetype = "dotted"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold"))

g

#plotly::ggplotly(g)
