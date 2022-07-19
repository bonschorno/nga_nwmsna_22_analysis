library(tidyverse)
library(srvyr)
library(survey)

data(api)

?apistrat

weighted_proportions <- apistrat %>% 
  as_survey_design(ids = dname, weights = pw) %>% 
  group_by(cname, stype) %>% 
  summarise(prop = survey_prop(vartype = c("se", "ci"), proportion = TRUE),
            total_subgroup = n())

unweighted_proportions <- apistrat %>% 
  group_by(cname, stype) %>% 
  count() %>% 
  group_by(cname) %>% 
  mutate(prop = n/sum(n))

# awards

# unweighted
apistrat %>% 
  group_by(stype) %>%
  count() %>% 
  ungroup() %>% 
  mutate(prop = n/sum(n))

# weighted
apistrat %>% 
  as_survey_design(ids = 1, weights = pw) %>% 
  group_by(stype) %>%
  summarise(prop = survey_prop(proportion = TRUE, vartype = c("se", "ci")))