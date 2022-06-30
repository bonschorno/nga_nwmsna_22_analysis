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

apistrat %>% 
  as_survey_design(ids = 1, weights = pw) %>% 
  group_by(stype) %>%
  summarise(prop = survey_mean(proportion = TRUE, vartype = c("se", "ci"), deff = TRUE))



##############


alpha = 1 - 0.95                                         # area outside the confidence band
z = qnorm(1 - alpha/2)                                       # critical z-quantile from Normal(0,1)

n = nrow(apistrat)                                                 # Sample size (assuming no missing values)
ws = apistrat$pw / sum(apistrat$pw) * n                          # Weights scaled to sum the sample size (assumed for sampling weights)
S = aggregate(ws ~ stype, sum, data=apistrat)                   # Weighted-base estimate of the total by category (Sum{ Y(i) })
sigma2 = sum( ws^2 )                                         # Sum of squared weights (note that we must NOT sum by category)
S[,"p"] = S[,"ws"] / n                                       # Estimated proportion by category
S[,"Delta"] =  z * sqrt( sigma2 ) *
  sqrt( S$p * (1 - S$p) ) / n               # Semi-size of the CI by category
LB_name = paste(formatC(alpha/2*100, format="g"), "%")       # Name for the CI's Lower Bound column
UB_name = paste(formatC((1 - alpha/2)*100, format="g"), "%") # Name for the CI's Upper Bound column
S[,LB_name] = S[,"p"] - S[,"Delta"]                          # CI's Lower Bound
S[,UB_name] = S[,"p"] + S[,"Delta"]                          # CI's Upper Bound
S

############


kobo_data %>% 
  as_survey_design(ids = ward_face, weights = weights_var) %>% 
  summarise(prop = survey_prop(vartype = c("se", "ci"), proportion = TRUE))

unique(apistrat$dname)
unique(apistrat$cname)

apistrat %>% 
  group_by(cname) %>% 
  count()
