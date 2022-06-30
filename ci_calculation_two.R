#' Compute survey-based Confidence Intervals
#'
#' @param df data frame with at least one column: "Category".
#' @param design survey design, normally created with \code{svydesign()} of the survey package.
#' @details The confidence interval for the proportion of each "Category" present in \code{df}
#' is computed using the \code{svymean()} function of the survey package on the given \code{design}.
#' @return data frame containing estimated proportions for each "Category" and respective
#' 95% confidence intervals.
#'
#' ASSUMPTION: the data do not have any missing values
ci_survey <- function(df, design) {
  design.mean = svymean(~stype, design)
  CI = confint( design.mean )
  # Add the estimated proportions and Delta = Semi-size CI for comparison with the CLT-based results below
  CI = cbind( p=design.mean, Delta=( CI[,"97.5 %"] - CI[,"2.5 %"] ) / 2, CI )
  
  return(as.data.frame(CI))
}

#' Compute CLT-based Confidence Intervals
#'
#' @param df data frame with at least two columns: "Category", "Weight" (sampling weights)
#' @details The confidence interval for the proportion of each "Category" present in \code{df}
#' is computed using Lyapunov's or Lindenberg's version of the Central Limit Theorem (CLT) which
#' applies to independent but NOT identically distributed random variables
#' Ref: https://en.wikipedia.org/wiki/Central_limit_theorem#Lack_of_identical_distribution
#' @return data frame containing estimated proportions for each "Category" and respective
#' \code{ci_level} confidence intervals.
#'
#' ASSUMPTION: the data do not have any missing values
ci_clt <- function(df, ci_level) {
  ### The following calculations are based on the following setup for each category given in 'df':
  ### 1) Let X(i) be the measurement of variable X for sampled case i, i = 1 ... n (n=500 in this case)
  ### where X is a 0/1 variable indicating absence or presence of a selected category.
  ### From the X(i) samples we would like to estimate the
  ### true proportion p of the presence of the category in the population.
  ### Therefore X(i) are iid random variables with Binomial(1,p) distribution
  ###
  ### 2) Let Y(i) = w(i)*X(i)
  ### where w(i) is the sampling weight applied to variable X(i).
  ###
  ### We apply the CLT to the sum of the Y(i)'s, using:
  ### - E(Y(i)) = mu(i) = w(i) * E(X(i)) = w(i) * p (since w(i) is a constant and the X(i) are identically distributed)
  ### - Var(Y(i)) = sigma2(i) = w(i)^2 * Var(X(i)) = w(i)^2 * p*(1-p) (since the X(i) iid)
  ###
  ### Hence, by CLT:
  ###   Sum{Y(i) - mu(i)} / sigma -> N(0,1)
  ### where:
  ###   sigma = sqrt( Sum{ sigma2(i) } ) = sqrt( Sum{ w(i)^2 } ) * sqrt( p*(1-p) )
  ### and note that:
  ###   Sum{ mu(i) } = Sum{ w(i) } * p = n*p
  ### since the sampling weights are assumed to sum up to the sample size.
  ###
  ### Note: all the Sums are from i = 1, ..., n
  ###
  ### 3) Compute the approximate confidence interval for p based on the N(0,1) distribution
  ### in the usual way, by first estimating sigma replacing p for the estimated p.
  ###
  
  alpha = 1 - ci_level                                         # area outside the confidence band
  z = qnorm(1 - alpha/2)                                       # critical z-quantile from Normal(0,1)
  
  n = nrow(df)                                                 # Sample size (assuming no missing values)
  ws = df$pw / sum(df$pw) * n                          # Weights scaled to sum the sample size (assumed for sampling weights)
  S = aggregate(ws ~ stype, sum, data=df)                   # Weighted-base estimate of the total by category (Sum{ Y(i) })
  sigma2 = sum( ws^2 )                                         # Sum of squared weights (note that we must NOT sum by category)
  S[,"p"] = S[,"ws"] / n                                       # Estimated proportion by category
  S[,"Delta"] =  z * sqrt( sigma2 ) *
    sqrt( S$p * (1 - S$p) ) / n               # Semi-size of the CI by category
  LB_name = paste(formatC(alpha/2*100, format="g"), "%")       # Name for the CI's Lower Bound column
  UB_name = paste(formatC((1 - alpha/2)*100, format="g"), "%") # Name for the CI's Upper Bound column
  S[,LB_name] = S[,"p"] - S[,"Delta"]                          # CI's Lower Bound
  S[,UB_name] = S[,"p"] + S[,"Delta"]                          # CI's Upper Bound
  
  return(S)
}

#' Show the CI with the specified number of significant digits
show_values <- function(values, digits=3) {
  op = options(digits=digits)
  print(values)
  options(op)
}

# Computation of CI using survey sampling theory (implemented in the survey package)
library(survey)
data(api)

design <- svydesign(ids = ~1, weights = ~pw, data = apistrat)
CI_survey = ci_survey(apistrat, design)
show_values(CI_survey)

# Computation of CI using the Central Limit Theorem for non-identically distributed variables
CI_clt = ci_clt(apistrat, ci_level=0.95)
show_values(CI_clt)

# make srvyr equal to survey------

#survey

design <- svydesign(ids = ~1, weights = ~pw, data = apistrat)
design.mean = svymean(~stype, design)
CI = confint( design.mean )
# Add the estimated proportions and Delta = Semi-size CI for comparison with the CLT-based results below
# CI = cbind( p=design.mean, Delta=( CI[,"97.5 %"] - CI[,"2.5 %"] ) / 2, CI )
# CI
design.mean

# srvyr

apistrat %>% 
  as_survey_design(ids = 1, weights = pw) %>% 
  group_by(stype) %>%
  summarise(prop = survey_mean())
