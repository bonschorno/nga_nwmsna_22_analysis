idp_freq_numeric_onevar <- function(df, variable_name){
  
  varname <- rlang::as_name(substitute(variable_name))
  
  output_df <- df %>% 
    select(state_face, {{variable_name}}, weights_var) %>% 
    group_by(state_face) %>% 
    summarise(value = Hmisc::wtd.mean({{variable_name}}, weights = weights_var, na.rm = TRUE, normwt = TRUE),
              var.value = Hmisc::wtd.var({{variable_name}}, weights = weights_var, na.rm = TRUE, normwt = TRUE),
              sd.value = sqrt(var.value),
              total_participants = n()) %>% 
    mutate(se.value = sd.value / sqrt(total_participants),
           ci_low = value - qt(1 - (0.05 / 2), total_participants - 1) * se.value,
           ci_upp = value + qt(1 - (0.05 / 2), total_participants - 1) * se.value,
           moe = qt(1 - (0.05 / 2), total_participants - 1) * se.value) %>% 
    select(-c(sd.value, se.value, var.value)) %>% 
    relocate(total_participants, .after = last_col()) %>% 
    mutate_all(~ifelse(is.nan(.), NA, .)) %>% 
    mutate(across(where(is.numeric), )) %>% 
    mutate(question_name = varname) %>% 
    relocate(question_name)
  
  return(output_df)
}
