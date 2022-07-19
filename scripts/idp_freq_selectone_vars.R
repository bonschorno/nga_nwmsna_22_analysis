idp_freq_selectone_vars <- function(complete_df, variable_name, subset_df){
  
  # will set the question name at the very end
  varname <- rlang::as_name(substitute(variable_name))
  
  # 1. step: retreive all possible answer options
  answer_options <- complete_df %>% 
    distinct({{variable_name}}) %>% 
    drop_na() %>% 
    pull()
  
  n_answer_options <- length(answer_options)
  
  # 2. step: create a lookup table where every variable is listed with every answer option
  lookup_table <- complete_df %>%
    distinct(state_face) %>%
    group_by(state_face) %>%
    slice(rep(1:n(), each = n_answer_options))
  
  # dimensions of the look-up table
  states_in_lookup <- length(unique(lookup_table$state_face))
  
  # create placeholder
  answer_options_vec <- rep(answer_options, states_in_lookup)
  
  # add placeholder to look-up table
  lookup_table$answer <- answer_options_vec
  
  # data_table is the actual data
  data_table <- subset_df %>%
    select(state_face, ward_face, answer = {{variable_name}}, weights_var) %>%
    drop_na(answer) %>%
    #group_by(lga_face) %>%
    as_survey_design(ids = ward_face, weights = weights_var) %>% 
    group_by(state_face, answer) %>% 
    summarise(prop = survey_prop(vartype = c("ci"), level = 0.92, proportion = TRUE),
              total_subgroup = n()) %>% 
    mutate(moe = (prop_upp - prop_low)/2) 
  
  # combine lookup table with the computed data
  final_table <- left_join(lookup_table, data_table) %>%
    group_by(state_face) %>% 
    mutate(total_participants = sum(total_subgroup, na.rm = TRUE)) %>% 
    mutate(across(.cols = where(is.numeric), .fns =  ~ round(.x, digits = 2))) %>% 
    ungroup() %>% 
    mutate(across(c(prop:moe), ~ if_else(total_participants > 0 & is.na(moe), 0, .x))) %>% 
    relocate(moe, .after = prop_upp) %>% 
    mutate(question_name = varname) %>% 
    relocate(question_name) 
  
  return(final_table)
  
}

survey::svyciprop
