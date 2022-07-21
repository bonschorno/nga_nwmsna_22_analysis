idp_freq_dummyvars <- function(df, name_dummy_variables){
  
  # 1. step: define answer options. In case of dummy variables, it's always yes and no
  answer_options <- c("yes", "no")
  n_answer_options <- length(answer_options)
  
  # 2. step: create a lookup table where every question is listed with every answer option
  lookup_table <- df %>%
    select(state_face, contains(name_dummy_variables)) %>%
    pivot_longer(-state_face, names_to = "question_name", values_to = "values") %>%
    distinct(state_face, question_name) %>%
    group_by(state_face) %>%
    slice(rep(1:n(), each = n_answer_options))
  
  # dimensions of the look-up table
  states_in_lookup <- length(unique(lookup_table$state_face))
  questions_in_lookup <- length(unique(lookup_table$question_name))
  
  # create placeholder
  answer_options_vec <- rep(answer_options, states_in_lookup*questions_in_lookup)
  
  # add placeholder to look-up table
  lookup_table$answer <- answer_options_vec
  
  # 3. step: create a table with the actual data
  data_table_srvyr <- df %>%
    select(state_face, ward_face, contains(name_dummy_variables), weights_var) %>% 
    pivot_longer(-c(state_face, weights_var, ward_face), names_to = "question_name", values_to = "value") %>% 
    mutate(answer = ifelse(value == 1, "yes", "no")) %>% 
    as_survey_design(ids = ward_face, weights = weights_var) %>% 
    group_by(state_face, question_name, answer) %>% 
    summarise(value = survey_prop(vartype = c("ci"), level = 0.92, proportion = TRUE),
              total_subgroup = n()) %>% 
    mutate(moe = (value_upp - value_low)/2)
  
  # 4. step: data cleaning
  final_table <- left_join(lookup_table, data_table_srvyr) %>% 
    replace(is.na(.), 0) %>% 
    group_by(state_face, question_name) %>% 
    mutate(total_participants = sum(total_subgroup)) %>% 
    mutate(across(.cols = where(is.numeric), .fns =  ~ round(.x, digits = 2))) %>% 
    relocate(moe, .after = value_upp) %>% 
    relocate(question_name) %>% 
    rename(ci_upp = value_upp,
           ci_low = value_low)
  
  return(final_table)
  
}