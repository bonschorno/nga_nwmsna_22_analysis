# Create dataset to change answer labels manually

idp_dataset <- read_csv("output/final_dataset_idp.csv")

length(unique(idp_dataset$answer))

unique(idp_dataset$answer)

answer_data <- idp_dataset %>% 
  group_by(battery) %>% 
  distinct(answer) %>% 
  drop_na(answer) %>% 
  filter(!answer %in% c("yes", "no", "numeric", "other", "do_not_know",
                        "prefer_not_to_answer")) %>% 
  mutate(new_label = str_to_sentence(gsub("_", " ", answer))) %>% 
  view

write_csv(answer_data, file = "data/renaming_labels_raw.csv")


# Update labels -----------------------------------------------------------

rm(list = ls())

final_dataset_raw <- read_csv("output/final_dataset_idp.csv")

cleaned_labels <- read_csv("data/renaming_labels_v1.csv")

final_dataset_cleanlabels <- final_dataset_raw %>% 
  left_join(cleaned_labels) %>% 
  mutate(new_label = case_when(answer == "yes" ~ "Yes",
                               answer == "no" ~ "No",
                               answer == "numeric" ~ "Numeric variable",
                               answer == "other" ~ "Other",
                               answer == "do_not_know" ~ "Don't know",
                               answer == "prefer_not_to_answer" ~ "Prefer not to answer",
                               TRUE ~ new_label)) %>% 
  mutate(xml_value = ifelse(str_detect(question_name, "/"), 
                            gsub("/.+", "", question_name), question_name)) %>% 
  view

dap_formatted <- readxl::read_xlsx("data/dap_formatted.xlsx") %>% 
  janitor::clean_names() %>% 
  select(xml_value, question)


final_dataset_questionsadded <- final_dataset_cleanlabels %>% 
  left_join(dap_formatted)

