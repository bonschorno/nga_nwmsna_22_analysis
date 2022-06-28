rm(list = ls())

source("functions/asset/small_function.r")
source("functions/analysis/surveymodule.r")
source("functions/analysis/graph_function.r")
source("functions/analysis/function_data_merge.R")

if(!any(list.files()=="analysis_check")){dir.create("analysis_check")}


# manual step - creation of the dap
# load analysis plan
# load survey tool
questions <- read.csv("input/questionnaire/kobo_questions.csv")
choices <- read.csv("input/questionnaire/kobo_choices.csv") 

data<-read.csv("input/datasets/data_clean.csv")
#names(data)

dapl<-read.csv("input/dap/dap_to_check.csv")

######################
# analysis
######################
options("survey.lonely.psu"="remove")

mainresults<-analysis_wrap(
 data=data,
 log=dapl,
 samplingframe=NULL,#samplingframe,
 questions=questions,
 choices=choices,
 lang="english",
 session=paste("analysis_opt_MSNA_check")
)

mainresults %>% write.csv("analysis_check/result_to_compare.csv")
