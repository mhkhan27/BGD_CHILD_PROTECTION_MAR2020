rm(list=ls())
# library -----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(plotKML)
library(stringr)
library(srvyr)
library(survey)
library(readxl)

koboquest <- list.files("scrap/koboquest/R",full.names = T)

for (i in koboquest ){
  source(i)
}

create_csv <-c ("yes","no") [1]
population <-c("adolescent","caregiver")[2]

# data --------------------------------------------------------------------
if (population =="adolescent"){
  camp_data <- read.csv("Inputs/adolescent/04_raw_Data/2020_03_04_HH.csv",
                        stringsAsFactors = F, 
                        na.strings = c(""," ", "n/a",NA)) %>% dplyr::select(c("X_uuid","New_Camp_N"))
  
  data_location <-"Output/recoding/adolescent_recoding_final_combind.csv"
  data_for_analysis1 <- read.csv(data_location,stringsAsFactors = F, 
                                na.strings = c(""," ", "n/a",NA)) 
  #data_set_yes_consent
  data_for_analysis <- data_for_analysis1 %>% inner_join(camp_data,by = "X_uuid") %>% dplyr::filter(consent == "yes")

  assess_survey<- readxl::read_xls("Inputs/adolescent/kobo_adol.xls",sheet = "survey")
  assess_choices<-readxl::read_xls("Inputs/adolescent/kobo_adol.xls",sheet = "choices")
  }

if (population =="caregiver"){
  camp_data <- read.csv("Inputs/caregiver/04_raw_Data/2020_02_29_HH.csv",
                        stringsAsFactors = F, 
                        na.strings = c(""," ", "n/a",NA)) %>% dplyr::select(c("X_uuid","New_Camp_N"))
  data_location <-"Output/recoding/caregiver_recoding_final_combind.csv"
  data_for_analysis1 <- read.csv(data_location,stringsAsFactors = FALSE, 
                                na.strings = c(""," ", "n/a",NA))
  #data_yes_consent
  data_for_analysis <- data_for_analysis1 %>% inner_join(camp_data,by = "X_uuid") %>% dplyr::filter(consent == "yes")
  
  assess_survey<- readxl::read_xls("Inputs/caregiver/kobo_caregiver.xls",sheet = "survey")
  assess_choices<-readxl::read_xls("Inputs/caregiver/kobo_caregiver.xls",sheet = "choices")
}

assessment<-load_questionnaire(data = data_for_analysis,questions = assess_survey,
choices = assess_choices,choices.label.column.to.use = "label::english")

# desensitized_data ---------------------------------------------------------

data_df <- data_for_analysis %>% as.data.frame() %>% select(-contains("geo")) %>% select(-contains("gps"))

# butter ------------------------------------------------------------------

pop<- read.csv("Inputs/pop_UNHCR_march_2020.csv", stringsAsFactors = F, na.strings=c(""," "))
analysis_strata<-"regional_strata"
sf_strata<-"Camp"
sf_pop<- "Total.Families"
df_strata<- "Upazila"

teknaf_camps <- c("Camp 21", "Camp 22", "Camp 23", "Camp 24", "Camp 25", "Camp 26", 
                  "Camp 27","Nayapara RC")
df<-data_df %>% dplyr::mutate(
  Upazila = if_else(New_Camp_N %in% teknaf_camps, "Teknaf","Ukhiya",NULL))

colnames1 <- df$New_Camp_N %>% unique %>% dput()


pop2<-pop %>% 
  filter(!is.na(Camp),is.na(Block)) %>% 
  filter(Camp!="Kutupalong RC") %>% 
  mutate(
    !!(sf_strata):=stringr::str_replace(Camp, "Total","") %>% trimws(),
    !!(sf_strata):= stringr::str_replace_all(Camp,"Extension","Extension"),
    Total.Families=readr::parse_number(Total.Families %>% stringr::str_replace_all(",","")),
    Total.Individuals= readr::parse_number(Total.Individuals %>% stringr::str_replace_all(",",""))
  ) %>% filter(Camp %in% colnames1) #removing camp 12 and 18

pop2 <- pop2 %>% dplyr::mutate(
  Upazila = if_else(Camp %in% teknaf_camps, "Teknaf","Ukhiya",NULL)) %>% 
  dplyr::group_by(Upazila) %>% dplyr::summarise(
  Total.Families = sum(Total.Families),
Total.Individuals =sum(Total.Individuals))


sf_with_weights<-df %>% 
  group_by(!!sym(df_strata)) %>% 
  summarise(sample_strata_num=n()) %>% 
  right_join(pop2, by="Upazila") %>% mutate(
    sample_global = sum(sample_strata_num),
         pop_global=sum(!!sym(sf_pop)),
         survey_weight= (!!sym(sf_pop)/pop_global)/(sample_strata_num/sample_global)
  )


df2<-df %>% left_join(sf_with_weights) %>% select(-contains("_Other"), -ends_with(".other"))

df2 <- df2 %>% dplyr::filter(!is.na(survey_weight))

dfsvy<-svydesign(ids = ~1,strata = formula(paste0("~",df_strata)),data = df2,weights = formula(paste0("~", "survey_weight")))

dfsvy$variables<- butteR::questionnaire_factorize_categorical(data = dfsvy$variables,questionnaire = assessment,return_full_data = T)

# colnames(df2) %>% dput() %>% dput

dont_analyze<-c("X_uuid",
                "New_Camp_N", "survey_date", "survey_start", "deviceid", "end_survey", 
                "instance_name", "enumerator_id", "enu_gen", "consent", "resp_age", "resp_gender", "resp_hoh", 
                "gender_hoh", "age_of_household","how_resp_work_pay",
                "sample_strata_num", "upazilla", "sample_global", "pop_global", "survey_weight",
                "Total.Families", "Total.Individuals","X_id", "X_submission_time", "X_index", "reported_date", 
                "District", "Settlement", "Union", "Name_Alias", "SSID", "Area_SqM", 
                "Area_Acre", "xlab", "ylab", "Label")

dont_analyze_in_data<-dont_analyze[dont_analyze %in% colnames(df2)]
is_not_empty<-function(x){ all(is.na(x))==FALSE}


cols_to_analyze<-df2 %>% select(-starts_with("Other"), -ends_with(".other")) %>%
  select_if(.,is_not_empty) %>% select(-dont_analyze_in_data) %>% colnames() 


# factors those have only one level ---------------------------------------
# data_for_fatorization <- read.csv(data_location,stringsAsFactors = T,
#                               na.strings = c(""," ", "n/a",NA))
# 
# l <- sapply(data_for_fatorization, function(x) is.factor(x))
# m <- data_for_fatorization[, l]
# drp <- ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP") %>% as.data.frame()
# drop_non_drp_col <-rownames_to_column(drp,var = "a")
# drop_col <- drop_non_drp_col %>% dplyr::filter(. == "DROP")
# 
# 
# factor_one_level <- drop_col %>% dplyr::mutate(
#   other = if_else(str_detect(drop_col$a,"other")== T,"yes","no",NULL)
# ) %>% dplyr::filter(other == "no")
# 
# 
# data_need_add_factor_level <-  factor_one_level$a
# data_need_add_factor_level<- data_need_add_factor_level %>% dput
# 
# aaaa <- df2[data_need_add_factor_level]
# 
# sapply(aaaa,unique)
#############################################################################################3

# dfsvy$variables$i.threats_harmful_practice<- forcats::fct_expand(dfsvy$variables$i.threats_harmful_practice,c( "no", "yes"))
# dfsvy$variables$i.threats_accidental<- forcats::fct_expand(dfsvy$variables$i.threats_accidental,c("no", "yes"))
dfsvy$variables$i.threats_safety<- forcats::fct_expand(dfsvy$variables$i.threats_safety,c( "no", "yes"))

if (population == "adolescent"){
  dfsvy$variables$i.work_unacceptable<- forcats::fct_expand(dfsvy$variables$i.work_unacceptable,c( "no", "yes"))
  dfsvy$variables$i.hazardous_work_acceptable<- forcats::fct_expand(dfsvy$variables$i.hazardous_work_acceptable,c( "no", "yes"))
  dfsvy$variables$i.early_marriage_acceptable<- forcats::fct_expand(dfsvy$variables$i.early_marriage_acceptable,c( "no", "yes"))
  dfsvy$variables$i.adol_not_seek_services_safety<- forcats::fct_expand(dfsvy$variables$i.adol_not_seek_services_safety,c("no","yes"))
  dfsvy$variables$i.adol_felt_unsafe_any_service<- forcats::fct_expand(dfsvy$variables$i.adol_felt_unsafe_any_service,c("no","yes"))
}

if (population == "caregiver"){
  dfsvy$variables$threats_14<-forcats::fct_expand(dfsvy$variables$threats_14,c( "not_concerned", "99999"))
  dfsvy$variables$i.threats_14_some_always<-forcats::fct_expand(dfsvy$variables$i.threats_14_some_always,c( "no", "yes"))
  }

# basic Analysis ----------------------------------------------------------

basic_analysis_overall<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze)
basic_analysis_strata<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "Upazila")
basic_analysis_gender<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "resp_gender")
if(population == "adolescent"){
basic_analysis_married<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "resp_marr")
basic_analysis_age_15_17<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "i.hh_resp_age_15_17")
basic_analysis_age_15_17_gender<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = c("i.hh_resp_age_15_17","resp_gender"))
basic_analysis_child<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "resp_child")
basic_analysis_activities14<-butteR::mean_proportion_table(design = dfsvy,list_of_variables = cols_to_analyze,aggregation_level = "resp_activities_14")
}
#write_csv ---------------------------------------------------------------

if (create_csv =="yes"){
  output_location <- "Output/butter_basic_analysis/"
  write.csv(basic_analysis_overall,paste0(output_location,population,"_basic_analysis_overall.csv"))
  write.csv(basic_analysis_strata,paste0(output_location,population,"_basic_analysis_strata.csv"))
  write.csv(basic_analysis_gender,paste0(output_location,population,"_basic_analysis_gender.csv"))
  
if(population == "adolescent"){
  write.csv(basic_analysis_married,paste0(output_location,population,"_basic_analysis_married.csv"))
  write.csv(basic_analysis_child,paste0(output_location,population,"_basic_analysis_child.csv"))
  write.csv(basic_analysis_activities14,paste0(output_location,population,"_basic_analysis_activities14.csv"))
  write.csv(basic_analysis_age_15_17,paste0(output_location,population,"_basic_analysis_age_15_17.csv"))
  write.csv(basic_analysis_age_15_17_gender,paste0(output_location,population,"_basic_analysis_age_15_17_gender.csv"))
}
  }
