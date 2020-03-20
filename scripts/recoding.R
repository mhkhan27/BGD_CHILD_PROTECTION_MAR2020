rm(list=ls())
# Library -----------------------------------------------------------------

library(dplyr)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(stringr)

population <-c("adolescent","caregiver")[2]
write <-c("yes","no")[1]

# data --------------------------------------------------------------------
if (population == "adolescent"){
  hh <- read.csv("Inputs/adolescent/03_clean_data/hh.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
  hh<-hh[rowSums(!is.na(hh[,-1]))>=1,]
  
  indv <- read.csv("Inputs/adolescent/03_clean_data/indv.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
}

if (population == "caregiver"){
  hh <- read.csv("Inputs/caregiver/03_clean_data/hh.csv",stringsAsFactors = FALSE, 
                 na.strings = c(""," ", "n/a",NA))
  hh<-hh[rowSums(!is.na(hh[,-1]))>=1,]
  
  indv <- read.csv("Inputs/caregiver/03_clean_data/indv.csv",stringsAsFactors = FALSE, 
                   na.strings = c(""," ", "n/a",NA))
}

# colnames  ---------------------------------------------------------------
some_primary <- c("elementary_school_standard_1", "elementary_school_standard_2",
                  "elementary_school_standard_3","elementary_school_standard_4","madrasha")

primary_higher <- c("middle_school_standard_5", "middle_school_standard_6", "middle_school_standard_7", 
                    "middle_school_standard_8", "high_school_standard_9","high_school_standard_10",
                    "tertiary_education")

threats_safety_colnames<- c("threats_1","threats_2","threats_3","threats_6","threats_8","threats_9",
                            "threats_14")

adol_hh_chores_daily_life <- c("resp_activities_1","resp_activities_2","resp_activities_3","resp_activities_4",
                               "resp_activities_6")

adol_attend_centre_learning_daily_life <- c("resp_activities_7","resp_activities_8","resp_activities_9","resp_activities_10")
seek_treat_r <- c("seek_treat.public.ngo_clinic", "seek_treat.private_clinic", "seek_treat.traditional_healer")

chores <- c("work_chores","work_shelter","work_phys_chores","work_sibs")
work_unacceptable <- c("work_outside","work_dang","work_constr","work_stop_school")

early_marriage <- c("marr_par", "marr_dowry", "marr_burden", "marr_safe", "marr_drop_school_girl", 
                   "marr_drop_school_boy", "marr_old_man", "marr_mom")

violence <- c("vio_fath", "vio_moth", "vio_discipline", "vio_husb", "vio_wife", 
              "vio_teach", "vio_solve_prob")

most_time_home <- c("resp_act_three.cook", "resp_act_three.clean", "resp_act_three.take_care_of_family_members",
  "resp_act_three.spend_time_with_family") ##one ignore  

service_help_partner <- c("ask_help.wife","ask_help.husband",
                          "ask_stress.wife","ask_stress.husband",
                          "ask_unsafe.wife","ask_unsafe.husband",
                          "ask_physical_assistance.wife","ask_physical_assistance.husband")


service_help_spouse <- c("ask_help.mother","ask_help.father",
                          "ask_stress.mother","ask_stress.father",
                          "ask_unsafe.mother","ask_unsafe.father",
                          "ask_physical_assistance.mother","ask_physical_assistance.father")

# hh_to_hh ----------------------------------------------------------------

final_data_hh <- hh %>% mutate(
  i.hh_size_small = if_else(hh_size < 5, "yes","no",NULL),
  i.hh_size_large = if_else(hh_size > 4, "yes","no",NULL),
  i.threats_1_some_always =if_else(threats_1 == "sometimes_concerned" | threats_1 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_2_some_always =if_else(threats_2 == "sometimes_concerned" | threats_2 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_3_some_always =if_else(threats_3 == "sometimes_concerned" | threats_3 == 
                                     "always_concerned","yes","no",NULL), 
  
  i.threats_4_some_always =if_else(threats_4 == "sometimes_concerned" | threats_4 == 
                                     "always_concerned","yes","no",NULL),
  i.threats_5_some_always =if_else(threats_5 == "sometimes_concerned" | threats_5 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_6_some_always =if_else(threats_6 == "sometimes_concerned" | threats_6 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_7_some_always =if_else(threats_7 == "sometimes_concerned" | threats_7 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_8_some_always =if_else(threats_8 == "sometimes_concerned" | threats_8 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_9_some_always =if_else(threats_9 == "sometimes_concerned" | threats_9 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_10_some_always =if_else(threats_10 == "sometimes_concerned" | threats_10 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_11_some_always =if_else(threats_11 == "sometimes_concerned" | threats_11 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_12_some_always =if_else(threats_12 == "sometimes_concerned" | threats_12 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_13_some_always =if_else(threats_13 == "sometimes_concerned" | threats_13 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_14_some_always =if_else(threats_14 == "sometimes_concerned" | threats_14 == 
                                     "always_concerned","yes","no",NULL), 
  i.threats_harmful_practice = if_else(threats_4 == "yes" | threats_5 == "yes","yes","no",NULL),
  i.threats_accidental = if_else(threats_10 == "yes" | threats_11 == "yes"| threats_12 == "yes" ,"yes","no",NULL),
  
  threat_safety_sum = rowSums(hh[threats_safety_colnames] == "yes"),
  i.threats_safety = if_else( threat_safety_sum > 0 ,"yes","no"),
  chores_acceptable_rowsum = rowSums(hh[chores] == "agree"),
  chores_unacceptable_rowsum = rowSums(hh[chores] == "disagree"),
  i.chores_acceptable = if_else(chores_acceptable_rowsum == 4, "yes","no",NULL),
  i.chores_unacceptable =  if_else(chores_unacceptable_rowsum == 4, "yes","no",NULL),
  
  work_unacceptable_rowsum =rowSums(hh[work_unacceptable] == "disagree"),
  i.work_unacceptable= if_else(work_unacceptable_rowsum == 4, "yes","no",NULL),
  i.hazardous_work_unacceptable = if_else(work_dang == "disagree" & 
                                            work_constr == "disagree","yes","no",NULL),
  
  i.hazardous_work_acceptable = if_else(work_dang == "agree" & 
                                          work_constr == "agree","yes","no",NULL),
  early_marriage_acceptable_rowsum= rowSums(hh[early_marriage]== "agree"),
  early_marriage_unacceptable_rowsum = rowSums(hh[early_marriage]== "disagree"),
  i.early_marriage_acceptable = if_else(early_marriage_acceptable_rowsum == 8 ,"yes","no",NULL),
  i.early_marriage_uncceptable = if_else(early_marriage_unacceptable_rowsum == 8 ,"yes","no",NULL),
  
  harmful_violence_acceptable_rowsum = rowSums(hh[violence] == "agree"),
  harmful_violence_unacceptable_rowsum = rowSums(hh[violence] == "disagree"),
  i.harmful_violence_acceptable = if_else(harmful_violence_acceptable_rowsum == 7,"yes","no",NULL),
  i.harmful_violence_unacceptable = if_else(harmful_violence_unacceptable_rowsum == 7,"yes","no",NULL),
  
  i.harmful_violence_acceptable_parents = if_else(vio_fath == "agree" & 
                                                    vio_moth == "agree","yes","no",NULL),
  i.harmful_violence_acceptable_males = if_else(vio_fath == "agree" & vio_husb == "agree" &
                                                  vio_moth == "disagree" & vio_wife == "disagree","yes","no",NULL)
)

if(population == "adolescent") {
  final_data_hh_v2 <- final_data_hh %>% mutate(
    i.hh_no_formal_edu = if_else(edu_hoh == "none","yes","no",NULL),
    i.hh_some_primary = if_else(edu_hoh %in% some_primary,"yes","no",NULL),
    i.hh_primary_higher = if_else(edu_hoh %in% primary_higher,"yes","no",NULL),
    hh_chores_daily_life_rowsum = rowSums(hh[adol_hh_chores_daily_life] == "yes",na.rm = T),
    i.adol_hh_chores_daily_life = if_else(hh_chores_daily_life_rowsum >0 ,"yes","no",NULL),
    adol_attend_centre_learning_daily_life_rowsum = rowSums(hh[adol_attend_centre_learning_daily_life] == "yes",na.rm = T),
    i.adol_attend_centre_learning_daily_life = if_else(adol_attend_centre_learning_daily_life_rowsum >0 ,"yes","no",NULL),
    i.daily_act_chores_school = if_else(i.adol_hh_chores_daily_life=="yes" &
                                          i.adol_attend_centre_learning_daily_life == "yes","yes","no",NULL),
    i.daily_act_chores_work = if_else(i.adol_hh_chores_daily_life == "yes" & 
                                        resp_activities_14 == "yes","yes","no",NULL),
    i.daily_act_school_work = if_else(i.adol_attend_centre_learning_daily_life == "yes" &
                                        resp_activities_14 == "yes","yes","no",NULL),
    seek_treat_rowsum = rowSums(hh[seek_treat_r]),
    i.married.hc = if_else(resp_marr == "yes" & seek_treat_rowsum >0 ,"yes","no",NULL),
    i.married.edu = if_else(resp_marr == "yes" & access_lc == "yes","yes","no",NULL),
    i.married.cp = if_else(resp_marr == "yes" & access_cp == "yes","yes","no",NULL),
    i.working_hc = if_else(resp_activities_14 == "yes" & seek_treat_rowsum >0 ,"yes","no",NULL),
    i.working_edu =if_else(resp_activities_14 == "yes" & access_lc == "yes","yes","no",NULL),
    i.working_cp = if_else(resp_activities_14 == "yes" & access_cp == "yes","yes","no",NULL),
    i.adol_not_seek_health_treatment_safety = if_else(not_seek_treat.not_feel_comfortable_at_the_health_center== "yes" |
                                                        not_seek_treat.not_feel_comfortable == "yes","yes","no",NULL),
    i.adol_not_seek_services_safety = if_else( i.adol_not_seek_health_treatment_safety == "yes"| 
                                               child_protection_services.comfortable_walking_to_the_CFS.AFS == "yes"|
                                                 attend_a_learning_center.comfortable_walking_to_the_learning_center == "yes",
                                               "yes","no",NULL),
                                               
    i.adol_felt_unsafe_any_service = if_else(if_challenges_hc.i_didn.t_feel_safe == "yes" |
                                              challenges_lc.i_didnt_feel_safe == "yes"|
                                              challenges_cp.didnt_feel_safe == "yes","yes","no",NULL),
 
    most_time_home_rowsum = rowSums(hh[most_time_home]),
    i.most_time_home = if_else(most_time_home_rowsum > 0,"yes","no",NULL),
    
    i.take_care_fam_disabled = if_else(resp_act_three.take_care_of_family_members == 1 &
                                         hh_disability == "yes","yes","no",NULL),
    service_help_partner_rowsum = rowSums(hh[service_help_partner]),
    i.service_help_partner= if_else(service_help_partner_rowsum > 0 ,"yes","no",NULL),
    
    service_help_spouse_rowsum = rowSums(hh[service_help_spouse]),
    i.service_help_spouse= if_else(service_help_spouse_rowsum > 0 ,"yes","no",NULL)
  )
}

if(population == "caregiver") {
  final_data_hh_v2 <- final_data_hh %>% mutate(
    i.hh_no_formal_edu = if_else(resp_high_level_edu == "none","yes","no",NULL),
    i.hh_some_primary = if_else(resp_high_level_edu %in% some_primary,"yes","no",NULL),
    i.hh_primary_higher = if_else(resp_high_level_edu %in% primary_higher,"yes","no",NULL),
  )
}

# HH_TO_INDV --------------------------------------------------------------

join_data <- hh %>% left_join(indv,by = c("X_uuid"="X_submission__uuid"))
# join_data2 <- indv %>% left_join(final_data_hh_v2,by = c("X_submission__uuid"="X_uuid"))

hh_to_indv <- join_data %>% group_by(X_uuid) %>% summarise(
  i.hh_baby = if_else(any(ind_age %in% 0:2),"yes","no",NULL),
  i.hh_child = if_else(any(ind_age %in% 3:5),"yes","no",NULL),
  i.hh_adol = if_else(any(ind_age %in% 6:12),"yes","no",NULL),
  i.hh_older_adol = if_else(any(ind_age %in% 13:17),"yes","no",NULL),
  i.hh_elderly = if_else(any(ind_age > 59),"yes","no",NULL)) 



# combind -----------------------------------------------------------------

final <- final_data_hh_v2 %>% left_join(hh_to_indv,by = "X_uuid")

if (population == "adolescent"){
  
final <- final %>% mutate(
  take_care_fam_young_condition = if_else(i.hh_child == "yes" | 
                                              i.hh_baby == "yes","yes","no",NULL),
  i.take_care_fam_young = if_else(take_care_fam_young_condition == "yes" & 
                                    resp_act_three.take_care_of_family_members == 1 ,"yes","no",NULL),
  i.take_care_fam_eld = if_else(resp_act_three.take_care_of_family_members == 1 &
                                  i.hh_elderly == "yes","yes","no",NULL)
)
}

# write_csv ---------------------------------------------------------------


if (write == "yes"){
    write.csv(final,paste0("Output/recoding/",population,"_recoding_final_combind.csv"))
  }
  
