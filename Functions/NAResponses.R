
function(data){
  na_count_per_question<-sapply(data, function(y) sum(length(which(is.na(y)))))
  na_percent_per_question <-sapply(data, function(y) ((sum(length(which(is.na(y)))))/nrow(HeaderYes))*100)
}


na_count <-sapply(HeaderYes, function(y) sum(length(which(is.na(y)))))
na_perc <-sapply(HeaderYes, function(y) ((sum(length(which(is.na(y)))))/nrow(HeaderYes))*100)
non_response_df<-data.frame(num_non_response=na_count,perc_non_response= na_perc)
non_response_df1<-non_response_df %>%
  mutate(question=rownames(.)) %>% 
  dplyr::select(question, everything()) %>% 
  arrange(num_non_response, question)

aux_file_path_na_hh<- paste0(aux_path,"\\", Sys.Date(),"_NA_ResponseRate_HHLevel.csv")
write.csv(non_response_df1,aux_file_path_na_hh)