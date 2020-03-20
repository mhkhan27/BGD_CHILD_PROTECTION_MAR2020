
# common ------------------------------------------------------------------

day_to_run<- Sys.Date()

write_csv_output <- c("yes","no")[1]

# adolescent --------------------------------------------------------------

rmarkdown::render("CPA_Adolescent_Coding_Daily_Monitoring_Report.Rmd")

file_location <- "D:\\mh1\\REACH\\cpp\\CPP/CPA_Adolescent_Coding_Daily_Monitoring_Report.html"
copy_to_drop <- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DYL - UNICEF (Child Protection)\\04 Data\\01_adolescent\\02_daily_report/"
file.copy(file_location,paste0(copy_to_drop,str_replace_all(day_to_run,"-","_"),"_CPA_Adolescent_Coding_Daily_Monitoring_Report.html"),overwrite = T)


# caregiver ---------------------------------------------------------------

rmarkdown::render("CPA_caregiver_Coding_Daily_Monitoring_Report.Rmd")

file_location <- "D:\\mh1\\REACH\\cpp\\CPP/CPA_caregiver_Coding_Daily_Monitoring_Report.html"
copy_to_drop <- "C:\\Users\\MEHEDI\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DYL - UNICEF (Child Protection)\\04 Data\\02_caregiver\\02_daily_report/"
file.copy(file_location,paste0(copy_to_drop,str_replace_all(day_to_run,"-","_"),"_CPA_caregiver_Coding_Daily_Monitoring_Report.html"),overwrite = T)
