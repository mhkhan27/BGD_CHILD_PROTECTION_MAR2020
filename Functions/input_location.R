
# library -----------------------------------------------------------------



library(dplyr)
library(anytime)
library(lubridate)
library(AMR)
library(forcats)

# define condition --------------------------------------------------------


population<-c("Host","Refugee")[1]
write <- c("yes","no")[2]


# load input --------------------------------------------------------------


if ( population == "Refugee"){
  hh_data<-read.csv("D:/mh1/REACH/MSNA/cleaned_data/Refugee/cleaned_data/20190915_HH_Refugee_Cleaned_20190917.csv", header = TRUE, sep = ",")
  individual_data<-read.csv("D:/mh1/REACH/MSNA/cleaned_data/Refugee/cleaned_data/20190915_Indiv_Refugee_Cleaned_20190917.csv", header = TRUE, sep = ",")   
}

if ( population == "Host"){
  hh_data<-read.csv("D:/mh1/REACH/MSNA/cleaned_data/Host_Community/cleaned_datasets/20190909_HH_HostCommunity_Cleaned_20190915_with_dist.csv", header = TRUE, sep = ",")
  individual_data<-read.csv("D:/mh1/REACH/MSNA/cleaned_data/Host_Community/cleaned_datasets/20190909_Indiv_HostCommunity_Cleaned_20190915.csv", header = TRUE, sep = ",")   
}



# Function ----------------------------------------------------------------


make_composite_indicators_bgd_msna_2019(hh_data = HH,individual_data = INDV,population == "Host")

