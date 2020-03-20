# library -----------------------------------------------------------------
library(dplyr)
library(butteR)
library(rgdal)
library(sf)
library(sp)
library(readr)
library(plotKML)
library(stringr)

# read_data ---------------------------------------------------------------

sample_size <- read_csv("Inputs/Sampling/kmz_sample_request_template_round1.csv") #need to change everytime 
sample_size <- sample_size%>%  filter(sample_size$number_points_w_buffer != "")
block_boundary_gdb <- "D:\\mh1\\REACH\\Common_shape_files/T190310_Outline_RRC_Block_A2.shp"
block_boundary <- st_read(block_boundary_gdb) %>% st_transform(crs = 4326)
block_to_aviod <-read_csv("Inputs/Sampling/BlocsToAvoid.csv")

input_path <- "D:\\mh1\\REACH\\cpp\\CPP\\Output\\sampling\\kml/"

round <- "round1" #need to change everytime 

write_kl <- paste0(input_path,round,"/") #need to change everytime 

# only_for_pilot ----------------------------------------------------------

shelter_gdb <- "D:\\mh1\\REACH\\cpp\\CPP\\Inputs\\Sampling/shelter_pt_blk.shp"

read_shelter <- st_read(shelter_gdb)

shelter <- read_shelter

sampler_ouput <- butteR::stratified_sampler(sample.target.frame = sample_size,sample.target.frame.strata = "Camp_Name",
                           sample.target.frame.samp.size = "number_points_w_buffer",pt.data = shelter,
                           pt.data.strata = "cmp_nam",pt.data.labels = "cmp_nam",target_gdb = write_kl,write_remaining_sample_csv = T,
                           write_kml = T)

# women_safety ------------------------------------------------------------

kml_file_list <-list.files(write_kl,full.names = T) %>% as.data.frame()

camp_26_file <- kml_file_list %>% filter(grepl("Camp_26_", kml_file_list$.)) %>% select(".") 
camp_27_file <- kml_file_list %>% filter(grepl("Camp_27_", kml_file_list$.)) %>% select(".") 

camp_26_gdb <- camp_26_file$.%>% as.character() %>%  dput()
camp_27_gdb <-camp_27_file$.%>% as.character() %>%  dput()

camp_26 <- st_read(camp_26_gdb) %>% mutate(camp= "camp_26")
camp_27 <- st_read(camp_27_gdb)%>% mutate(camp= "camp_27")

Combind <- rbind(camp_26,camp_27)
# Combind <- camp_26


file.remove(c(camp_26_gdb,camp_27_gdb))
# file.remove(camp_26_gdb)



combind_join2 <-st_join(Combind,block_boundary, st_intersects) %>% mutate(blk_to_avoid = if_else(BlockNam %in% block_to_aviod$BlocsToAvoid,paste0(camp,"_yes"),paste0(camp,"_no")))



for (i in c("camp_26", "camp_27")) {
  
  combind_join <- combind_join2 %>% filter(combind_join2$camp == i)
  
women_safe<- combind_join %>% filter(str_detect(blk_to_avoid,"no"))

if(nrow(women_safe) !=0) { 
  women_safe$ID <- seq.int(nrow(women_safe))
  a <- plotKML(sf::as_Spatial(women_safe),paste0(write_kl,"/",i,"_WS_",nrow(women_safe),"pts"),
               kmz=FALSE,altitude=0,plot.labpt=TRUE,labels= paste0(women_safe$ID,"_",women_safe$blk_to_avoid),LabelScale=0.5)
  }
  
 women_unsafe <-combind_join %>% filter(str_detect(blk_to_avoid,"yes"))
 
if(nrow(women_unsafe) !=0) {  
  women_unsafe$ID <- seq.int(nrow(women_unsafe))
          
b <- plotKML(sf::as_Spatial(women_unsafe),paste0(write_kl,"/",i,"_WUS_",nrow(women_unsafe),"pts"),
        kmz=FALSE,altitude=0,plot.labpt=TRUE,labels= paste0(women_unsafe$ID,"_",women_unsafe$camp),LabelScale=0.5)

}
 }


# round1 -------------------------------------------------------------------

shltr_csv <- read_csv("Output/sampling/kml/pilot/20200203_samp_remaining.csv") #need to change everytime 

remaining_shltr <- st_as_sf(shltr_csv,coords = c("x","y"),crs = 4326)

sampler_ouput <- butteR::stratified_sampler(sample.target.frame = sample_size,sample.target.frame.strata = "Camp_Name",
                                            sample.target.frame.samp.size = "number_points_w_buffer",pt.data = remaining_shltr,
                                            pt.data.strata = "cmp_nam",pt.data.labels = "cmp_nam",target_gdb = write_kl,write_remaining_sample_csv = T,
                                            write_kml = T)


