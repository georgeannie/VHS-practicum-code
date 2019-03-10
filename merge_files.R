#################################################################################################################
#                 MERGE HEMS AND GEMS TRAUMA DATASET                           #
#################################################################################################################
# Load required packages.
library(janitor)
library(tidyr)
library(dplyr)
#Function to merge files for GEMS
merge_files =function (file_list){
  for (file in file_list){
    
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset_name")){
      dataset_name= read.csv(file, header=TRUE, na.strings=c(""," ", "NA"), stringsAsFactors = FALSE)
      
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset_name")){
      temp_dataset = read.csv(file, header=TRUE, na.strings=c(""," ", "NA"), stringsAsFactors = FALSE)
      dataset_name = rbind(dataset_name, temp_dataset)
      rm(temp_dataset)
    }
  }
  return(dataset_name)
}

#----------------------TRAUMA PART 1 ----------------------#
rm(trauma_part1_gems)
dir="/home/rstudio/trauma/part1"
setwd(dir)

file_list = list.files(dir)
trauma_part1_gems=merge_files(file_list) %>%
  clean_names()
names(trauma_part1_gems) <- sub("_e_.*", "", names(trauma_part1_gems))

#----------------------TRAUMA PART 2 ----------------------#
rm(trauma_part2_gems)
dir="/home/rstudio/trauma/part2"
setwd(dir)

file_list = list.files(dir)
trauma_part2_gems=merge_files(file_list) %>%
  clean_names()
names(trauma_part2_gems) <- sub("_e_.*", "", names(trauma_part2_gems))

trauma_part2_gems = trauma_part2_gems%>%
  filter(inter_facility_transfer_tr25_54 == "No")


#----------------------TRAUMA PART 3 ----------------------#
rm(trauma_part3_gems)
dir="/home/rstudio/trauma/part3"
setwd(dir)

file_list = list.files(dir)
trauma_part3_gems=merge_files(file_list) %>%
  clean_names()
names(trauma_part3_gems) <- sub("_e_.*", "", names(trauma_part3_gems))

#--------------------JOIN USING INCIDENT ID  --------------------#
trauma_gems1 = merge(trauma_part2_gems, trauma_part1_gems, by="incident_id")
trauma_gems2 = merge(trauma_gems1, trauma_part3_gems, by="incident_id")

#----------------REORDER COLUMN NAMES ALPHABETICALLY -----#
trauma_gems_all2=trauma_gems2[,order(names(trauma_gems2))]

#----------------CLEAN DATA TO MATCH HEMS --------------------#
trauma_gems_clean=trauma_gems_all2[,-c(16, 23, 24, 26, 37, 40, 41)]

#-----------------get injury zip for gems data ----------------------#
zip=read.csv("/home/rstudio/trauma/input/trauma_zip.csv",   header = TRUE,  
             na.strings=c(""," ", "NA"), 
             stringsAsFactors = FALSE)%>%
  clean_names() 
names(zip) <- sub("_e_.*", "", names(zip))

trauma_gems_clean_zip=merge(trauma_gems_clean, zip, by='incident_id')
trauma_gems_clean_zip=trauma_gems_clean_zip[,order(names(trauma_gems_clean_zip))]


#----------------------------TRAUMA HEMS ------------------------#
trauma_hems = read.csv("/home/rstudio/trauma/hems/Truama_Registry_all_data2.csv" , 
                       header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE)%>%
  clean_names() 
names(trauma_hems) <- sub("_e_.*", "", names(trauma_hems))

#--------------REORDER COLUMN NAMES ALPHABETICALLY -----#
#-------------(SHOULD BE UPDATED WHEN THE NEW DATA IS RECEIVED FOR HEMS) -----#
trauma_hems$prehospital_gcs_eye = NA
trauma_hems$prehospital_gcs_motor=NA
trauma_hems$prehospital_gcs_verbal=NA
trauma_hems=trauma_hems[,order(names(trauma_hems))]

#-------------------CLEAN/REORDER DATA TO MATCH GEMS AND ARRANGE ALPHABETICALY -------#
trauma_hems_clean=trauma_hems[,!names(trauma_hems) %in% c("hospital_discharge_date_time",
                                                          "hospital_discharge_orders_written_time_tr25_94",
                                                          "icd_9_procedure_date_tr22_5",
                                                          "icd_9_procedure_time_tr22_31",
                                                          "patient_state_tr1_23")]
trauma_hems_clean=trauma_hems_clean[,order(names(trauma_hems_clean))]

#-----------------COMBINE HEMS AND GEMS ----------------------#
trauma = rbind(trauma_hems_clean, 
               setNames(trauma_gems_clean_zip, names(trauma_hems_clean)))

#----------ICD10, FACILITY NAME AND OTHER DATA -----------#
icd10 = read.csv("/home/rstudio/trauma/input/icd10.csv" , 
                 header = TRUE,  na.strings=c(""," ", "NA"), 
                 stringsAsFactors = FALSE)%>%
  clean_names() 

#------------------MERGER WITH ICD10, FACILITY NAME DATA ---------#
trauma_icd10=left_join(trauma, icd10, by='incident_id') %>%
  distinct()

setwd("/home/rstudio/trauma/output")
write.csv(trauma_icd10, 'trauma.csv')
