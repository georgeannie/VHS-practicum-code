#-----------------------------------------------------------------------------------------#
#            PREHOSPITAL DATA TO GET INCIDENT TIMES/EMS/FACILITY/INJURY ZIP CODE          #
#    TRAUMA DATA IS CURRENTLY MISSING INCIDENT TIMES TO AND FROM THE SCENE                #     
#    THIS TABLE IS LOADED TO GET TIME INTERVALS OF TRANSPORT TO AND FROM THE SCENE.       #
#    ZIPCODES/EMS AGENCY/FACILITY FROM THE TRAUMA DATA WILL BE MATCHED AGAINST THE        #
#    THE PREHOSPITAL DATA TO GET THE TIME DIFFERENCE                                      #
#-----------------------------------------------------------------------------------------#

# Load required packages.
library(janitor)
library(tidyr)
library(dplyr)
library(plyr)
library('RPostgreSQL')
source("/home/rstudio/R/aws_rds_access.R")
source("/home/rstudio/R/merge_files_function.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#--------------PREHOSPITAL REPONSE GEMS/HEMS MERGING AND CLEANING---------------------------------#
response_gems=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/Gems/Prehospital facility-Response/response.csv",
                       header = TRUE,  
                       na.strings=c(""," ", "NA"), 
                       stringsAsFactors = FALSE)%>%
  clean_names() 
names(response_gems) <- sub("_e_.*", "", names(response_gems))
response_gems=response_gems[,order(names(response_gems))]
response_gems=response_gems[,-21]

response_hems=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/PreHospital facility-Response.csv",
                       header = TRUE,  
                       na.strings=c(""," ", "NA"), 
                       stringsAsFactors = FALSE)%>%
  clean_names() 
names(response_hems) <- sub("_e_.*", "", names(response_hems))
response_hems=response_hems[,order(names(response_hems))]

response=rbind(response_gems, response_hems)

#---------------FILTER FOR ONLY 911 AND GET EMS AGENCY FOR EACH ---------------------#
ems_service=response[,c("id", "response_ems_agency_name", "response_type_of_service_requested")] %>%
     filter(response_type_of_service_requested == '911 Response (Scene)') %>%
  distinct(.keep_all = TRUE)


#----------------------PREHOSPITAL DISPATCH GEMS AND HEMS MERGE ----------------------#
rm(pre_dispatch_gems)
dir="C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/Gems/PreHospital dispatch-time-scene"
setwd(dir)

file_list = list.files(dir)
pre_dispatch_gems=merge_files(file_list) %>%
  clean_names()
names(pre_dispatch_gems) <- sub("_e_.*", "", names(pre_dispatch_gems))
pre_dispatch_gems=rename(pre_dispatch_gems, c("incident_id_it_record_001" = "id"))
pre_dispatch_gems=pre_dispatch_gems[,order(names(pre_dispatch_gems))]


pre_dispatch_hems=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/PreHospital dispatch-time-scene.csv",
                           header = TRUE,  
                            na.strings=c(""," ", "NA"), 
                            stringsAsFactors = FALSE)%>%
     clean_names() 
names(pre_dispatch_hems) <- sub("_e_.*", "", names(pre_dispatch_hems))
pre_dispatch_hems=pre_dispatch_hems[,order(names(pre_dispatch_hems))]

pre_dispatch = rbind(pre_dispatch_gems, pre_dispatch_hems)

#----------------------MERGE EMS SERVICE AND DISPATCH ----------------------#
dispatch_ems=left_join(ems_service, pre_dispatch, by=c("id"))

#----------------------ZIP CODE MERGE FILES ----------------------#
zip_2017=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/dAgency.09 & zipcode- 2017.csv",
                           header = TRUE,  
                           na.strings=c(""," ", "NA"), 
                           stringsAsFactors = FALSE)%>%
  clean_names() 

zip_2018=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/dAgency.09 & zipcode- 2018.csv",
                  header = TRUE,  
                  na.strings=c(""," ", "NA"), 
                  stringsAsFactors = FALSE)%>%
  clean_names() 

zip=rbind(zip_2017, zip_2018)

#---------------------- JOIN DISPATCH/EMS/ZIP-----------------------------------------#
dispatch_ems_zip=left_join(dispatch_ems, zip, by=c("id" = "incident_id_it_record_001"))

#---------------------PREHOSPITAL DISPOSITION MERGE GEMS AND HEMS --------------------#
disposition_gems=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/Gems/prehosp airway-disposition/disposition.csv",
                       header = TRUE,  
                       na.strings=c(""," ", "NA"), 
                       stringsAsFactors = FALSE)%>%
  clean_names() 
names(disposition_gems) <- sub("_e_.*", "", names(disposition_gems))
disposition_gems=disposition_gems[,order(names(disposition_gems))]
disposition_gems=disposition_gems[,-23]

disposition_hems=read.csv("C:/Users/User/Desktop/DAPT Semester 4/VCU-HS/data/PreHospital airway-disposition.csv",
                       header = TRUE,  
                       na.strings=c(""," ", "NA"), 
                       stringsAsFactors = FALSE)%>%
  clean_names() 
names(disposition_hems) <- sub("_e_.*", "", names(disposition_hems))
disposition_hems=disposition_hems[,order(names(disposition_hems))]

disposition=rbind(disposition_gems, disposition_hems)

#-----------------------------GET FACILITY NAME----------------------------#
facility=disposition[,c("id", "disposition_destination_name_delivered_transferred_to")] %>%
  distinct(.keep_all = TRUE)

#-----------------------------MERGE DISPATCH/ZIP/EMS/FACILITY----------------------------#
dispatch_zip_ems_fac=left_join(dispatch_ems_zip, facility, by=c("id"))

#------------------LOAD THE PREHOSPITAL FOR FACILITY/ZIP AND EMS AGENCY TO GET TIME DIFFERENCE ----#
dbSendQuery(con, "drop table prehospital_incident")
dbWriteTable(con,c('prehospital_incident'), value=dispatch_zip_ems_fac, row.names=FALSE)

