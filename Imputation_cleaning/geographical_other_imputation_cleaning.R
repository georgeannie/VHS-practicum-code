# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(stringr)
library('RPostgreSQL')
library(lazyeval)

source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")
source("/home/rstudio/R/VHS_github/VHS-practicum-code/Imputation_cleaning/function_impute_zip_ems_facility.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
            password = password)


trauma_patient_ed=dbGetQuery(con, 'select * from trauma_patient_ed')
trauma_incident=dbGetQuery(con, 'select * from trauma_incident')
prehospital=dbGetQuery(con, 'select * from prehospital_incident')

#---------------DISCONNECT POSTGRES ------#
dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)

#---------------ATTACH SQLDF ------#
library(sqldf)
ems_service = sqldf('select b.ems_service_name_tr7_3_y
                    from trauma_incident a
                    left join trauma_patient_ed b
                    on a.incident_id = b.incident_id')

facility = sqldf('select b.facility_name
                 from trauma_incident a
                 left join trauma_patient_ed  b
                 on a.incident_id = b.incident_id')

zip=sqldf('select injury_zip_tr5_6
          from trauma_incident a
          left join trauma_patient_ed  b
          on a.incident_id = b.incident_id')

trauma=merge(trauma_incident, trauma_patient_ed, by=c('incident_id'))

trauma = trauma %>%
  mutate(ems_unit_notified_time_tr9_10_parse = parse_character(ems_unit_notified_time_tr9_10)) %>%
  separate(ems_unit_notified_time_tr9_10_parse, into = c("ems_unit_notified_time_hr"), 
           sep = "\\s*\\:\\s*", convert = TRUE)

#---------------------------------------------------------------------------#
# 280 records missing service, facility, zip, county. Ignore these records  #
# 161 records of these are on scene and not interfacility leaving with      #
# 51597 records that are on scene                                           #
#---------------------------------------------------------------------------#
null_trauma=trauma[ (is.na(trauma$injury_zip_tr5_6) &
                   (is.na(trauma$injury_county_tr5_9) | trauma$injury_county_tr5_9 == 'Not Applicable') &
                   is.na(trauma$facility_name) &
                   is.na(trauma$ems_service_name_tr7_3_y)) &
                 trauma$inter_facility_transfer_tr25_54 == 'Yes',]

trauma=trauma[ !(is.na(trauma$injury_zip_tr5_6) &
            (is.na(trauma$injury_county_tr5_9) | trauma$injury_county_tr5_9 == 'Not Applicable') &
             is.na(trauma$facility_name) &
              is.na(trauma$ems_service_name_tr7_3_y)) &
              trauma$inter_facility_transfer_tr25_54 == 'No',]

#---------------------------------------------------------------------------#
#                       Analysis                                            #
#             6902 records - missing zip codes                              #
#            26182 records missing ems service name                         #
#             1895 records missing facilty name                             #
#----------------------STEP I - Imputation for zip code---------------------#
# Find most common zip codes for each facility and impute injury zip code   #
#               Still 285 missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#

trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = facility_name, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#----------------------STEP II - Imputation for zip code---------------------#
# Find most common zip codes for each ems and impute injury zip code        #
#               Still 189 missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#--------------------STEP III - Imputation for zip code---------------------#
# Find most common zip codes for each county and impute injury zip code     #
#               Still 49  missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = injury_county_tr5_9, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#---------------------STEP Iv - Imputation for ems service name ------------#
# Find most common zip codes for each ems and impute ems service name       #
#               Still 1481 missing ems service name missing                 #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id)
sum(is.na(trauma$ems_service_name_tr7_3_y))

#---------------------STEP v - Imputation for ems service name -------------#
# Find most common facility name for each ems and impute ems service name   #
#               Still 171  missing ems service name missing                 #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = facility_name, 
                    key_var =incident_id)
sum(is.na(trauma$ems_service_name_tr7_3_y))

#--------------------STEP vI - Imputation for facility name ----------------#
# Find most common zip code for each facility and impute facility name      #
#               Still 104  missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id)
sum(is.na(trauma$facility_name_y))

#-------------------STEP vII - Imputation for facility name ----------------#
# Find most common ems for each facility and impute facility name           #
#               Still 66   missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id)
sum(is.na(trauma$facility_name))

#----------------------STEP IX - Imputation for zip code--------------------#
# Find most common zip codes for each facility and impute injury zip code   #
#               Still 2   missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = facility_name, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#----------------------STEP X - Imputation for zip code---------------------#
# Find most common zip codes for each ems and impute injury zip code        #
#               Still 2 missing injury zip codes missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#--------------------STEP XI - Imputation for zip code----------------------#
# Find most common zip codes for each county and impute injury zip code     #
#               Still 2  missing injury zip codes missing                   #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = injury_county_tr5_9, 
                    key_var =incident_id)
sum(is.na(trauma$injury_zip_tr5_6))

#---------------------STEP XII- Imputation for ems service name ------------#
# Find most common zip codes for each ems and impute ems service name       #
#               Still 66 missing ems service name missing                   #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id)
sum(is.na(trauma$ems_service_name_tr7_3_y))

#-------------------STEP XIV - Imputation for ems service name -------------#
# Find most common facility name for each ems and impute ems service name   #
#               Still 66   missing ems service name missing                 #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = facility_name, 
                    key_var =incident_id)
sum(is.na(trauma$ems_service_name_tr7_3_y))

#--------------------STEP XV - Imputation for facility name ----------------#
# Find most common zip code for each facility and impute facility name      #
#               Still 66  missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name_y,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id)
sum(is.na(trauma$facility_name_y))

#-------------------STEP XVI - Imputation for facility name ----------------#
# Find most common ems for each facility and impute facility name           #
#               Still 66   missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id)
sum(is.na(trauma$facility_name))

#-----------------STEP XVII - Imputation for ems service name -------------#
# Find most common county name for each ems and impute ems service name    #
#               Still 2  missing ems service name missing                  #  
#--------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = injury_county_tr5_9, 
                    key_var =incident_id)
sum(is.na(trauma$ems_service_name_tr7_3_y))

#-----------------STEP XVIII - Imputation for facility name ----------------#
# Find most common ems for each facility and impute facility name           #
#               Still 2   missing facility name missing                     #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name,
                    distinct_var2 = injury_county_tr5_9, 
                    key_var =incident_id)
sum(is.na(trauma$facility_name))


#-----------------Analysis using trauma dataset-----------------------------#
#     Using trauma dataset alone leaves us with 2 missing records with      #
#     missing ems, 2 records with missing zip and 2 records with missing    #
#     facility. Hence prehospital data will be used for imputation of the 6 #
#     records                                                               #    
#---------------------------------------------------------------------------#
prehospital_fac_based_on_zip=get_entry(df=prehospital, 
                    var1=scene_incident_postal_code_e_scene_19,
                    distinct_var2 = disposition_destination_name_delivered_transferred_to
                  )

prehospital_ems_based_on_zip=get_entry(df=prehospital, 
                             var1=scene_incident_postal_code_e_scene_19,
                             distinct_var2 = response_ems_agency_name
                  )

prehospital_ems_based_on_fac=get_entry(df=prehospital, 
                                 var1=disposition_destination_name_delivered_transferred_to,
                                 distinct_var2 = response_ems_agency_name
)

prehospital_fac_based_on_ems=get_entry(df=prehospital, 
                                     var1=response_ems_agency_name,
                                     distinct_var2 = disposition_destination_name_delivered_transferred_to 
)

prehospital_zip_based_on_ems=get_entry(df=prehospital, 
                                 var1= response_ems_agency_name,
                                 distinct_var2 = scene_incident_postal_code_e_scene_19
)

prehospital_zip_based_on_fac=get_entry(df=prehospital, 
                                     var1= disposition_destination_name_delivered_transferred_to,
                                     distinct_var2 = scene_incident_postal_code_e_scene_19
)

#------------------Imputation based on prehospital data --------------------------------------------#
#  Incident id | zip code | facility name                      | ems agency                         #
# -------------|----------|------------------------------------|------------------------------------#
#   2881927    | ?? 20110 | Lake Ridge Medical Campus (ED in   | ??CITY OF MANASSAS FIRE & RESCUE   #
#              |          | prehospital)                       |                                    #
#   2850567    | ?? 23113 | ?? VCU Health Systems              | Virginia State Police - Medflight I#
#   2826937    | 28560    | ??                                 | ??                                 #
#---------------------------------------------------------------------------------------------------#
#     ?? - the missing field. Since incident id 2826937 has no facility or ems agency name for zip  #
#     code 28560, this incident will be removed from the trauma dataset                             #
#---------------------------------------------------------------------------------------------------#
trauma$injury_zip_tr5_6[trauma$incident_id == "2881927"] = '20110'
trauma$ems_service_name_tr7_3_y[trauma$incident_id == "2881927"] = 'CITY OF MANASSAS FIRE & RESCUE'

trauma$injury_zip_tr5_6[trauma$incident_id == "2850567"] = '23113'
trauma$facility_name[trauma$incident_id == "2850567"] = 'VCU Health Systems'

remove_trauma=trauma[trauma$incident_id == "2826937",] 
trauma=trauma[!trauma$incident_id == "2826937",] 

sum(is.na(trauma$facility_name))
sum(is.na(trauma$ems_service_name_tr7_3_y))
sum(is.na(trauma$injury_zip_tr5_6))

trauma=rbind(trauma, null_trauma, remove_trauma)

#---------------------------------------------------------------------------------------------------#
#REMOVE ALL INTERFACILITY TRANSFER
#---------------------------------------------------------------------------------------------------#
trauma=trauma[trauma$inter_facility_transfer_tr25_54 == 'No',]

#---------------------------------------------------------------------------------------------------#
#REMOVE AL MISSING GENDER AND THOSE WITH VALUE 'NOT APPLICABLE'
#---------------------------------------------------------------------------------------------------#
trauma=trauma[!(trauma$patient_gender_tr1_15 == 'Not Applicable' |
                is.na(trauma$patient_gender_tr1_15) |
                trauma$patient_gender_tr1_15 == '-Select-'),]
                trauma$patient_gender_tr1_15 == "Not Known" |
                trauma$patient_gender_tr1_15 ==  "Not Known/Not Recorded"),]
#---------------------------------------------------------------------------------------------------#
#REMOVE ALL MISSING AGE
#---------------------------------------------------------------------------------------------------#
trauma=trauma[!is.na(trauma$patient_age_tr1_12), ]

#---------------------------------------------------------------------------------------------------#
#3 PATIENT UNITS HAVE MONTHS/DAYS BUT CARRY A MEDICARE. THEY HAVE OTHER FIELDS WHICH INDICATE 
# THEY ARE NOT CHILDREN.  SO CHANGING UNITS TO YEARS 
# 8 OTHER PATIENT UNITS WITH MONTHS/DAYS CARRY A MEDICARE BUT THEIR AGE CANNOT BE DETERMINED.
# HENCE 8 ROWS WILL BE DELETED
#---------------------------------------------------------------------------------------------------#
test=trauma%>%
     filter((patient_age_units_tr1_14 == "Months" |
             patient_age_units_tr1_14 == "Days") &
             financial_primary_method_of_payment_tr2_5 == "Medicare")

change_years=trauma %>% 
  filter((patient_age_units_tr1_14 == "Months" |
            patient_age_units_tr1_14 == "Days") &
           financial_primary_method_of_payment_tr2_5 == "Medicare" &
           patient_age_tr1_12 > 10) %>%
  mutate(patient_age_units_tr1_14 = "Years")

trauma=trauma %>%
  filter(!incident_id %in% test$incident_id) %>%
  rbind(change_years)

#---------------------------------------------------------------------------------------------------#
#  REMOVE ED-ACUTE CARE DISPOSITION OF 'NOT APPLICABLE', 'NOT KNOWN', 'NOT KNOWN/NOT RECORDED       #
#---------------------------------------------------------------------------------------------------#
trauma=trauma %>%
  filter(! (ed_acute_care_disposition_tr17_27 == 'Not Applicable' |
              ed_acute_care_disposition_tr17_27 == 'Not Known' |
              ed_acute_care_disposition_tr17_27 == 'Not Known/Not Recorded'))

#---------------------------------------------------------------------------------------------------#
#Check the missing values in facility, ems service and zip code
#---------------------------------------------------------------------------------------------------#
sum(is.na(trauma$facility_name_y))
sum(is.na(trauma$ems_service_name_tr7_3_y))
sum(is.na(trauma$injury_zip_tr5_6))


library(RPostgreSQL)
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

dbSendQuery(con, "drop table clean_trauma")
dbWriteTable(con,c('clean_trauma'), value=trauma, row.names=FALSE)

dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)
