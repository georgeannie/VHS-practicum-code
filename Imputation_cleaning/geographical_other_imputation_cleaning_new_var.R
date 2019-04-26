# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(dplyr)
library(plyr)
library(stringr)
library('RPostgreSQL')
library(lazyeval)

source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")
source("/home/rstudio/R/VHS_github/VHS-practicum-code/Imputation_cleaning/function_impute_zip_ems_facility.R") 


source("C:/Users/User/Documents/VHS-practicum-code/function_aws_rds_access.R")

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

#trauma$injury_zip_imp = trauma$injury_zip_tr5_6
#trauma$ems_imp = trauma$ems_service_name_tr7_3_y
#trauma$facility_imp = trauma$facility_name
source("C:/Users/User/Documents/VHS-practicum-code/Imputation_cleaning/function_impute_zip_ems_facility.R")

trauma=unique_entry(df=trauma, 
                    var1=injury_zip_tr5_6,
                    distinct_var2 = facility_name, 
                    key_var =incident_id,
                    new_column=injury_zip_imp)
sum(is.na(trauma$injury_zip_imp))

#----------------------STEP II - Imputation for zip code---------------------#
# Find most common zip codes for each ems and impute injury zip code        #
#               Still 189 missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_imp,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id,
                    new_column=injury_zip_imp1)
sum(is.na(trauma$injury_zip_imp1))

#--------------------STEP III - Imputation for zip code---------------------#
# Find most common zip codes for each county and impute injury zip code     #
#               Still 49  missing injury zip codes missing                  #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=injury_zip_imp1,
                    distinct_var2 = injury_county_tr5_9, 
                    key_var =incident_id,
                    new_column=injury_zip_imp2)
sum(is.na(trauma$injury_zip2))

#---------------------STEP Iv - Imputation for ems service name ------------#
# Find most common zip codes for each ems and impute ems service name       #
#               Still 1481 missing ems service name missing                 #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_service_name_tr7_3_y,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id,
                    new_column=ems_imp)
sum(is.na(trauma$ems_imp))

#---------------------STEP v - Imputation for ems service name -------------#
# Find most common facility name for each ems and impute ems service name   #
#               Still 171  missing ems service name missing                 #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=ems_imp,
                    distinct_var2 = facility_name, 
                    key_var =incident_id,
                    new_column=ems_imp1)
sum(is.na(trauma$ems_imp1))

#--------------------STEP vI - Imputation for facility name ----------------#
# Find most common zip code for each facility and impute facility name      #
#               Still 104  missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_name,
                    distinct_var2 = injury_zip_tr5_6, 
                    key_var =incident_id,
                    new_column=facility_imp)
sum(is.na(trauma$facility_imp))

#-------------------STEP vII - Imputation for facility name ----------------#
# Find most common ems for each facility and impute facility name           #
#               Still 66   missing facility name missing                    #  
#---------------------------------------------------------------------------#
trauma=unique_entry(df=trauma, 
                    var1=facility_imp,
                    distinct_var2 = ems_service_name_tr7_3_y, 
                    key_var =incident_id,
                    new_column=facility_imp1)
sum(is.na(trauma$facility_imp1))

sum(is.na(trauma$injury_zip_imp3))
sum(is.na(trauma$ems_imp1))
sum(is.na(trauma$facility_imp1))

trauma = trauma %>%
  select(-injury_zip_imp, -injury_zip_imp1, -ems_imp, -facility_imp) %>%
  plyr::rename(c("injury_zip_imp2" = "injury_zip_imp", "ems_imp1" = "ems_imp",
               "facility_imp1" = "facility_imp"))

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
                trauma$patient_gender_tr1_15 == '-Select-' |
                trauma$patient_gender_tr1_15 == "Not Known" |
                trauma$patient_gender_tr1_15 ==  "Not Known/Not Recorded"),]
#---------------------------------------------------------------------------------------------------#
#REMOVE ALL MISSING AGE
#---------------------------------------------------------------------------------------------------#
trauma=trauma[!is.na(trauma$patient_age_tr1_12), ]

#---------------------------------------------------------------------------------------------------#
#REMOVE 'NOT APPLICABLE' PATIENT AGE UNITS
#---------------------------------------------------------------------------------------------------#
trauma=trauma %>% 
  filter(patient_age_units_tr1_14 == 'Years' |
           patient_age_units_tr1_14 == 'Months' |
           patient_age_units_tr1_14 == 'Days')


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
