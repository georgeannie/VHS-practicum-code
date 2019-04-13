#--------------------------------------------------------------------------------------#
#                         FEATURE ENGINEERING - NEW DATA SET FOR MODELING              #
#           WORK IN PROGRESS                                
#--------------------------------------------------------------------------------------#
library(geosphere)
library(dplyr)
library(tidyr)
library(rlist)
library(purrr)
library(lubridate)
library(zipcode)
library(stringr)
library('RPostgreSQL')

source("distance_function.R")
source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#----------------------READ ALL ROWS FROM CLEAN_TRAUMA TABLE --------------#
clean_trauma=dbGetQuery(con, 'select * from clean_trauma')
trauma_patient_motor_safety=dbGetQuery(con, 'select * from trauma_patient_motor_safety')
zipcode_info=dbGetQuery(con, 'select * from zipcode_info')
trauma_prehospital=dbGetQuery(con, 'select * from trauma_prehospital')
trauma_patient_alcohol_drug = dbGetQuery(con, 'select * from trauma_patient_alcohol_drug')
zip_facility=dbGetQuery(con, 'select * from zip_facility')
zip_ems=dbGetQuery(con, 'select * from zip_ems')

#---------------DISCONNECT POSTGRES ------#
dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)

clean_trauma1=clean_trauma

library(sqldf)
#--------------------------------------------------------------------------------------#
#THE ONLY MISSING AGE UNITS HAS A MEDICARE. HENCE CONVERTING AGE UNITS AS YEARS
#--------------------------------------------------------------------------------------#

clean_trauma1$patient_age_units_tr1_14[is.na(clean_trauma1$patient_age_units_tr1_14)] = 'Years'

#--------------------------------------------------------------------------------------#
#CONVERT AGE UNITS OF MONTHS AND DAYS TO YEARS IN 0 AND 1
#--------------------------------------------------------------------------------------#
clean_trauma1$patient_age_tr1_12 = as.numeric(clean_trauma1$patient_age_tr1_12)
clean_trauma1=sqldf('select *,
                 CASE
                 WHEN patient_age_units_tr1_14 = "Days" THEN 0
                 WHEN (patient_age_units_tr1_14 = "Months" and patient_age_tr1_12 < 12) THEN 0
                 WHEN (patient_age_units_tr1_14 = "Months" and patient_age_tr1_12 >= 12) THEN 1  
                 WHEN patient_age_units_tr1_14= "Years" THEN patient_age_tr1_12
                 END AS years
                 from clean_trauma1')

clean_trauma1=sqldf('select *,
          CASE
          WHEN years = NULL THEN "NULL"
          WHEN years <1 THEN "<1"
          WHEN years <= 5 THEN "1-5"
          WHEN years <= 10 THEN "6-10"
          WHEN years <= 17 THEN "11-17"
          WHEN years <= 34 THEN "18-34"
          WHEN years <= 49 THEN "35-49"
          WHEN years <= 64 THEN "50-64"
          ELSE "65+"
          END AS AgeBin
          FROM clean_trauma1') 


#--------------------------------------------------------------------------------------#
#CONVERT AGE UNITS OF MONTHS AND DAYS TO YEARS IN DECIMAL
#--------------------------------------------------------------------------------------#
clean_trauma1=clean_trauma1 %>%
  mutate(decimal_years= case_when(
      patient_age_units_tr1_14== "Years" ~ patient_age_tr1_12,
      patient_age_units_tr1_14 == "Days" ~ round(patient_age_tr1_12 / 365 , 2),
      patient_age_units_tr1_14 == "Months" ~ round(patient_age_tr1_12 / 12, 2)
      )
    )

#---------------------------------------------------------------------------#
#       FLAG IF VEHICLE ACCIDENT. SET 'Y' IT IF BOTH OR ONE OF THE  FIELDS  #
#       HAVE VALUE. SET ALL MISSING VALUES AS NO                            #
#---------------------------------------------------------------------------#

vehacct = trauma_patient_motor_safety %>%
  mutate(vehicle_accident = case_when((is.na(airbag_deployment_tr29_32) |
                                         airbag_deployment_tr29_32 == 'Not Applicable'|
                                         airbag_deployment_tr29_32 ==  "Airbag Not Deployed") &
                                        (is.na(safety_device_used_tr29_24) |
                                           safety_device_used_tr29_24 == 'None') ~ 'No',
                                      TRUE ~ 'Yes')) %>%
  group_by(incident_id) %>%
  arrange(incident_id, desc(vehicle_accident))

vehacct=sqldf('SELECT *, min(rowid) row_names 
              FROM vehacct
              group BY incident_id')

clean_trauma1 = clean_trauma1 %>%
  left_join(vehacct, by=c("incident_id"))

clean_trauma1$vehicle_accident[is.na(clean_trauma1$vehicle_accident)] = 'No'

#---------------------------------------------------------------------------#
#       SET MISSING VALUES FOR DRUG AND ALCOHOL USE AS NO                   #
#---------------------------------------------------------------------------#

clean_trauma1= clean_trauma1 %>% 
  mutate(alcohol_intervention_alcohol_screen_tr18_46 = coalesce(alcohol_intervention_alcohol_screen_tr18_46,
                                                                'No'),
         drug_use_indicator_tr18_45 = coalesce(drug_use_indicator_tr18_45,
                                               'No')
  )

#---------------------------------------------------------------------------#
#       GET HOUR OF EMS NOTIFIED TIME. IF EMS UNIT NOTIFIED TIME IS MISSING #
#       GET THE HOUR FROM THE INCIDENT TIME. IF INCIDENT TIME IS MISSING    #
#      GET THE HOUR FROM THE ADMISSION TIME. ASSUMING THAT THE INCIDENT TIME#
#     OR ADMISSION TIME WILL BE SIMILAR TO EMS UNIT NOTIFIED TIME          S #
#---------------------------------------------------------------------------#

clean_trauma1$ems_notify_time_hour=format(as.POSIXct(clean_trauma1$ems_unit_notified_time_tr9_10, 
                                                     format="%H:%M"),"%H")

filter = clean_trauma1 %>%
  filter(is.na(ems_notify_time_hour)) %>%
  mutate(ems_notify_time_hour=format(as.POSIXct(incident_time_tr5_18, 
                                                format="%H:%M"),"%H"))

clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% filter$incident_id) %>%
  rbind(filter)

filter = clean_trauma1 %>%
  filter(is.na(ems_notify_time_hour)) %>%
  mutate(ems_notify_time_hour=format(as.POSIXct(ed_acute_care_admission_time_tr18_56, 
                                                format="%H:%M"),"%H"))


clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% filter$incident_id) %>%
  rbind(filter)
clean_trauma1$ems_notify_time_hour = as.integer(clean_trauma1$ems_notify_time_hour)

#---------------------------------------------------------------------------#
#       GET TIME OF DAY WHEN THE EMS WAS REQUESTED FOR                      #
#---------------------------------------------------------------------------#

# create breaks to define times of day
breaks <- c("00", "5", "11", "17", "23")

# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

clean_trauma1$ems_unit_notified_time_of_day = cut(x=(clean_trauma1$ems_notify_time_hour), 
                                breaks = breaks, labels = labels, include.lowest=TRUE)

#---------------------------------------------------------------------------#
#Indication of systolic blood pressure in range based off of age.           #
# 1 meaning within range and 0 out of range.                                #
#---------------------------------------------------------------------------#

clean_trauma1$prehospital_sbp_tr18_67 = as.numeric(clean_trauma1$prehospital_sbp_tr18_67)
clean_trauma1$prehospital_respiratory_rate_tr18_70 = as.numeric(clean_trauma1$prehospital_respiratory_rate_tr18_70)

conversion=clean_trauma1

         
conversion$sbpinrange<-ifelse(conversion$years<1, 
                              (ifelse(conversion$prehospital_sbp_tr18_67>71 & conversion$prehospital_sbp_tr18_67<105,1,0)),
                              ifelse(conversion$years>=1 & conversion$years<=2,
                                     (ifelse(conversion$prehospital_sbp_tr18_67>85 & conversion$prehospital_sbp_tr18_67<107,1,0)),
                                     ifelse(conversion$years>=3 & conversion$years<=5,
                                            (ifelse(conversion$prehospital_sbp_tr18_67>88 & conversion$prehospital_sbp_tr18_67<113,1,0)),
                                            ifelse(conversion$years>=6 & conversion$years<=9,       
                                                   (ifelse(conversion$prehospital_sbp_tr18_67>96 & conversion$prehospital_sbp_tr18_67<116,1,0)),
                                                   ifelse(conversion$years>=10 & conversion$years<=11, 
                                                          (ifelse(conversion$prehospital_sbp_tr18_67>101 & conversion$prehospital_sbp_tr18_67<121,1,0)),
                                                          ifelse(conversion$years>=12 & conversion$years<=15, 
                                                                 (ifelse(conversion$prehospital_sbp_tr18_67>109 & conversion$prehospital_sbp_tr18_67<132,1,0)),
                                                                 ifelse(conversion$years>15,
                                                                        (ifelse(conversion$prehospital_sbp_tr18_67>89 & conversion$prehospital_sbp_tr18_67<121,1,0)),
                                                                        0
                                                                 )))))))            

conversion$sbpinrange[is.na(conversion$sbpinrange)] = '1'

#---------------------------------------------------------------------------#
# Indication of respiratory rate within range based off of age.  1 meaning  #
# within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#

conversion$respinrange<-ifelse(conversion$years<1, 
                               (ifelse(conversion$prehospital_respiratory_rate_tr18_70>29 & conversion$prehospital_respiratory_rate_tr18_70<54,1,0)),
                               ifelse(conversion$years>=1 & conversion$Years<3,
                                      (ifelse(conversion$prehospital_respiratory_rate_tr18_70>21 & conversion$prehospital_respiratory_rate_tr18_70<38,1,0)),
                                      ifelse(conversion$years>=3 & conversion$years<6,
                                             (ifelse(conversion$prehospital_respiratory_rate_tr18_70>19 & conversion$prehospital_respiratory_rate_tr18_70<29,1,0)),
                                             ifelse(conversion$years>=6 & conversion$years<12,       
                                                    (ifelse(conversion$prehospital_respiratory_rate_tr18_70>17 & conversion$prehospital_respiratory_rate_tr18_70<26,1,0)),
                                                    ifelse(conversion$years>=12 & conversion$years<16, 
                                                           (ifelse(conversion$prehospital_respiratory_rate_tr18_70>11 & conversion$prehospital_respiratory_rate_tr18_70<21,1,0)),
                                                           ifelse(conversion$years>=16, 
                                                                  (ifelse(conversion$prehospital_respiratory_rate_tr18_70>11 & conversion$prehospital_respiratory_rate_tr18_70<19,1,0)),
                                                                  0
                                                           )))))) 

conversion$respinrange[is.na(conversion$respinrange)] = '1'

#---------------------------------------------------------------------------#
# Indication of pulse rate within range based off of age.  1 meaning        #
# within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(pulseinrange = case_when( 
    (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
            (prehospital_pulse_rate_tr18_69> 99 & prehospital_pulse_rate_tr18_69<206) ~ 1,
    ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
       (prehospital_pulse_rate_tr18_69> 99 & prehospital_pulse_rate_tr18_69 < 191) ~ 1,
    ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
       (prehospital_pulse_rate_tr18_69> 97 & prehospital_pulse_rate_tr18_69<141) ~ 1,
    (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
      (prehospital_pulse_rate_tr18_69> 79 & prehospital_pulse_rate_tr18_69<121) ~ 1,
    (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
      (prehospital_pulse_rate_tr18_69> 74 & prehospital_pulse_rate_tr18_69<119) ~ 1,
    (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
      (prehospital_pulse_rate_tr18_69> 59 & prehospital_pulse_rate_tr18_69<101) ~ 1,
    (is.na(prehospital_pulse_rate_tr18_69)) ~ 1,
    TRUE ~ 0)
  )

#---------------------------------------------------------------------------#
# Indication of oximetry.  1 meanin within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#

conversion$oximetryinrange = ifelse(conversion$prehospital_pulse_oximetry_tr18_82>94 & 
                               conversion$prehospital_pulse_oximetry_tr18_82<101,1,0)

conversion$oximetryinrange[is.na(conversion$oximetryinrange)] = '1'

#---------------------------------------------------------------------------#
# Find difference from normal in sbp for all ages                           #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_sbp = 
        case_when(prehospital_sbp_tr18_67 == 0 ~ 0,
                  (years < 1 & prehospital_sbp_tr18_67 < 72) ~   (prehospital_sbp_tr18_67 - 72),
                  (years < 1 & prehospital_sbp_tr18_67 > 104) ~  (prehospital_sbp_tr18_67 - 104),
                  (years >=1 & years < 3) & 
                    (prehospital_sbp_tr18_67 < 86) ~  (prehospital_sbp_tr18_67 - 86) ,
                  (years >=1 & years < 3) &
                    (prehospital_sbp_tr18_67 > 106) ~  (prehospital_sbp_tr18_67 - 106) ,
                  (years >=3 & years < 6) &
                    (prehospital_sbp_tr18_67 < 89) ~  (prehospital_sbp_tr18_67 - 89),
                  (years >=3 & years < 6) &
                    (prehospital_sbp_tr18_67 > 112) ~  (prehospital_sbp_tr18_67 - 112),
                  (years >=6 & years < 10) &
                    (prehospital_sbp_tr18_67 < 97) ~  (prehospital_sbp_tr18_67 - 97),
                  (years >=6 & years < 10) &
                    (prehospital_sbp_tr18_67 > 115) ~  (prehospital_sbp_tr18_67 - 115),
                  (years >=10 & years < 12) &
                    (prehospital_sbp_tr18_67 < 101) ~  (prehospital_sbp_tr18_67 - 101),
                  (years >=10 & years < 12) &
                    (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
                  (years >=12& years < 16) &
                    (prehospital_sbp_tr18_67 < 110) ~  (prehospital_sbp_tr18_67 - 110),
                  (years >=12 & years < 16) &
                    (prehospital_sbp_tr18_67 > 131) ~  (prehospital_sbp_tr18_67 - 131),
                  (years >=16) &
                    (prehospital_sbp_tr18_67 < 90) ~  (prehospital_sbp_tr18_67 - 90),
                  (years >=16) &
                    (prehospital_sbp_tr18_67 > 120) ~  (prehospital_sbp_tr18_67 - 120),
                  TRUE ~ 0
                  ))

#---------------------------------------------------------------------------#
# Find difference from normal in respiratory for all ages                   #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_resp = 
           case_when(prehospital_respiratory_rate_tr18_70 == 0 ~ 0,
                     (years < 1 & prehospital_respiratory_rate_tr18_70 < 30) ~ (prehospital_respiratory_rate_tr18_70 - 30),
                     (years < 1 & prehospital_respiratory_rate_tr18_70 > 53) ~ (prehospital_respiratory_rate_tr18_70 - 53),
                     (years >=1 & years < 3) & 
                       (prehospital_respiratory_rate_tr18_70 < 22) ~  (prehospital_respiratory_rate_tr18_70 - 22) ,
                     (years >=1 & years < 3) &
                       (prehospital_respiratory_rate_tr18_70 > 37) ~  (prehospital_respiratory_rate_tr18_70 - 37) ,
                     (years >=3 & years < 6) &
                       (prehospital_respiratory_rate_tr18_70 < 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
                     (years >=3 & years < 6) &
                       (prehospital_respiratory_rate_tr18_70 > 28) ~  (prehospital_respiratory_rate_tr18_70 - 28),
                     (years >=6 & years < 12) &
                       (prehospital_respiratory_rate_tr18_70 < 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
                     (years >=6 & years < 12) &
                       (prehospital_respiratory_rate_tr18_70 > 25) ~  (prehospital_respiratory_rate_tr18_70 - 25),
                     (years >=12 & years < 16) &
                       (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
                     (years >=12 & years < 16) &
                       (prehospital_respiratory_rate_tr18_70 > 20) ~  (prehospital_respiratory_rate_tr18_70 - 20),
                     (years >=16) &
                       (prehospital_respiratory_rate_tr18_70 < 12) ~  (prehospital_respiratory_rate_tr18_70 - 12),
                     (years >=16) &
                       (prehospital_respiratory_rate_tr18_70 > 18) ~  (prehospital_respiratory_rate_tr18_70 - 18),
                     TRUE ~ 0
           ))


#---------------------------------------------------------------------------#
# Find the difference from normal of pulse                                  #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_pulse = case_when( 
    (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
       prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),
    
    (patient_age_tr1_12 <28 & patient_age_units_tr1_14 == 'Days') &
       prehospital_pulse_rate_tr18_69 > 205         ~ (prehospital_pulse_rate_tr18_69 - 205),
    
    ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
       prehospital_pulse_rate_tr18_69 < 100        ~ (prehospital_pulse_rate_tr18_69 - 100),
    
    ((patient_age_tr1_12 >=28 & patient_age_units_tr1_14 == 'Days') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <=12 & patient_age_units_tr1_14 == 'Months')) &
        prehospital_pulse_rate_tr18_69 > 190      ~ (prehospital_pulse_rate_tr18_69 - 190),
    
    ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
      prehospital_pulse_rate_tr18_69 < 98         ~ (prehospital_pulse_rate_tr18_69 - 98),
    
    ((patient_age_tr1_12 >12 & patient_age_tr1_12 <= 24 & patient_age_units_tr1_14 == 'Months') |
       (patient_age_tr1_12 >=1 & patient_age_tr1_12 <= 2 & patient_age_units_tr1_14 == 'Years')) &
       prehospital_pulse_rate_tr18_69 >140         ~ (prehospital_pulse_rate_tr18_69 - 140),
    
    (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
       prehospital_pulse_rate_tr18_69 < 80         ~ (prehospital_pulse_rate_tr18_69 - 80),
                                                      
    (patient_age_tr1_12 >=3 & patient_age_tr1_12 <= 5 & patient_age_units_tr1_14 == 'Years') &
       prehospital_pulse_rate_tr18_69 > 120        ~ (prehospital_pulse_rate_tr18_69 - 120),
    
    (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &
       prehospital_pulse_rate_tr18_69 < 75          ~ (prehospital_pulse_rate_tr18_69 - 75),  
    
    (patient_age_tr1_12 >=6 & patient_age_tr1_12 <= 11 & patient_age_units_tr1_14 == 'Years') &   
       prehospital_pulse_rate_tr18_69 > 118        ~ (prehospital_pulse_rate_tr18_69 - 118),
    
    (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &
       prehospital_pulse_rate_tr18_69 < 60           ~ (prehospital_pulse_rate_tr18_69 - 60),
    
    (patient_age_tr1_12 >=12 & patient_age_units_tr1_14 == 'Years') &   
       prehospital_pulse_rate_tr18_69 > 100          ~ (prehospital_pulse_rate_tr18_69 - 100),
    
    (is.na(prehospital_pulse_rate_tr18_69)) ~ 0,
    TRUE ~ 0)
  )

#---------------------------------------------------------------------------#
# Find the difference from pulse oximetry                                   #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_in_oximetry= case_when(
       prehospital_pulse_oximetry_tr18_82 == 0 ~  0,
       prehospital_pulse_oximetry_tr18_82 < 95 ~  prehospital_pulse_oximetry_tr18_82 - 95,
       prehospital_pulse_oximetry_tr18_82 > 100 ~  prehospital_pulse_oximetry_tr18_82 - 100,
       TRUE ~ 0)
  )

clean_trauma1 = conversion

#---------------------------------------------------------------------------#
#              SET A SEVERITY FLAG BASED ON GCS TOTAL SCORE                 #
#              TOTAL < 8 - SEVERE, TOTAL > 8 & <= 12 MODERATE,              #
#              TOTAL > 12 & <=14 MILD, TOTAL > 14 MINOR                     # 
#---------------------------------------------------------------------------#

clean_trauma1 = clean_trauma1 %>%
  mutate(gcs_severity_flag=case_when((prehospital_gcs_total_manual_tr18_64 > 14 | 
                                        prehospital_gcs_total_manual_tr18_64 == 0) ~ 'Minor',
                                      prehospital_gcs_total_manual_tr18_64 <= 8 ~ 'Severe',
                                     (prehospital_gcs_total_manual_tr18_64 > 8 &
                                        prehospital_gcs_total_manual_tr18_64 <=12) ~ 'Moderate',
                                     (prehospital_gcs_total_manual_tr18_64 > 12 &
                                        prehospital_gcs_total_manual_tr18_64 <=14) ~ 'Mild'
                                     )
  )


#-------------------------------------------------------------------#
#FIND DISTANCE FROM SCENE TO HOSPITAL
#-------------------------------------------------------------------#
#LOAD ZIPCODE DF
data("zipcode")
zip_lat = zipcode

zip_facility$zip_code = as.character(zip_facility$zip_code)
zip_facility$facility_name = toupper(zip_facility$facility_name)

clean_trauma1$facility_name = toupper(clean_trauma1$facility_name)

clean_trauma1 = clean_trauma1 %>% 
  left_join(zip_facility, by = 'facility_name') %>%
  plyr::rename(c("zip_code"="facility_zip"))

#JOIN NEW ZIPCODE DF TO CLEAN_TRAUMA
temp = clean_trauma1 %>% 
  left_join(zip_lat, by = c("injury_zip_tr5_6" = "zip"), copy = FALSE) %>%
  plyr::rename(c("latitude"="injury_lat", "longitude"="injury_long"))

temp = temp %>% 
  left_join(zip_lat, by = c("facility_zip" = "zip"), copy = FALSE) %>%
  plyr::rename(c("latitude"="facility_lat", "longitude" = "facility_long"))

#CALC DISTANCE BETWEEN INJURY AND FACILITY
temp = temp %>% 
  mutate(
    distance = get_geo_distance(injury_long, injury_lat, facility_long, facility_lat),
    drive_dist_to_facility = round((distance + 1)*1.30, 2),
    ground_time_to_facility = round(drive_dist_to_facility/30 * 60, 0)
  ) %>%
  select(incident_id, drive_dist_to_facility, ground_time_to_facility )

clean_trauma1 = clean_trauma1 %>%
  left_join(temp, by=c("incident_id"))

#-------------------------------------------------------------------#
#FIND DISTANCE FROM EMS TO SCENE
#-------------------------------------------------------------------#
zip_ems$zip = as.character(zip_ems$zip)
zip_ems$ems_service_name_tr7_3= toupper(zip_ems$ems_service_name_tr7_3)

clean_trauma1$ems_service_name_tr7_3_y = toupper(clean_trauma1$ems_service_name_tr7_3_y)

temp = clean_trauma1 %>% 
  left_join(zip_ems, by = c("ems_service_name_tr7_3_y" =  "ems_service_name_tr7_3"), 
            copy = FALSE) %>%
  plyr::rename(c("zip"="ems_zip"))

#JOIN NEW ZIPCODE DF TO CLEAN_TRAUMA
temp = temp %>% 
  left_join(zip_lat, by = c("injury_zip_tr5_6" = "zip"), copy = FALSE) %>%
  plyr::rename(c("latitude" = "injury_lat", "longitude" = "injury_long" ))

temp = temp %>% 
  left_join(zip_lat, by = c("ems_zip" = "zip"), copy = FALSE) %>%
  plyr::rename(c("latitude"="ems_lat",  "longitude" = "ems_long"))

#REARRANGE INCIDENT ID AND DISTANCE BY THE SHORTEST DISTANCE FOR EACH INCIDENT
temp = temp %>%
  mutate(
    distance = get_geo_distance(injury_long, injury_lat, ems_long, ems_lat),
    drive_dist_from_ems = round((distance + 1)*1.3, 2), 
    ground_time_from_ems = round(drive_dist_from_ems/30 * 60, 0)
  ) %>%
  select(incident_id, ems_zip, drive_dist_from_ems, ground_time_from_ems) %>%
  arrange(incident_id, drive_dist_from_ems)

#SELECT THE FIRST ROW OF EACH INCIDENT ID TO THE SHORTEST DISTANCE WHICH IS ARRANGED IN THE 
# STEP ABOVE
closest_ems=sqldf('SELECT *, min(rowid) row_names 
              FROM temp
              group BY incident_id')

clean_trauma1 = clean_trauma1 %>%
  left_join(closest_ems, by=c("incident_id"))

#---------------------------------------------------------------------------#
#               DEFINE RURAL/URBAN AREA OF INJURY SCENE                     #
#---------------------------------------------------------------------------#
zipcode_info$zip_code = as.character(zipcode_info$zip_code)
clean_trauma1 = clean_trauma1 %>%
  left_join(zipcode_info, by=c( "injury_zip_tr5_6" = "zip_code")) 

#---------------------------------------------------------------------------#
#               DEFINE OUTCOME IF HELICOPTER IS NEEDED OR NOT?              #
#  CALCULATE DAYS IN HOSPITAL AS DIFFERENCE IN DAYS BETWEEN DISCHARGE DATE  #
# AND ADMISSION DATE                                                        #
#    DISPOSTION - DECEASED/EXPIRED, ICU, OR OR TRANSFER, OUTCOME IS Y       #
#    DAYS IN HOSPITAL <1, OUTCOME IS N                                      #
#    DAYS IN HOSPITAL > 0 AND TIME TO HOSPITAL > 60 MINS, OUTCOME IS Y      #
#    ELSE, N                                                                #  
#---------------------------------------------------------------------------#

clean_trauma1$days_in_hospital = as.numeric(clean_trauma1$hospital_discharge_date_tr25_34 - 
                                              clean_trauma1$ed_acute_care_admission_date_tr18_55)

clean_trauma1 = clean_trauma1 %>% mutate(Outcome = case_when(
  (ed_acute_care_disposition_tr17_27 %in% c("Deceased/Expired", "Intensive Care Unit", "Operating room" ,
                                            "Trasferred to another hospital") |
     hospital_discharge_disposition_tr25_27 %in%  c("Deceased/Expired", "Died in the hospital")) &
     ground_time_to_facility  > 59 ~ 'Y',
  (days_in_hospital >1  & ground_time_to_facility  > 59 ) ~ 'Y',
  (days_in_hospital <= 1) ~ 'N',
  TRUE ~ 'N'
))

#---------------------------------------------------------------------------#
#               DEFINE OUTCOME WTHOUT TIME FOR AMPT SCORE                   #
#    DISPOSTION - DECEASED/EXPIRED, ICU, OR OR TRANSFER, OUTCOME IS Y       #
#    DAYS IN HOSPITAL <1, OUTCOME IS N                                      #
#    DAYS IN HOSPITAL > 0 AND TIME TO HOSPITAL > 60 MINS, OUTCOME IS Y      #
#    ELSE, N                                                                #  
#---------------------------------------------------------------------------#
clean_trauma1 = clean_trauma1 %>% mutate(ampt_score_Outcome = case_when(
  (ed_acute_care_disposition_tr17_27 %in% c("Deceased/Expired", "Intensive Care Unit", 
                                            "Operating room" ,
                                            "Trasferred to another hospital") |
     hospital_discharge_disposition_tr25_27 %in%  
                       c("Deceased/Expired", "Died in the hospital")) ~ 'Y',
  (days_in_hospital >1) ~ 'Y',
  (days_in_hospital <= 1) ~ 'N',
  TRUE ~ 'N'
))

clean_trauma1=clean_trauma1[,order(names(clean_trauma1))]

library('RPostgreSQL')
source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

dbSendQuery(con, "drop table feature_set_trauma")
dbWriteTable(con,c('feature_set_trauma'), value=clean_trauma1, row.names=FALSE)
