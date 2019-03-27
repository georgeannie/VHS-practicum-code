#--------------------------------------------------------------------------------------#
#                         FEATURE ENGINEERING - NEW DATA SET FOR MODELING              #
#           WORK IN PROGRESS                                
#--------------------------------------------------------------------------------------#
library(geosphere)
library(dplyr)
library(tidyr)
library(rlist)
library(purr)
library('RPostgreSQL')
source("aws_rds_access.R")
source("distance.R")

#------------------------------CONNECT TO AWS RDS -----------------------------#
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host="vhs.cz9sqac5flvw.us-east-2.rds.amazonaws.com",
              user = "vcudapt2019", 
              password = "vcudapt2019")

#----------------------READ ALL ROWS FROM TRAUMA TABLE --------------#
trauma_patient_ed=dbGetQuery(con, 'select * from trauma_patient_ed')
trauma_patient_motor_safety=dbGetQuery(con, 'select * from trauma_patient_motor_safety')
zipcodes=dbGetQuery(con, 'select * from zipcode_info')
trauma_incident = dbGetQuery(con, 'select * from trauma_incident')



#PLACEHOLDER FOR PREHOSPITAL PARAMETERS

#IMPUTATION FOR MISSING PREHOSPITAL PARAMETRERS (TRAUMA)

#PLACEHOLDER FOR PREHOSPITAL GCS

separate(`prehospital_gcs_eye`, into = c("Prehospital_GCS_EYE_SEP"), sep = "\\s+", remove = FALSE, convert = FALSE, extra = "drop")
separate(`prehospital_gcs_verbal`, into = c("Prehospital_GCS_Verbal_SEP"), sep = "\\s+", remove = FALSE, convert = FALSE, extra = "drop")
separate(`prehospital_gcs_motor`, into = c("Prehospital_GCS_Motor_SEP"), sep = "\\s+", remove = FALSE, convert = FALSE, extra = "drop")

#IMPUTATION FO GCS

#PLACEHOLDER FOR HOUR/TIME OF DAY OF UNIT DISPATCHED

#IMPUTATION FOR TIME

#PLACEHOLDER FOR DISTANCE FROM EMS AND TO HOSPITAL
injury_zip=data.frame("incident_id" = trauma_patient_ed$agency_number_d_agency_02,
                      "zip_code"=trauma_patient_ed$incident_agency_location_postal_code_d_location_09)

lat_lang_injury=sqldf('select a.latitude as injury_lat, a.longitude as injury_long, 
                       a.zip_code, b.incident_id
                  from injury_zip b
                  left join zipcodes a
                  on a.zip_code = b.zip_code')

lat_lang_hosp=sqldf('select a.latitude as hosp_lat, a.longitude as hosp_long, 
                      a.zip_code, b.incident_id
                  from injury_zip b
                  left join facility_zip c
                  on b.facility_name = c.facility_name
                       join zipcodes a 
                  on b.zipcode = a.zip_code')

lat_lang_ems=sqldf('select a.latitude as ems_lat, a.longitude as ems_long, 
                      a.zip_code, b.incident_id
                  from injury_zip b
                  left join ems_zip c
                  on b.ems_name = c.ems_service_name
                       join zipcodes a 
                  on b.zipcode = a.zip_code')

geo_lat_lang=left_join(lat_lang_injury, lat_lang_hosp, by="incident_id")
geo_lat_lang=left_join(geo_lat_lang, lat_lang_ems, by="incident_id")

trauma_patient_ed$distance_to_hospital=get_geo_distance(geo_lat_lang$injury_long, 
                                                      geo_lat_lang$injury_lat, 
                                                      geo_lat_lang$hosp_long, 
                                                      geo_lat_lang$hosp_lat) 

trauma_patient_ed$distance_to_ems=get_geo_distance(geo_lat_lang$injury_long, 
                                                        geo_lat_lang$injury_lat, 
                                                        geo_lat_lang$ems_long, 
                                                        geo_lat_lang$ems_lat) 

#IMPUTATION FOR DISTANCE


#PLACEHOLDER FOR MOTOR VEHICLE
mv_incident=data.frame("incident_id"=unique(trauma_patient_motor_safety$incident_id)) 
mv_incident$vehicle_related

#IMPUTATION FOR MISSING VEHICLE ACCIDENT FLAG



#PLACEHOLDER FOR TIME INTERVAL OF ARRIVAL AT SCENEs
table_prehospital = dispatch_zip_ems_fac[!is.na(dispatch_zip_ems_fac$scene_incident_postal_code_e_scene_19), ]

table_trauma = trauma_patient_ed
to_scene = sqldf('select a.ems_service_name_tr7_3, a.injury_zip_tr5_6, 
                 b.incident_unit_arrived_on_scene_date_time, incident_unit_notified_by_dispatch_date_time
                 from table_trauma a
                 left join table_prehospital b
                  on a.injury_zip_tr5_6 = b.scene_incident_postal_code_e_scene_19
                  and a.ems_service_name_tr7_3 = b.response_ems_agency_name
                  and (a.injury_zip_tr5_6 > " "
                   AND a.ems_service_name_tr7_3 > " ")')
to_scene=to_scene%>%
    distinct(.keep_all = TRUE) 
  
#PLACEHOLDER FOR TIME INTERVAL OF ARRIVAL AT HOSPITAL

#IMPUTATION FOR TIME INTERVAL


# create breaks to define times of day
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

trauma_incident$ems_unit_notified_time_of_day<- cut(x=hour(trauma_incident$ems_unit_notified_time_tr9_10), breaks = breaks, labels = labels, include.lowest=TRUE)

############## Hour Number ####################

trauma$ems_notify_time_hour <- hour(trauma_incident$ems_unit_notified_time_tr9_10)

#PLACEHOLDER FOR COMORBIDITY - REDUCE NUMBER OF FACTORS

#PLACEHOLDER FOR OUTCOME



