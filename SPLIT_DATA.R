###########################################################################################################
#                 SPLIT FILES TO AVOID DUPLICATE AND ENABLE EASY MODELING                                 #
#                             LOAD POSTGRES TABLE IN AWS                                                  #
###########################################################################################################
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(openxlsx)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(bit64)
#-------------------Connect to DB -------------------------------# 
library('RPostgreSQL')
source("aws_rds_access.R")
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#-----------------READ THE COMBINED TRAUMA DATA - GEMS AND HEMS AND REMOVE COLUMNS--------------------------#
trauma=read.csv("/home/rstudio/trauma/output/trauma.csv" , 
                header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
   clean_names() %>%
  select(-iss_manual) %>%
  select(-ventilator_details_tr26_76)

#-----------------GET DISTINCT EMS SERVICE NAMES FOR EACH INCIDENT TO REMOVE NA'S -----------------------#
trauma_ems_service=trauma[, c('incident_id', 'ems_service_name_tr7_3') ] %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(ems_service_name_tr7_3)) %>%
  group_by(incident_id, ems_service_name_tr7_3)

#------------------------GET DISTINCT FACILITY NAME FOR EACH INCIDENT ----------------------------------#
trauma_facility_name=trauma[, c('incident_id', 'facility_name')]  %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(facility_name)) %>%
  group_by(incident_id, facility_name)

#-----------------SEPARATE VEHICLE ACCIDENT TO IDENTIFY VEHICLE ACCIDENT-------------------------------#
trauma_patient_motor_safety = trauma[, c("incident_id", "airbag_deployment_tr29_32", 
                                              "safety_device_used_tr29_24")] %>%
  mutate(airbag_deployment_tr29_32 = recode(airbag_deployment_tr29_32, `-Select-` = "Not Applicable", 
                                            `Not Known/Not Recorded` = "Not Applicable")) %>%
  mutate(safety_device_used_tr29_24 = recode(safety_device_used_tr29_24, None = "None", 
                                             `Not Known/Not Recorded` = "None", `-Select-` = "None")) %>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(airbag_deployment_tr29_32) | !is.na(safety_device_used_tr29_24)) %>%
  group_by(incident_id, airbag_deployment_tr29_32, safety_device_used_tr29_24)

dbSendQuery(con, "drop table trauma_patient_motor_safety")
dbWriteTable(con,c('trauma_patient_motor_safety'), value=trauma_patient_motor_safety, row.names=FALSE)

#-----------------SEPARATE ALCOHOL AND DRUG RELATED CASES----------------------------------------------#
trauma_patient_alcohol_drug = trauma[,c("incident_id", "alcohol_intervention_alcohol_screen_tr18_46",
                                        "drug_use_indicator_tr18_45")] %>%
  distinct(.keep_all = TRUE) %>%
  mutate(alcohol_intervention_alcohol_screen_tr18_46 = recode(alcohol_intervention_alcohol_screen_tr18_46, 
                                                              `No(confirmed by test)` = "No", 
                                                              `No(not tested)` = "No", `Not Applicable` = "No", 
                                                              `Not Known` = "No", 
                                                              `Not Known/Not Recorded` = "No", 
                                                              `Yes(confirmed by test [beyond legal limits])` = "Yes", 
                                                              `Yes(confirmed by test [trace levels])` = "Yes")) %>%
  mutate(drug_use_indicator_tr18_45 = recode(drug_use_indicator_tr18_45, `No(confirmed by test)` = "No", 
                                             `Not Applicable` = "No", `Not Known` = "No", 
                                             `Not Known/Not Recorded` = "No",
                                             `Not Known/Not Recorded` = "No", `Not Performed` = "No", 
                                             `Yes (confirmed by test [illegal use drug])` = "Yes", 
                                             `Yes (confirmed by test [prescription drug])` = "Yes", 
                                             `No (confirmed by test)` = "No")) %>%
  filter(!is.na(alcohol_intervention_alcohol_screen_tr18_46) | !is.na(drug_use_indicator_tr18_45)) %>%
  distinct(.keep_all = TRUE) %>%
  group_by(incident_id, alcohol_intervention_alcohol_screen_tr18_46,
           drug_use_indicator_tr18_45) 

dbSendQuery(con, "drop table trauma_patient_alcohol_drug")
dbWriteTable(con,c('trauma_patient_alcohol_drug'), value=trauma_patient_alcohol_drug, row.names=FALSE)

#-----------------------------SEPARATE MULTIPLE COMORBIDITY CONDITIONS FOR A INCIDENT ID---------------------#
trauma_comorbidity = trauma[,c("incident_id", "co_morbidity_condition_tr21_21")] %>%
    distinct(.keep_all = TRUE) %>%
    filter(!is.na(co_morbidity_condition_tr21_21))

dbSendQuery(con, "drop table trauma_comorbidity")
dbWriteTable(con,c('trauma_comorbidity'), value=trauma_comorbidity, row.names=FALSE)

#---------------------------- GET ALL ICD10 CODES FOR A CASE ----------------------------------------# 
 trauma_icd10_diagnosis = trauma %>%
    select(incident_id, icd_10_diagnosis_codes_list) %>%
    separate(icd_10_diagnosis_codes_list, 
             into = c("icd10_1", "icd10_2", "icd10_3", "icd10_4", "icd10_5", "icd10_6", "icd10_7", "icd10_8", 
                  "icd10_9", "icd10_10", "icd10_11", "icd10_12", "icd10_13", "icd10_14", "icd10_15", 
                  "icd10_16", "icd10_17", "icd10_18", "icd10_19", "icd10_20", "icd10_21", "icd10_22",
                  "icd10_23", "icd10_24", "icd10_25", "icd10_26", "icd10_30", "icd10_31", "icd10_32", 
                  "icd10_33", "icd10_34", "icd10_35", "icd10_36", "icd10_37", "icd10_38", "icd10_39", "icd10_40"), sep = "\\s*\\,\\s*", convert = TRUE) %>%
    gather(key, value, icd10_1:icd10_40, na.rm = TRUE, convert = TRUE) %>%
    select(-key)   

 dbSendQuery(con, "drop table trauma_icd10_diagnosis")
 dbWriteTable(con,c('trauma_icd10_diagnosis'), value=trauma_icd10_diagnosis, row.names=FALSE)
 
#-----------------------GET UNIQUE ICD10 DESC FOR A CASE -------------------------------------------# 
 trauma_code_desc= trauma %>%
   select(icd_10_diagnosis_detail_description_tr200_1) %>%
   filter(!is.na(icd_10_diagnosis_detail_description_tr200_1)) %>%
   mutate(desc = substr(icd_10_diagnosis_detail_description_tr200_1,
                        regexpr(") ", icd_10_diagnosis_detail_description_tr200_1) + 2, 
                        length(icd_10_diagnosis_detail_description_tr200_1))) %>%
   mutate(code=substr(icd_10_diagnosis_detail_description_tr200_1,
          2, 
          regexpr(")", icd_10_diagnosis_detail_description_tr200_1)-1))
 
 dbSendQuery(con, "drop table trauma_code_desc")
 dbWriteTable(con,c('trauma_code_desc'), value=trauma_code_desc, row.names=FALSE)
 
#----------------------------- TRAUMA PATIENT ED DETAILS -------------------------------------------#
trauma_patient_ed=trauma %>%
   select(ed_acute_care_admission_date_tr18_55, 
          ed_acute_care_admission_time_tr18_56, 
          ed_acute_care_discharge_date_tr17_25, 
          ed_acute_care_discharge_time_tr17_26, 
          ed_acute_care_disposition_tr17_27, 
          ed_death_tr27_14, 
          financial_primary_method_of_payment_tr2_5, 
          hospital_discharge_date_tr25_34, 
          hospital_discharge_disposition_tr25_27, 
          hospital_discharge_orders_written_date_tr25_93, 
          icu_days_total_tr26_9, 
          incident_id, 
          injury_county_tr5_9, 
          injury_state_tr5_7, 
          injury_supplemental_cause_tr5_8, 
          injury_zip_tr5_6, 
          inter_facility_transfer_tr25_54, 
          iss_calculated_tr21_8, 
          location, patient_age_tr1_12, patient_age_units_tr1_14, patient_ethnicity_tr1_17, 
          patient_gender_tr1_15, transport_to_your_facility_by_tr8_8) %>%
   distinct(.keep_all = TRUE)

trauma_patient_ed = left_join(trauma_patient_ed, trauma_ems_service, by="incident_id")

trauma_patient_ed = left_join(trauma_patient_ed, trauma_facility_name, by="incident_id")

dbSendQuery(con, "drop table trauma_patient_ed")
dbWriteTable(con,c('trauma_patient_ed'), value=trauma_patient_ed, row.names=FALSE)

#---------------------TRAUMA INCIDENT TIMES - REMOVE NULL DATES IN DUPLICATE INCIDENTS -------------# 
trauma_incident_unique=trauma %>%
  select(ems_unit_arrived_on_scene_tr9_2, 
         ems_unit_left_scene_tr9_3, 
         ems_unit_notified_date_tr9_1, 
         ems_unit_notified_time_tr9_10, 
         incident_date_tr5_1, 
         incident_id, 
         incident_time_tr5_18) %>%
  distinct(.keep_all = TRUE) 
  
trauma_incident_dup=trauma %>%
  select(ems_unit_arrived_on_scene_tr9_2, 
         ems_unit_left_scene_tr9_3, 
         ems_unit_notified_date_tr9_1, 
         ems_unit_notified_time_tr9_10, 
         incident_date_tr5_1, 
         incident_id, 
         incident_time_tr5_18) %>%
  distinct(.keep_all = TRUE) %>%
  get_dupes(incident_id) %>%
  group_by(incident_id) %>%
  filter(!is.na(ems_unit_notified_date_tr9_1))
trauma_incident_dup=trauma_incident_dup[,!names(trauma_incident_dup) %in% c("dupe_count")]
trauma_incident_dup=trauma_incident_dup[,order(names(trauma_incident_dup))]

trauma_incident_unique_1= trauma_incident_unique[!trauma_incident_unique$incident_id %in% trauma_incident_dup$incident_id,]
trauma_incident_unique_1=trauma_incident_unique_1[,order(names(trauma_incident_unique_1))]

trauma_incident = rbind(trauma_incident_unique_1, as.data.frame(trauma_incident_dup)) 

dbSendQuery(con, "drop table trauma_incident")
dbWriteTable(con,c('trauma_incident'), value=trauma_incident, row.names=FALSE)

#-----------------PREHOSPITAL --------------------------------#
trauma_prehospital_unique=trauma%>%
  select(incident_id, prehospital_gcs_total_manual_tr18_64, 
         prehospital_pulse_oximetry_tr18_82, 
         prehospital_pulse_rate_tr18_69, 
         prehospital_respiratory_rate_tr18_70, 
         prehospital_sbp_tr18_67) %>%
  distinct(.keep_all = TRUE)

trauma_prehospital_dup=trauma%>%
  select(incident_id, prehospital_gcs_total_manual_tr18_64, 
         prehospital_pulse_oximetry_tr18_82, 
         prehospital_pulse_rate_tr18_69, 
         prehospital_respiratory_rate_tr18_70, 
         prehospital_sbp_tr18_67) %>%
  distinct(.keep_all = TRUE) %>%
  get_dupes(incident_id) %>%
  group_by(incident_id) %>%
  summarize(prehospital_gcs_total_manual_mean = round(mean(prehospital_gcs_total_manual_tr18_64, na.rm = TRUE)), 
            prehospital_pulse_oximetry_mean = round(mean(prehospital_pulse_oximetry_tr18_82, na.rm = TRUE)), 
            prehospital_pulse_rate_mean = round(mean(prehospital_pulse_rate_tr18_69, na.rm = TRUE)), 
            prehospital_respiratory_rate_mean = round(mean(prehospital_respiratory_rate_tr18_70, na.rm = TRUE)), 
            prehospital_sbp_mean = round(mean(prehospital_sbp_tr18_67, na.rm = TRUE)))

trauma_prehospital_unique_1= trauma_prehospital_unique[!trauma_prehospital_unique$incident_id %in% trauma_prehospital_dup$incident_id,]

trauma_prehospital = rbind(trauma_prehospital_unique_1, 
                           setNames(as.data.frame(trauma_prehospital_dup), names(trauma_prehospital_unique_1))) 

trauma_prehospital = left_join(trauma_prehospital, trauma_patient_alcohol_drug, by='incident_id')

dbSendQuery(con, "drop table trauma_prehospital")
dbWriteTable(con,c('trauma_prehospital'), value=trauma_prehospital, row.names=FALSE)

#---------------------INITIAL ASSESSMENT ---------------------------------------#
initial_assessment= trauma %>%
  select(incident_id,  initial_assessment_respiratory_assistance)%>%
  distinct(.keep_all = TRUE) %>%
  filter(!is.na(initial_assessment_respiratory_assistance))
  group_by(incident_id) 
  
dbSendQuery(con, "drop table initial_respiratory")
dbWriteTable(con,c('initial_respiratory'), value=initial_assessment, row.names=FALSE)


