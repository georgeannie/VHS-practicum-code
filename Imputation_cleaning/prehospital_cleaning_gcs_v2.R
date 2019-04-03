#--------------------------------------------------------------------------------------#
#                   CLEANING GCS SCORE IN TRAUMA PREHOSPITAL DATASET                   #
#     1. EXTRACT THE SCORE FROM THE GCS DESCRIPTIONS                                   #

#--------------------------------------------------------------------------------------#
library(dplyr)
library(tidyr)
library(rlist)
library(purr)
library('RPostgreSQL')

source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#----------------------READ ALL ROWS FROM CLEAN_TRAUMA TABLE --------------#
clean_trauma=dbGetQuery(con, 'select * from clean_trauma')
trauma_prehospital=dbGetQuery(con, 'select * from trauma_prehospital')

#---------------DISCONNECT POSTGRES ------#
dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)


library(sqldf)
clean_trauma1=clean_trauma

#---------------------------------------------------------------------------#
#  EXTRACT THE GCS SCORE FROM PREHOSPITAL GCS INFORMATION                   #
#---------------------------------------------------------------------------#
trauma_prehospital1 = trauma_prehospital

trauma_prehospital1 = trauma_prehospital1 %>%
  separate(prehospital_gcs_eye, 
           into = c("prehospital_gcs_eye_num"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop") %>%
  separate(prehospital_gcs_verbal, 
           into = c("prehospital_gcs_verbal_num"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop") %>%
  separate(prehospital_gcs_motor, 
           into = c("prehospital_gcs_motor_num"), sep = "\\s+", 
           remove = FALSE, convert = FALSE, extra = "drop")

trauma_prehospital1$prehospital_gcs_total_manual_tr18_64_imp = 
  trauma_prehospital1$prehospital_gcs_total_manual_tr18_64
#---------------------------------------------------------------------------#
#  SET THE SCORE AS NA IF IT IS 'NOT' OR IF IT IS MISSING                 #
#---------------------------------------------------------------------------#

trauma_prehospital1$prehospital_gcs_eye_num[is.na(trauma_prehospital1$prehospital_gcs_eye_num) |
                                              trauma_prehospital1$prehospital_gcs_eye_num == 'Not'] = NA
trauma_prehospital1$prehospital_gcs_verbal_num[is.na(trauma_prehospital1$prehospital_gcs_verbal_num) |
                                                 trauma_prehospital1$prehospital_gcs_verbal_num == 'Not'] = NA
trauma_prehospital1$prehospital_gcs_motor_num[is.na(trauma_prehospital1$prehospital_gcs_motor_num) |
                                                trauma_prehospital1$prehospital_gcs_motor_num == 'Not'] = NA

#---------------------------------------------------------------------------#
#  CONVERT THE SCORES TO NUMERIC                                            #
#---------------------------------------------------------------------------#

trauma_prehospital1$prehospital_gcs_eye_num = as.numeric(trauma_prehospital1$prehospital_gcs_eye_num)
trauma_prehospital1$prehospital_gcs_verbal_num = as.numeric(trauma_prehospital1$prehospital_gcs_verbal_num)
trauma_prehospital1$prehospital_gcs_motor_num = as.numeric(trauma_prehospital1$prehospital_gcs_motor_num)

#---------------------------------------------------------------------------#
#  SET A FLAG IF SUM OF EYE, VERBAL AND MOTOR DO NOT MATCH THE TOTAL        #
#---------------------------------------------------------------------------#

trauma_prehospital1$gcs_total_match_flag = trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 
  (trauma_prehospital1$prehospital_gcs_eye_num + 
     trauma_prehospital1$prehospital_gcs_motor_num +
     trauma_prehospital1$prehospital_gcs_verbal_num)

# MERGE WITH TRAUMA DATA
clean_trauma1 = clean_trauma1 %>% 
  merge(trauma_prehospital1, by=c("incident_id"))

# MISSING 21661 GCS EYE
# MISSING 23907 GCS VERBAL
# MISSING 23913 GCS MOTOR

sum(is.na(clean_trauma1$prehospital_gcs_eye_num))
sum(is.na(clean_trauma1$prehospital_gcs_verbal_num))
sum(is.na(clean_trauma1$prehospital_gcs_motor_num))


# GET THE MODE OF GCS EYE FOR EACH ED DISPOSITION TO FILL NA IN GCS EYE
clean_trauma1=unique_entry(df=clean_trauma1, 
                           var1=prehospital_gcs_eye_num,
                           distinct_var2 = ed_acute_care_disposition_tr17_27, 
                           key_var =incident_id)
sum(is.na(clean_trauma1$prehospital_gcs_eye_num))

# GET THE MODE OF GCS VERBAL FOR EACH ED DISPOSITION TO FILL NA IN GCS EYE
clean_trauma1=unique_entry(df=clean_trauma1, 
                           var1=prehospital_gcs_verbal_num,
                           distinct_var2 = ed_acute_care_disposition_tr17_27, 
                           key_var =incident_id)
sum(is.na(clean_trauma1$prehospital_gcs_verbal_num))

# GET THE MODE OF GCS MOTOR FOR EACH ED DISPOSITION TO FILL NA IN GCS EYE
clean_trauma1=unique_entry(df=clean_trauma1, 
                           var1=prehospital_gcs_motor_num,
                           distinct_var2 = ed_acute_care_disposition_tr17_27, 
                           key_var =incident_id)
sum(is.na(clean_trauma1$prehospital_gcs_motor_num))

#---------------------------------------------------------------------------#
#  SUM THE EYE, VERBAL, MOTOR AND REPLACE TOTAL IF THERE IS A MISMATCH IN   #
#  TOTAL AND THE SUM OF INDIVIDUAL SCORES                                   #
#---------------------------------------------------------------------------#

filter1 = clean_trauma1 %>%
  filter(prehospital_gcs_eye_num > 0 &
           prehospital_gcs_motor_num > 0 &
           prehospital_gcs_verbal_num > 0 &
           is.na(gcs_total_match_flag)) %>%
  mutate(prehospital_gcs_total_manual_tr18_64_imp = prehospital_gcs_eye_num + 
           prehospital_gcs_motor_num  +
           prehospital_gcs_verbal_num)

clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% filter1$incident_id) %>%
  rbind(filter1)

#---------------------------------------------------------------------------#
#  RESET A FLAG IF SUM OF EYE, VERBAL AND MOTOR DO NOT MATCH THE TOTAL        #
#---------------------------------------------------------------------------#

clean_trauma1$gcs_total_match_flag = clean_trauma1$prehospital_gcs_total_manual_tr18_64 == 
  (clean_trauma1$prehospital_gcs_eye_num + 
     clean_trauma1$prehospital_gcs_motor_num +
     clean_trauma1$prehospital_gcs_verbal_num)



library(RPostgreSQL)
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

dbSendQuery(con, "drop table clean_trauma")
dbWriteTable(con,c('clean_trauma'), value=clean_trauma1, row.names=FALSE)

dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)
