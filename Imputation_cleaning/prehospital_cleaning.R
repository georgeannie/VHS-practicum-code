#--------------------------------------------------------------------------------------#
#                   CLEANING GCS SCORE IN TRAUMA PREHOSPITAL DATASET                   #
#     1. EXTRACT THE SCORE FROM THE GCS DESCRIPTIONS                                   #

#--------------------------------------------------------------------------------------#
library(geosphere)
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

#---------------------------------------------------------------------------#
#  SET THE SCORE AS ZERO IF IT IS 'NOT' OR IF IT IS MISSING                 #
#---------------------------------------------------------------------------#

trauma_prehospital1$prehospital_gcs_eye_num[is.na(trauma_prehospital1$prehospital_gcs_eye_num) |
                                              trauma_prehospital1$prehospital_gcs_eye_num == 'Not'] = 0
trauma_prehospital1$prehospital_gcs_verbal_num[is.na(trauma_prehospital1$prehospital_gcs_verbal_num) |
                                                 trauma_prehospital1$prehospital_gcs_verbal_num == 'Not'] = 0
trauma_prehospital1$prehospital_gcs_motor_num[is.na(trauma_prehospital1$prehospital_gcs_motor_num) |
                                                trauma_prehospital1$prehospital_gcs_motor_num == 'Not'] = 0

#---------------------------------------------------------------------------#
#  CONVERT THE SCORES TO NUMERIC                                            #
#---------------------------------------------------------------------------#

trauma_prehospital1$prehospital_gcs_eye_num = as.numeric(trauma_prehospital1$prehospital_gcs_eye_num)
trauma_prehospital1$prehospital_gcs_verbal_num = as.numeric(trauma_prehospital1$prehospital_gcs_verbal_num)
trauma_prehospital1$prehospital_gcs_motor_num = as.numeric(trauma_prehospital1$prehospital_gcs_motor_num)

#---------------------------------------------------------------------------#
#  IF TOTAL SCORE IS MISSING, ADD THE VERBAL, EYE AND MOTOR SCORE           #
#---------------------------------------------------------------------------#

trauma_prehospital1 = trauma_prehospital1 %>%
  mutate(prehospital_gcs_total_manual_tr18_64 = coalesce(prehospital_gcs_total_manual_tr18_64,
                                                         (prehospital_gcs_motor_num + prehospital_gcs_eye_num +  prehospital_gcs_verbal_num)))

#---------------------------------------------------------------------------#
#  DISTRIBUTE THE HIGHEST SCORE OF 15 AS 5 IN EACH OF EYE, VERBAL AND MOTOR #
#  DISTRIBUTE THE LOWEST SCORE OF 3 AS 1 IN EACH OF EYE, VERBAL AND MOTOR   #
#---------------------------------------------------------------------------#

trauma_prehospital1$prehospital_gcs_eye_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 15 &
                                              trauma_prehospital1$prehospital_gcs_eye_num == 0] = 5
trauma_prehospital1$prehospital_gcs_motor_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 15 &
                                                trauma_prehospital1$prehospital_gcs_motor_num == 0] = 5
trauma_prehospital1$prehospital_gcs_verbal_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 15 &
                                                 trauma_prehospital1$prehospital_gcs_verbal_num == 0] = 5

trauma_prehospital1$prehospital_gcs_eye_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 3 &
                                              trauma_prehospital1$prehospital_gcs_eye_num == 0] = 1
trauma_prehospital1$prehospital_gcs_motor_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 3 &
                                                trauma_prehospital1$prehospital_gcs_motor_num == 0] = 1
trauma_prehospital1$prehospital_gcs_verbal_num[trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 3 &
                                                 trauma_prehospital1$prehospital_gcs_verbal_num == 0] = 1


#---------------------------------------------------------------------------#
#  SET A FLAG IF SUM OF EYE, VERBAL AND MOTOR DO NOT MATCH THE TOTAL        #
#---------------------------------------------------------------------------#

trauma_prehospital1$gcs_total_match_flag = trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 
  (trauma_prehospital1$prehospital_gcs_eye_num + 
     trauma_prehospital1$prehospital_gcs_motor_num +
     trauma_prehospital1$prehospital_gcs_verbal_num)

#---------------------------------------------------------------------------#
#  SUM THE EYE, VERBAL, MOTOR AND REPLACE TOTAL IF THERE IS A MISMATCH IN   #
#  TOTAL AND THE SUM OF INDIVIDUAL SCORES                                   #
#---------------------------------------------------------------------------#

filter1 = trauma_prehospital1 %>%
     filter(prehospital_gcs_eye_num > 0 &
              prehospital_gcs_motor_num > 0 &
              prehospital_gcs_verbal_num > 0 &
              gcs_total_match_flag == FALSE) %>%
     mutate(prehospital_gcs_total_manual_tr18_64 = prehospital_gcs_eye_num + 
                                       prehospital_gcs_motor_num  +
                                       prehospital_gcs_verbal_num)

trauma_prehospital1 = trauma_prehospital1 %>%
  filter(!incident_id %in% filter1$incident_id) %>%
  rbind(filter1)

#---------------------------------------------------------------------------#
#                         RESET THE TOTAL MATCH FLAG                        #
#---------------------------------------------------------------------------#

trauma_prehospital1$gcs_total_match_flag = trauma_prehospital1$prehospital_gcs_total_manual_tr18_64 == 
  (trauma_prehospital1$prehospital_gcs_eye_num + 
     trauma_prehospital1$prehospital_gcs_motor_num +
     trauma_prehospital1$prehospital_gcs_verbal_num)

clean_trauma1 = clean_trauma1 %>% 
  merge(trauma_prehospital1, by=c("incident_id"))

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
