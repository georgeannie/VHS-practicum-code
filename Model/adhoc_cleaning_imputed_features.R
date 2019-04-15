#-----------------------------------------------------------------------
#       ADHOC CHANGE TO FIX PREHOSPITAL FEATURES IN IMPUTED SET
#-----------------------------------------------------------------------

library('RPostgreSQL')
library(caret)
library(DMwR)
library(dplyr)
set.seed(9560)


source("C:/Users/User/Documents/VHS-practicum-code/function_aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#----------------------READ ALL ROWS FROM CLEAN_TRAUMA TABLE --------------#
imp_set1=dbGetQuery(con, 'select * from imputed_feature1')
imp_set2=dbGetQuery(con, 'select * from imputed_feature2')

imp_set2 = imp_set2 %>%
  select(-pulseinrange, -respinrange, -oximetryinrange, -sbpinrange,
         diff_from_normal_sbp, diff_from_normal_resp, diff_from_normal_pulse, diff_in_oximetry)

conversion = imp_set2
conversion$years=as.integer(conversion$years)
#---------------------------------------------------------------------------#
#Indication of systolic blood pressure in range based off of age.           #
# 1 meaning within range and 0 out of range.                                #
#---------------------------------------------------------------------------#
conversion$sbpinrange<-ifelse(conversion$years <1, 
                              (ifelse(conversion$pre_sbp>71 & conversion$pre_sbp<105,1,0)),
                              ifelse(conversion$years>=1 & conversion$years<=2,
                                     (ifelse(conversion$pre_sbp>85 & conversion$pre_sbp<107,1,0)),
                                     ifelse(conversion$years>=3 & conversion$years<=5,
                                            (ifelse(conversion$pre_sbp>88 & conversion$pre_sbp<113,1,0)),
                                            ifelse(conversion$years>=6 & conversion$years<=9,       
                                                   (ifelse(conversion$pre_sbp>96 & conversion$pre_sbp<116,1,0)),
                                                   ifelse(conversion$years>=10 & conversion$years<=11, 
                                                          (ifelse(conversion$pre_sbp>101 & conversion$pre_sbp<121,1,0)),
                                                          ifelse(conversion$years>=12 & conversion$years<=15, 
                                                                 (ifelse(conversion$pre_sbp>109 & conversion$pre_sbp<132,1,0)),
                                                                 ifelse(conversion$years>15,
                                                                        (ifelse(conversion$pre_sbp>89 & conversion$pre_sbp<121,1,0)),
                                                                        0
                                                                 )))))))            

conversion$sbpinrange[is.na(conversion$sbpinrange)] = '1'

#---------------------------------------------------------------------------#
# Indication of respiratory rate within range based off of age.  1 meaning  #
# within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#
conversion = conversion %>%
  mutate(respinrange = case_when( 
    (years <1  & (pre_resp> 29 & pre_resp <54)) ~ 1,
    ((years >=1 & years < 3) & (pre_resp> 21 & pre_resp<38)) ~ 1,
     ((years >=3 & years < 6) & (pre_resp> 19 & pre_resp<29)) ~ 1,
      ((years >=6 & years < 12) & (pre_resp> 17 & pre_resp<26)) ~ 1,
       ((years >=12 & years < 16) & (pre_resp> 11 & pre_resp<21)) ~ 1,
        ((years >=16) & (pre_resp> 11 & pre_resp<19)) ~ 1,
         (is.na(pre_resp)) ~ 1,
         TRUE ~ 0)
  )
    
#---------------------------------------------------------------------------#
# Indication of pulse rate within range based off of age.  1 meaning        #
# within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(pulseinrange = case_when( 
    (age <28 & patient_age_units_tr1_14 == 'Days') &
      (pre_pulse> 99 & pre_pulse<206) ~ 1,
    ((age >=28 & patient_age_units_tr1_14 == 'Days') |
       (age >=1 & age <=12 & patient_age_units_tr1_14 == 'Months')) &
      (pre_pulse> 99 & pre_pulse < 191) ~ 1,
    ((age >12 & age <= 24 & patient_age_units_tr1_14 == 'Months') |
       (age >=1 & age <= 2 & patient_age_units_tr1_14 == 'Years')) &
      (pre_pulse> 97 & pre_pulse<141) ~ 1,
    (age >=3 & age <= 5 & patient_age_units_tr1_14 == 'Years') &
      (pre_pulse> 79 & pre_pulse<121) ~ 1,
    (age >=6 & age <= 11 & patient_age_units_tr1_14 == 'Years') &
      (pre_pulse> 74 & pre_pulse<119) ~ 1,
    (age >=12 & patient_age_units_tr1_14 == 'Years') &
      (pre_pulse> 59 & pre_pulse<101) ~ 1,
    (is.na(pre_pulse)) ~ 1,
    TRUE ~ 0)
  )

#---------------------------------------------------------------------------#
# Indication of oximetry.  1 meanin within range and 0 out of range.                                          #
#---------------------------------------------------------------------------#

conversion$oximetryinrange = ifelse(conversion$pre_oxi>94 & 
                                      conversion$pre_oxi<101,1,0)

conversion$oximetryinrange[is.na(conversion$oximetryinrange)] = '1'

#---------------------------------------------------------------------------#
# Find difference from normal in sbp for all ages                           #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_sbp = 
           case_when(pre_sbp == 0 ~ 0,
                     (years < 1 & pre_sbp < 72) ~   (pre_sbp - 72),
                     (years < 1 & pre_sbp > 104) ~  (pre_sbp - 104),
                     (years >=1 & years < 3) & 
                       (pre_sbp < 86) ~  (pre_sbp - 86) ,
                     (years >=1 & years < 3) &
                       (pre_sbp > 106) ~  (pre_sbp - 106) ,
                     (years >=3 & years < 6) &
                       (pre_sbp < 89) ~  (pre_sbp - 89),
                     (years >=3 & years < 6) &
                       (pre_sbp > 112) ~  (pre_sbp - 112),
                     (years >=6 & years < 10) &
                       (pre_sbp < 97) ~  (pre_sbp - 97),
                     (years >=6 & years < 10) &
                       (pre_sbp > 115) ~  (pre_sbp - 115),
                     (years >=10 & years < 12) &
                       (pre_sbp < 101) ~  (pre_sbp - 101),
                     (years >=10 & years < 12) &
                       (pre_sbp > 120) ~  (pre_sbp - 120),
                     (years >=12& years < 16) &
                       (pre_sbp < 110) ~  (pre_sbp - 110),
                     (years >=12 & years < 16) &
                       (pre_sbp > 131) ~  (pre_sbp - 131),
                     (years >=16) &
                       (pre_sbp < 90) ~  (pre_sbp - 90),
                     (years >=16) &
                       (pre_sbp > 120) ~  (pre_sbp - 120),
                     TRUE ~ 0
           ))

#---------------------------------------------------------------------------#
# Find difference from normal in respiratory for all ages                   #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_resp = 
           case_when(pre_resp == 0 ~ 0,
                     (years < 1 & pre_resp < 30) ~ (pre_resp - 30),
                     (years < 1 & pre_resp > 53) ~ (pre_resp - 53),
                     (years >=1 & years < 3) & 
                       (pre_resp < 22) ~  (pre_resp - 22) ,
                     (years >=1 & years < 3) &
                       (pre_resp > 37) ~  (pre_resp - 37) ,
                     (years >=3 & years < 6) &
                       (pre_resp < 20) ~  (pre_resp - 20),
                     (years >=3 & years < 6) &
                       (pre_resp > 28) ~  (pre_resp - 28),
                     (years >=6 & years < 12) &
                       (pre_resp < 18) ~  (pre_resp - 18),
                     (years >=6 & years < 12) &
                       (pre_resp > 25) ~  (pre_resp - 25),
                     (years >=12 & years < 16) &
                       (pre_resp < 12) ~  (pre_resp - 12),
                     (years >=12 & years < 16) &
                       (pre_resp > 20) ~  (pre_resp - 20),
                     (years >=16) &
                       (pre_resp < 12) ~  (pre_resp - 12),
                     (years >=16) &
                       (pre_resp > 18) ~  (pre_resp - 18),
                     TRUE ~ 0
           ))


#---------------------------------------------------------------------------#
# Find the difference from normal of pulse                                  #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_from_normal_pulse = case_when( 
    (age <28 & patient_age_units_tr1_14 == 'Days') &
      pre_pulse < 100        ~ (pre_pulse - 100),
    
    (age <28 & patient_age_units_tr1_14 == 'Days') &
      pre_pulse > 205         ~ (pre_pulse - 205),
    
    ((age >=28 & patient_age_units_tr1_14 == 'Days') |
       (age >=1 & age <=12 & patient_age_units_tr1_14 == 'Months')) &
      pre_pulse < 100        ~ (pre_pulse - 100),
    
    ((age >=28 & patient_age_units_tr1_14 == 'Days') |
       (age >=1 & age <=12 & patient_age_units_tr1_14 == 'Months')) &
      pre_pulse > 190      ~ (pre_pulse - 190),
    
    ((age >12 & age <= 24 & patient_age_units_tr1_14 == 'Months') |
       (age >=1 & age <= 2 & patient_age_units_tr1_14 == 'Years')) &
      pre_pulse < 98         ~ (pre_pulse - 98),
    
    ((age >12 & age <= 24 & patient_age_units_tr1_14 == 'Months') |
       (age >=1 & age <= 2 & patient_age_units_tr1_14 == 'Years')) &
      pre_pulse >140         ~ (pre_pulse - 140),
    
    (age >=3 & age <= 5 & patient_age_units_tr1_14 == 'Years') &
      pre_pulse < 80         ~ (pre_pulse - 80),
    
    (age >=3 & age <= 5 & patient_age_units_tr1_14 == 'Years') &
      pre_pulse > 120        ~ (pre_pulse - 120),
    
    (age >=6 & age <= 11 & patient_age_units_tr1_14 == 'Years') &
      pre_pulse < 75          ~ (pre_pulse - 75),  
    
    (age >=6 & age <= 11 & patient_age_units_tr1_14 == 'Years') &   
      pre_pulse > 118        ~ (pre_pulse - 118),
    
    (age >=12 & patient_age_units_tr1_14 == 'Years') &
      pre_pulse < 60           ~ (pre_pulse - 60),
    
    (age >=12 & patient_age_units_tr1_14 == 'Years') &   
      pre_pulse > 100          ~ (pre_pulse - 100),
    
    (is.na(pre_pulse)) ~ 0,
    TRUE ~ 0)
  )

#---------------------------------------------------------------------------#
# Find the difference from pulse oximetry                                   #
#---------------------------------------------------------------------------#

conversion = conversion %>%
  mutate(diff_in_oximetry= case_when(
    pre_oxi == 0 ~  0,
    pre_oxi < 95 ~  pre_oxi - 95,
    pre_oxi > 100 ~  pre_oxi - 100,
    TRUE ~ 0)
  )

#-------- MOVE ALL FEATURES BACK INTO THE ORIGINAL DATAFRAME --------------#
df5 = conversion       #----> CHANGE BACK TO ORIGINAL DATAFRAME PRIOR TO LOAD


#--------------LOAD TWO BEST IMPUTED DATAFRAME -----------------------------#
# RUN ABOVE STEPS TWICE TO CREATE NEW FEATURES PRIOR TO LOADING             #
#---------------------------------------------------------------------------#
library('RPostgreSQL')
source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

# SET 1
dbSendQuery(con, "drop table imputed_feature1")

dbWriteTable(con,c('imputed_feature1'), value=df5, row.names=FALSE)

#SET 2
dbSendQuery(con, "drop table imputed_feature2")

dbWriteTable(con,c('imputed_feature2'), value=df5, row.names=FALSE)
