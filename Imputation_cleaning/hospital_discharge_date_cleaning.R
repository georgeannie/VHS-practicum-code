#######################################################################################
#       CLEAN UP HOSPITAL DISCHARGE DATE. USE THE HOSPITAL DISCHARGE DATE AND         #
#       ED ADMISSION DATE TO FIND THE NUMBER OF DAYS IN THE HOSPITAL.                 #
#       IF HOSPITAL DISCHARGE DISPOSITION IS DECEASED THEN REPLACE ED DISPOSITION WITH#
#       DECEASED. USE THE NUMBER OF DAYS, ED DISPOSITION AND TIME(MAYBE) TO DETERMINE #
#       OUTCOME - NEED A HELICOPTER?                                                  #
#######################################################################################
# Load required packages.
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
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


clean_trauma=dbGetQuery(con, 'select * from clean_trauma')

clean_trauma1=clean_trauma

#------------------------------------------------------------------------#
# CONVERT DISCHARGE DATE, ADMISSION DATE, INCIDENT DATE TO MDY FORMAT    #
# POPULATE HOSPITAL DISCHARGE DATE WHICH IS NULL USING HOSPITAL DISCHARGE#
# ORDER WRITTEN                                                          #  
#------------------------------------------------------------------------#

clean_trauma1 = clean_trauma1 %>%
  mutate(hospital_discharge_date_tr25_34 = mdy(hospital_discharge_date_tr25_34), 
      ed_acute_care_discharge_date_tr17_25 = mdy(ed_acute_care_discharge_date_tr17_25), 
      ed_acute_care_admission_date_tr18_55 = mdy(ed_acute_care_admission_date_tr18_55), 
      incident_date_tr5_1 = mdy(incident_date_tr5_1), 
      hospital_discharge_orders_written_date_tr25_93 = 
        mdy(hospital_discharge_orders_written_date_tr25_93)) 

#------------------------------------------------------------------------#
# REPLACE ALL MISSING HOSPITAL DISCHARGE DATE WITH HOSPITAL DISCHARGE    #
# ORDERS WRITTEN DATE IF AVAILABLE AND ENSURE THAT IT IS AVAILABLE IN AS #
# CLEAN_TRAUM                                                            #
#------------------------------------------------------------------------#

filter_discharge_date = clean_trauma1 %>%
  filter(is.na(hospital_discharge_date_tr25_34)) %>%
  mutate(hospital_discharge_date_tr25_34= 
          coalesce(hospital_discharge_date_tr25_34, hospital_discharge_orders_written_date_tr25_93)) 

clean_trauma1= clean_trauma1 %>%
  filter(!incident_id %in% filter_discharge_date$incident_id) %>%
  rbind(filter_discharge_date)
  
#------------------------------------------------------------------------#
#9695 RECORDS NEEDS TO BE FILLED WITH DISCHARGE DATE TO GET THE NUMBER OF#
# DAYS IN THE HOSPITAL AND IT SHOULD BE ON OR AFTER THE ED ADMISSION DATE#
#------------------------------------------------------------------------#

no_discharge= clean_trauma1 %>%
  filter(is.na(hospital_discharge_date_tr25_34))

#------------------------------------------------------------------------#
#47 OF THESE CASES HAVE ADMISSION DATE BEFORE THE INCIDENT DATE AND SINCE#
# MAJORITY OF THESE CASES ARE TRANSFERS, THE DISCHARGE DATE AND ADMISSION#
# DATE WILL BE SET AS THE INCIDENT DATE                                  #
#------------------------------------------------------------------------#

filter0= no_discharge %>%
  filter(ed_acute_care_admission_date_tr18_55 < incident_date_tr5_1)  %>%
  mutate(hospital_discharge_date_tr25_34 = incident_date_tr5_1,
         ed_acute_care_admission_date_tr18_55 = incident_date_tr5_1)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter0$incident_id,]

#------------------------------------------------------------------------#
# 511 OF THESE CASES HAVE ED DISPOSITION OF DECEASED. HENCE SET DISCHARGE#
# DATE AS ADMISSION DATE                                                 #
#------------------------------------------------------------------------#

filter1 = no_discharge%>%
  filter(ed_acute_care_disposition_tr17_27 == "Deceased/Expired" ) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter1$incident_id,]

#------------------------------------------------------------------------#
#4771 OF THE REAMINING CASES WERE TRANSFERED TO ANOTHER HOSPITAL WITH ED #
# DISCHARGE DATE OCCURING ON SAME OR AFTER DAY OF ADMISSION OR ON THE    #
# SAME DAY AS INCIDENT                                                   #
#------------------------------------------------------------------------#

filter2 = no_discharge%>%
  filter(ed_acute_care_disposition_tr17_27 == "Transferred to another hospital" &
          ed_acute_care_discharge_date_tr17_25 >= ed_acute_care_admission_date_tr18_55 &
           ed_acute_care_admission_date_tr18_55 >= incident_date_tr5_1 ) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_discharge_date_tr17_25)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter2$incident_id,]

#------------------------------------------------------------------------#
#2098 OF THE REAMINING CASES WERE TRANSFERED TO ANOTHER HOSPITAL WITH NO #
# ED DISCHARGE BUT THE ADMISSION DATE OCCURING ON SAME OR AFTER          #
# INCIDENT DATE. SET DISCHARGE DATE AS ADMISSION DATE                    #
#------------------------------------------------------------------------#

filter3 = no_discharge%>%
  filter(ed_acute_care_disposition_tr17_27 == "Transferred to another hospital" &
           is.na(ed_acute_care_discharge_date_tr17_25) &
           ed_acute_care_admission_date_tr18_55 >= incident_date_tr5_1 ) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter3$incident_id,]

#------------------------------------------------------------------------#
#31 OF THE REAMINING CASES WERE DECEASED AND HAD ICU DAYS GREATER THAN   #
# EQUAL TO ZERO AND ALSO SATISFIED ADMISSION DATE ON OR AFTER INCIDENT   #
# SET DISCHARGE DATE AS ADMISSION PLUS ICU DAYS                          #
#------------------------------------------------------------------------#

filter4 = no_discharge%>%
  filter(icu_days_total_tr26_9 >= 0 &
          hospital_discharge_disposition_tr25_27 == "Deceased/Expired"  &
          ed_acute_care_admission_date_tr18_55 >= incident_date_tr5_1 ) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55 + 
           as.integer(icu_days_total_tr26_9))

no_discharge=no_discharge[!no_discharge$incident_id %in% filter4$incident_id,]

#------------------------------------------------------------------------#
#15 OF THE REAMINING CASES WERE DECEASED HAD ADMISSION DATE ON OR AFTER  #
# THE INCIDENT DATE. SET DISCHARGE DATE AS ADMISSION                     #
#------------------------------------------------------------------------#

filter5 = no_discharge%>%
  filter(hospital_discharge_disposition_tr25_27 == "Deceased/Expired"  &
         ed_acute_care_admission_date_tr18_55 >= incident_date_tr5_1 ) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter5$incident_id,]

#------------------------------------------------------------------------#
#272 OF THE REAMINING CASES HAVE SOME ICU DAYS AND ADMISSION DATE ON OR  #
# AFTER THE INCIDENT DATE. SET DISCHARGE DATE AS ADMISSION DATE + ICU DAY#
#------------------------------------------------------------------------#

filter6 = no_discharge%>%
  filter(!is.na(icu_days_total_tr26_9)) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55 + icu_days_total_tr26_9)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter6$incident_id,]

#------------------------------------------------------------------------#
#246 OF THE REAMINING CASES HAVE ED ACUTE DISCHARGE DATE ON OR AFTER THE #
# ADMISSION DATE. SET DISCHARGE DATE AS ED DISCHARGE DATE                #
#------------------------------------------------------------------------#
filter7 = no_discharge%>%
  filter(ed_acute_care_discharge_date_tr17_25 >= ed_acute_care_admission_date_tr18_55) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_discharge_date_tr17_25)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter7$incident_id,]

#------------------------------------------------------------------------#
#1704 OF THE REAMINING CASES ARE MAJORITY WITH DISPOSITION HOME WITHOUT  #
# SERVICES AND AMA AND OTHER. VERY FEW ARE OPERATING ROOM. HENCE SET     #
# DISCHARGE DATE AS ADMISSION DATE                                       #
#------------------------------------------------------------------------#

filter8 =no_discharge%>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter8$incident_id,]

#------------------------------------------------------------------------#
#COMBINE ALL FILTERS AND REPLACE THE ROWS IN CLEAN_TRAUMA                #
#------------------------------------------------------------------------#

clean_discharge_date = filter0 %>%
  bind_rows(filter1, filter2, filter3, filter4, filter5, filter6, filter7, filter8)

clean_trauma1 = clean_trauma1 %>%
      filter(!incident_id %in% clean_discharge_date$incident_id) %>%
      rbind(clean_discharge_date)


#PART II: CLEANING HOSPITAL DISCHARGE DATES TO AVOID NEGATIVE DAYS IN HOSPITAL
#------------------------------------------------------------------------#
#28 OBSERVATIONS HAVE DISCHARGE DATE LESS THAN ADMISSION DATE. NEEDS TO  #
#BE ADDRESSED TO AVOID NEGATIVE DAYS                                     #
#------------------------------------------------------------------------#
# SOLUTION1: USE YEAR OF ED ADMISSION DATE IN THE DISCHARGE DATE FOR     #
# THE DAY AND MONTH FOR THESE RECORDS IS REALISTIC                       #
#------------------------------------------------------------------------#

incorrect_discharge_date = clean_trauma1 %>%
  filter(hospital_discharge_date_tr25_34 < ed_acute_care_admission_date_tr18_55) %>%
  separate(hospital_discharge_date_tr25_34, 
           into = c("hospital_discharge_date_tr25_34_year",
                    "hospital_discharge_date_tr25_34_month", 
                    "hospital_discharge_date_tr25_34_day"), 
           convert = FALSE, remove = FALSE)  %>%
  separate(ed_acute_care_admission_date_tr18_55, 
           into = c("ed_acute_care_admission_date_tr18_55_year",
                    "ed_acute_care_admission_date_tr18_55_month", 
                    "ed_acute_care_admission_date_tr18_55_day"), 
           convert = FALSE, remove=FALSE)  %>%
  mutate(hospital_discharge_date_tr25_34 = 
           paste0(ed_acute_care_admission_date_tr18_55_year, "-", 
                  str_pad(hospital_discharge_date_tr25_34_month, 2, pad=0), "-", 
                  str_pad(hospital_discharge_date_tr25_34_day, 2, pad=0))
  ) 
  
#------------------------------------------------------------------------#
# ANALYSIS: 15 RECORDS ARE RECTIFIED                                    #
#------------------------------------------------------------------------#

filter1  = incorrect_discharge_date %>%
  filter(hospital_discharge_date_tr25_34 >= ed_acute_care_admission_date_tr18_55)

#------------------------------------------------------------------------#
# 13 RECORDS STILL NEED TO BE CORRECTED                                  #  
# SOLUTION 2: REPLACE ED ADMISSIOND DATE WITH INCIDENT DATE              #
#------------------------------------------------------------------------#

incorrect_discharge_date  = incorrect_discharge_date %>%
  filter(hospital_discharge_date_tr25_34 < ed_acute_care_admission_date_tr18_55) %>%
  mutate(ed_acute_care_admission_date_tr18_55 = incident_date_tr5_1)

filter2=incorrect_discharge_date %>%
  filter(hospital_discharge_date_tr25_34 >= ed_acute_care_admission_date_tr18_55)

#------------------------------------------------------------------------#
# RECHECK IF ALL ROWS HAVE BEEN ADDRESSED AND HAS DISCHARGE DATE GREATER #
# THAN OR EQUAL TO ADMISSION DATE. 5 ROWS STILL NEED TO BE ADDRESSED    #
# SOLUTION 3: IF HOSPITAL DISCHARGE ORDERS WRITTEN DATE IS ON OR AFTER   #
# THE DISCHARGE DATE AND ON OR AFTER THE ADMISSION DATE, THEN USE THE    #
# DISCHARGE ORDERS WRITTEN DATE. 2 RECORDS CORRECTED                     #
#------------------------------------------------------------------------#
incorrect_discharge_date = incorrect_discharge_date %>%
  filter(hospital_discharge_date_tr25_34 < ed_acute_care_admission_date_tr18_55)

filter3=incorrect_discharge_date %>%
  filter(hospital_discharge_orders_written_date_tr25_93 >= hospital_discharge_date_tr25_34 &
                 hospital_discharge_orders_written_date_tr25_93 >= ed_acute_care_admission_date_tr18_55) %>%
  mutate(hospital_discharge_date_tr25_34 = hospital_discharge_orders_written_date_tr25_93)
  
#------------------------------------------------------------------------#
# RECHECK IF ALL ROWS HAVE BEEN ADDRESSED AND HAS DISCHARGE DATE GREATER #
# THAN OR EQUAL TO ADMISSION DATE. 5 ROWS STILL NEED TO BE ADDRESSED    #
# SOLUTION 3: IF HOSPITAL DISCHARGE ORDERS WRITTEN DATE IS ON OR AFTER   #
# THE DISCHARGE DATE AND ON OR AFTER THE ADMISSION DATE, THEN USE THE    #
# DISCHARGE ORDERS WRITTEN DATE. 2 RECORDS CORRECTED                     #
#------------------------------------------------------------------------#
incorrect_discharge_date = incorrect_discharge_date %>%
  filter(!incident_id %in% filter3$incident_id)

filter4 = incorrect_discharge_date %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

#------------------------------------------------------------------------#
# REMOVE EXTRA COLUMNS ADDED AND COMBINE WITH ORIGINAL CLEAN_TRAUMA
#------------------------------------------------------------------------#
incorrect_discharge_date = rbind(filter1, filter2, filter3, filter4) %>%
  select(-hospital_discharge_date_tr25_34_year, 
         -hospital_discharge_date_tr25_34_month,
         -hospital_discharge_date_tr25_34_day,
         -ed_acute_care_admission_date_tr18_55_year,
         -ed_acute_care_admission_date_tr18_55_month,
         -ed_acute_care_admission_date_tr18_55_day)

clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% incorrect_discharge_date$incident_id) %>%
  rbind(incorrect_discharge_date)

#------------------------------------------------------------------------#
#6 RECORDS STILL HAVE EMPTY DISCHARGE DATE                               #
#------------------------------------------------------------------------#

no_discharge= clean_trauma1 %>%
  filter(is.na(hospital_discharge_date_tr25_34))


#------------------------------------------------------------------------#
#3 OF THE REAMINING CASES HAVE SOME ICU DAYS AND ADMISSION DATE ON OR  #
# AFTER THE INCIDENT DATE. SET DISCHARGE DATE AS ADMISSION DATE + ICU DAY#
#------------------------------------------------------------------------#

filter1 = no_discharge%>%
  filter(!is.na(icu_days_total_tr26_9)) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55 + icu_days_total_tr26_9)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter1$incident_id,]

#------------------------------------------------------------------------#
#2 OF THE REAMINING CASES HAVE ED ACUTE DISCHARGE DATE ON OR AFTER THE #
# ADMISSION DATE. SET DISCHARGE DATE AS ED DISCHARGE DATE                #
#------------------------------------------------------------------------#
filter2 = no_discharge%>%
  filter(ed_acute_care_discharge_date_tr17_25 >= ed_acute_care_admission_date_tr18_55) %>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_discharge_date_tr17_25)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter2$incident_id,]

#------------------------------------------------------------------------#
#0 OF THE REAMINING CASES ARE MAJORITY WITH DISPOSITION HOME WITHOUT  #
# SERVICES AND AMA AND OTHER. VERY FEW ARE OPERATING ROOM. HENCE SET     #
# DISCHARGE DATE AS ADMISSION DATE                                       #
#------------------------------------------------------------------------#

filter3 =no_discharge%>%
  mutate(hospital_discharge_date_tr25_34 = ed_acute_care_admission_date_tr18_55)

no_discharge=no_discharge[!no_discharge$incident_id %in% filter3$incident_id,]

#------------------------------------------------------------------------#
#COMBINE ALL FILTERS AND REPLACE THE ROWS IN CLEAN_TRAUMA                #
#------------------------------------------------------------------------#

clean_discharge_date = filter1 %>%
  bind_rows(filter2, filter3)

clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% clean_discharge_date$incident_id) %>%
  rbind(clean_discharge_date)

#-------------------------------------------------------------------------------#
# FINALLY CLEAN THE ADMISSION DATE IF IT IS ON OR BEFORE THE INCIDENT DATE BUT  #
# DISCHARGE DATE IS ON OR AFTER INCIDENT DATE                                   #
#-------------------------------------------------------------------------------#
filter5=clean_trauma1 %>%
  filter(ed_acute_care_admission_date_tr18_55 <= incident_date_tr5_1 &
          hospital_discharge_date_tr25_34 >=incident_date_tr5_1  ) %>%
  mutate(ed_acute_care_admission_date_tr18_55 = incident_date_tr5_1)

clean_trauma1 = clean_trauma1 %>%
  filter(!incident_id %in% filter5$incident_id) %>%
  rbind(filter5)

dbSendQuery(con, "drop table clean_trauma")
dbWriteTable(con,c('clean_trauma'), value=clean_trauma1, row.names=FALSE)

dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)

