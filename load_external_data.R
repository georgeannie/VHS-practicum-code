###########################################################################################################
#                 LOAD ZIP CODES WITH LATITUDE/LONGITUDE/URBAN/RURAL INFO                                #
#                             LOAD POSTGRES TABLE IN AWS                                                  #
###########################################################################################################
library(janitor)
library(lubridate)
library(hms)
library(tidyr)
library(stringr)
library(readr)
library(RcppRoll)
library(dplyr)
library(tibble)
#-------------------Connect to DB -------------------------------# 
library('RPostgreSQL')
source("aws_rds_access.R")
pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#-----------------READ ZIP CODES FOR RURAL/URBAN FLAG --------------------------#
zip=read.csv("/home/rstudio/trauma/zip_rural.csv" , 
                header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
   clean_names() 

dbSendQuery(con, "drop table zipcode_info")
dbWriteTable(con,c('zipcode_info'), value=zip, row.names=FALSE)

#--------------------READ EMS AGENCY --- STILL WAITING --------------#
ems_agency=read.csv("/home/rstudio/trauma/ems_agency.csv" , 
                    header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
  clean_names() 

zip_ems = left_join(zip, ems_agency, by='zip_code')

dbSendQuery(con, "drop table zip_ems")
dbWriteTable(con,c('zip_ems'), value=zip_ems, row.names=FALSE)


#--------------------READ FACILTY ---STILL WAITING-------------------
facility=read.csv("/home/rstudio/trauma/facility.csv" , 
                    header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
  clean_names() 

zip_fac = left_join(zip, facility, by='zip_code')


dbSendQuery(con, "drop table zip_facility")
dbWriteTable(con,c('zip_facility'), value=zip_fac, row.names=FALSE)

