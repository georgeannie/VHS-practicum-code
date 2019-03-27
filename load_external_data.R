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
zip=read.csv("/home/rstudio/trauma/input/rural.csv" , 
                header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
   clean_names() 

dbSendQuery(con, "drop table zipcode_info")
dbWriteTable(con,c('zipcode_info'), value=zip, row.names=FALSE)

#--------------------READ EMS AGENCY -----------------------------------#
ems_agency=read.csv("/home/rstudio/trauma/input/ems_agency.csv" , 
                    header = TRUE,  na.strings=c(""," ", "NA"), 
                    stringsAsFactors = FALSE) %>%
  clean_names() 

dbSendQuery(con, "drop table zip_ems")
dbWriteTable(con,c('zip_ems'), value=ems_agency, row.names=FALSE)

#--------------------READ FACILTY -----------------------------------------
facility=read.csv("/home/rstudio/trauma/input/facilities.csv" , 
                  header = TRUE,  na.strings=c(""," ", "NA"), 
                  stringsAsFactors = FALSE) %>%
  clean_names() 

dbSendQuery(con, "drop table zip_facility")
dbWriteTable(con,c('zip_facility'), value=facility, row.names=FALSE)