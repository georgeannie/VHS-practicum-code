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

#-----------------READ THE COMBINED TRAUMA DATA - GEMS AND HEMS AND REMOVE COLUMNS--------------------------#
zip=read.csv("/home/rstudio/trauma/zipcode_info.csv" , 
                header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
   clean_names() 

geography=read.csv("/home/rstudio/trauma/geography.csv" , 
                   header = TRUE,  na.strings=c(""," ", "NA"), stringsAsFactors = FALSE) %>%
  clean_names() 

zip_geo = left_join(zip, geography, by='zip_code')


dbSendQuery(con, "drop table zipcode_info")
dbWriteTable(con,c('zipcode_info'), value=zip_geo, row.names=FALSE)
