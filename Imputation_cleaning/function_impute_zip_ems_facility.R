#---------------------------------------------------------------------------#
# Function1: unique_entry                                                   #
#  GROUP AND FIND MOST USED ENTRIES FOR IMPUTATION AND CLEANING OF          #
#         THE DATASET                                                       #
# Function2: get_entry                                                      #
#  GROUP AND FIND MOST USED ENTRIES FOR IMPUTATION                          #
#---------------------------------------------------------------------------#

library(janitor)
library(sqldf)
library(dplyr)
library(rlang)
library(tidyverse)
unique_entry=function(df, var1, distinct_var2, key_var){

#---------------------------------------------------------------------------#
#               ENQUO TO REMOVE QUOTES AND PASS ARGUMENTS AS COLUMNS        #
#---------------------------------------------------------------------------#
  var1_e=enquo(var1)
  distinct_var2_e=enquo(distinct_var2)
  df_e=enquo(df)
  key_var_e=enquo(key_var)
  
#---------------------------------------------------------------------------#
#                 GROUP ENTRIES USING THE DISTINCT ARGUMENT PASSED          #
#---------------------------------------------------------------------------#
  group_count= df %>%
    group_by(!!var1_e ,!!distinct_var2_e)%>%
    dplyr::summarize(Count=n()) %>%
    filter(!is.na(!!var1_e) & !is.na(!!distinct_var2_e))  %>%
    arrange(-Count) %>%
    distinct(!!distinct_var2_e)
  
  
#---------------------------------------------------------------------------#
#         GET THE FREQUENT USED ENTRY (VAR1) FOR EACH OF THE GROUP ENTRIES  #
#---------------------------------------------------------------------------#
  sql = sprintf("select min(rowid) row_names, %s, %s 
                from group_count 
                group by %s", 
                deparse(substitute(var1)), 
                deparse(substitute(distinct_var2)), 
                deparse(substitute(distinct_var2))
  )
  
  unique_query=sqldf(sql, row.names=TRUE)  
  
#---------------------------------------------------------------------------#
#   ATTACH THE FREQUENT ENTRIES TO EACH KEY ENTRY WHICH HAS NULL VARIABLE   #
#---------------------------------------------------------------------------#
  
  sql = sprintf("select b.%s, a.%s, b.%s 
                from unique_query a 
                 join df b 
                on a.%s = b.%s 
                 and b.%s is null", 
                deparse(substitute(key_var)), 
                deparse(substitute(var1)), 
                deparse(substitute(distinct_var2)),
               # deparse(substitute(df)),
                deparse(substitute(distinct_var2)),
                deparse(substitute(distinct_var2)),
                deparse(substitute(var1))
  )
  
  incident_missing=sqldf(sql, row.names=TRUE)
  
#---------------------------------------------------------------------------#
#   MATCH AND MERGE THE KEY ENTRIES WITH THE MAIN DATAFRAME (DF)            #
#---------------------------------------------------------------------------#

  final_df=left_join(df, incident_missing, by =c(quo_name(key_var_e)  , 
                                                 quo_name(distinct_var2_e))) %>%
    clean_names()

#---------------------------------------------------------------------------#
#   CONVERT COLUMN NAME FROM ARGUMENT AS IS                                 #
#---------------------------------------------------------------------------#
  
  var1_new=quo_name(paste0(deparse(substitute(var1)), "_x"))
  var1_y_new=quo_name(paste0(deparse(substitute(var1)), "_y"))

#---------------------------------------------------------------------------#
#   REPLACE THE NULLS IN THE ACTUAL COLUMN IN THE DATAFRAME WITH THE NEW    #
#   COLUMN VALUE                                                            #
#---------------------------------------------------------------------------#
  
  final_df[is.na(final_df[,var1_new]), var1_new] = final_df[is.na(final_df[,var1_new]), var1_y_new]

#---------------------------------------------------------------------------#
#   REMOVE THE NEW COLUMN CREATED AFTER MERGE                               #
#---------------------------------------------------------------------------#
  
  final_df=final_df[,!names(final_df) %in% c(var1_y_new)]

#---------------------------------------------------------------------------#
#   REPLACE COLUMN NAME WITH THE ACTUAL COLUMN NAME PASSED AS ARG           #
#---------------------------------------------------------------------------#
  names(final_df)[names(final_df) == var1_new] = as.character(deparse(substitute(var1)))    
  return(final_df)
  
}


get_entry=function(df, var1, distinct_var2){
  
  #---------------------------------------------------------------------------#
  #               ENQUO TO REMOVE QUOTES AND PASS ARGUMENTS AS COLUMNS        #
  #---------------------------------------------------------------------------#
  var1_e=enquo(var1)
  distinct_var2_e=enquo(distinct_var2)
  df_e=enquo(df)
  
  #---------------------------------------------------------------------------#
  #                 GROUP ENTRIES USING THE DISTINCT ARGUMENT PASSED          #
  #---------------------------------------------------------------------------#
  group_count= df %>%
    group_by(!!var1_e ,!!distinct_var2_e)%>%
    dplyr::summarize(Count=n()) %>%
    filter(!is.na(!!var1_e) & !is.na(!!distinct_var2_e)) %>%
    arrange(-Count) %>%
    distinct(!!distinct_var2_e)
  
  #---------------------------------------------------------------------------#
  #         GET THE FREQUENT USED ENTRY (VAR1) FOR EACH OF THE GROUP ENTRIES  #
  #---------------------------------------------------------------------------#
  sql = sprintf("select min(rowid) row_names, %s, %s 
                from group_count 
                group by %s", 
                deparse(substitute(var1)), 
                deparse(substitute(distinct_var2)), 
                deparse(substitute(distinct_var2))
  )
  
  unique_query=sqldf(sql, row.names=TRUE)  
}