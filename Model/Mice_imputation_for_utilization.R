#----------------------------------UTILIZATION MODEL --------------------------#
# IMPUTATION + TESTS + LOGISTIC REGRESSION + DECISION TREE  
#  THE INPUT TO THIS IS FROM THE HYPOTHESIS TEST WHICH INCLUDES REMOVAL AND    #
#  AND REPLACING OF OUTLIERS
#------------------------------------------------------------------------------#
library(caret)
library(DMwR)
library(dplyr)
set.seed(9560)


feature_set2 = feature_set1

feature_set2 = feature_set2 %>%
  mutate( Outcome = as.factor(Outcome),
          ampt_score_Outcome = as.factor(ampt_score_Outcome),
          transport = as.factor(transport_to_your_facility_by_tr8_8), 
          AgeBin = as.factor(AgeBin), 
          vehicle_accident = as.factor(vehicle_accident), 
          drug_use = as.factor(drug_use_indicator_tr18_45),
          alcohol_use = as.factor(alcohol_intervention_alcohol_screen_tr18_46),
          rural_ind = as.factor(rural_ind), 
          sbpinrange = as.factor(sbpinrange), 
          pulseinrange = as.factor(pulseinrange), 
          respinrange = as.factor(respinrange), 
          gcs_eye=as.factor(prehospital_gcs_eye),
          gcs_motor = as.factor(prehospital_gcs_motor),
          gcs_verbal = as.factor(prehospital_gcs_verbal),
          gcs_eye_num = as.factor(prehospital_gcs_eye_num), 
          gcs_motor_num = as.factor(prehospital_gcs_motor_num), 
          gcs_verbal_num = as.factor(prehospital_gcs_verbal_num),
          gcs_total_manual= as.factor(prehospital_gcs_total_manual_tr18_64),
          gcs_total_manual_imp = as.factor(prehospital_gcs_total_manual_tr18_64_imp), 
          age = as.factor(patient_age_units_tr1_14), 
          gender = as.factor(patient_gender_tr1_15), 
          ethinicity = as.factor(patient_ethnicity_tr1_17),
          oximetryinrange = as.factor(oximetryinrange), 
          hospital_discharge_disp = as.factor(hospital_discharge_disposition_tr25_27), 
          ed_discharge_disp = as.factor(ed_acute_care_disposition_tr17_27), 
          gcs_severity_flag = as.factor(gcs_severity_flag),
          gcs_total_match_flag = as.factor(gcs_total_match_flag), 
          method_of_payment= as.factor(financial_primary_method_of_payment_tr2_5), 
          ems_unit_notified_time_hr = as.factor(ems_unit_notified_time_hr),
          ems_unit_notified_time_of_day = as.factor(ems_unit_notified_time_of_day), 
          ems_notify_time_hour = as.factor(ems_notify_time_hour), 
          ed_death = as.factor(ed_death_tr27_14),
          trauma_level = as.factor(trauma_level),
          years=as.factor(years),
          pre_oxi = prehospital_pulse_oximetry_tr18_82,
          pre_pulse = prehospital_pulse_rate_tr18_69,
          pre_sbp=prehospital_sbp_tr18_67,
          pre_resp = prehospital_respiratory_rate_tr18_70)

#convert categorical variables to factors
fac_list=feature_set2 %>%
  select(transport , AgeBin, vehicle_accident,
         drug_use, alcohol_use,
         rural_ind, decimal_years,
         gcs_eye_num, gcs_motor_num, gcs_verbal_num,
         gcs_total_manual_imp, 
         age, gender, 
         hospital_discharge_disp, 
         ed_discharge_disp, ed_death,
         method_of_payment, 
         ems_unit_notified_time_of_day, 
         ems_notify_time_hour, trauma_level, years, ampt_score_Outcome, Outcome,
         gcs_severity_flag, gcs_total_match_flag, sbpinrange, pulseinrange, respinrange,
         oximetryinrange, gcs_total_match_flag, incident_id, ems_service_name_tr7_3_y
  )

num_list = feature_set2 %>%
  select(pre_oxi, pre_resp,
         pre_sbp, pre_pulse, drive_dist_from_ems, drive_dist_to_facility, ground_time_from_ems,
         ground_time_to_facility, incident_date_tr5_1, incident_time_tr5_18
         )

new_feature=cbind(num_list, fac_list)         

#------------------------------------------------------------------------------------------------
#glimpse of missing data - counts and percentage of missing cat and num varibale
# 
#-------------------------------------------------------------------------------------------------
library(naniar)

gg_miss_var(new_feature)

miss_var_summary(new_feature)

#NUMBER OF NA IN THE DATA
sort(sapply(new_feature, function(x) { sum(is.na(x)) }), decreasing=TRUE)

#pattern of missingness
missing_by_gender=new_feature  %>% 
  group_by(gender) %>%
  miss_var_summary()

missing_by_age=new_feature  %>% 
  group_by(AgeBin) %>%
  miss_var_summary()


missing_by_age_gender= new_feature%>% 
  group_by(AgeBin, gender) %>%
  miss_var_summary()

#--------------------------IMPUTATION ----------------------------------#
library(mice)
library(finalfit)
library(VIM)

#----------------------------------------------------------
#IMPUTATION FOR SBP, PULSE, OXI, RESP, GCS INDI
#All the numericals are misssing at random MAR
#----------------------------------------------------------
#MARGIN PLOT SHOWS ALL THESE FIELDS ARE MAR THAT IS THEY DO NOT HAVE RED/BLUE BOX SAME
marginplot(new_feature[, c("pre_sbp", "pre_pulse")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "pre_oxi")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "pre_resp")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#--------------------------------------PRODUCTION CHANGE ----------------------------------------#
#change m and maxit in production to avoid bias due to simulation, The higher number of iterations
#the better
#------------------------------------------------------------------------------------------------#
#------------------------------------IMPUTATION -------------------------------------------------#
# RUN A DRY RUN WITHOUT ITERATIONS TO GET THE PREDICTION MATRIX AND MARK THE IMPORTANT VARIABLES 
# TO BE USED FOR IMPUTATION OF THE MISSING VARIABLES - IMPORTANT VARIABLES CONSIDERED ARE AGE,
# GENDER, VEHICLE ACCIDENT, DRUG, ALCOHOL, ED DISPOSITION, outcome and AMPT SCORE OUTCOME                 
#-----------------------------------------------------------------------------------------------#
dry_run = mice(new_feature, maxit=0, print=FALSE)
pred=dry_run$predictorMatrix
pred[,c('method_of_payment', 
        'ems_notify_time_hour', 'ems_unit_notified_time_of_day', 'trauma_level',
         'hospital_discharge_disp', 'rural_ind', 'gcs_total_manual_imp', 'gcs_eye_num',
        'gcs_verbal_num', 'gcs_motor_num', 'gcs_severity_flag', 'gcs_total_match_flag', 
        'sbpinrange', 'pulseinrange', 'respinrange', 'decimal_years',
        'oximetryinrange', 'gcs_total_match_flag', 'gcs_total_manual_imp',
        'ed_discharge_disp', 'ed_death')] = 0

pred

meth=dry_run$meth
meth['method_of_payment'] = ''
meth['ems_notify_time_hour'] = ''
meth['gcs_severity_flag']= ''
meth['gcs_total_match_flag']=''  
meth['sbpinrange']='' 
meth['pulseinrange']=''
meth['respinrange']=''
meth['decimal_years']=''
meth['oximetryinrange']=''
meth['ed_discharge_disp'] = ''
meth['ed_death'] = ""
meth['ems_unit_notified_time_of_day'] = ""
meth['trauma_level']= ""
meth['hospital_discharge_disp'] = ""
meth['rural_ind'] = "" 
meth['method_of_payment'] = "" 
meth['gcs_total_manual_imp']  = ""
meth['gcs_eye_num']  = ""
meth['gcs_verbal_num']  = ""
meth['gcs_motor_num']  = ""

library(nnet)
impData=mice(new_feature , seed=500, 
             print=TRUE, maxit = 30, m=5, pred=pred, meth=meth, nnet.MaxNWts=4000)

plot(impData)

#predictors used to impute the missing features
# pulse rate
# sbp
# pulse oximetry
# resp rate
# gcs eye
# gcs verbal
# gcs motor

#check how different the numerical imputed data are
stripplot(impData, pch = 20, cex = 1.2)

densityplot(impData)
xyplot(impData, pre_oxi ~ pre_sbp | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_pulse | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_resp | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_eye | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_verbal | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_motor | .imp, pch = 20, cex = 1.4)

df1=mice::complete(impData, 1)
df2=mice::complete(impData, 2)
df3=mice::complete(impData, 3)
df4=mice::complete(impData, 4)
df5=mice::complete(impData, 5)

write.csv(df1, "/home/rstudio/trauma/imputed_data_df1.csv")
write.csv(df2, "/home/rstudio/trauma/imputed_data_df2.csv")
write.csv(df3, "/home/rstudio/trauma/imputed_data_df3.csv")
write.csv(df4, "/home/rstudio/trauma/imputed_data_df4.csv")
write.csv(df5, "/home/rstudio/trauma/imputed_data_df5.csv")

final_imp_data =  mice::complete(impData, "long", include=TRUE)
# compute mean and standard deviation in each imputed dataset - SBP
pool_mean_sbp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_sbp, na.rm = TRUE),sd(x$pre_sbp, na.rm = TRUE))))
pool_mean_sbp
Reduce("+",pool_mean_sbp)/length(pool_mean_sbp)


# compute mean and standard deviation in each imputed dataset - resp
pool_mean_resp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_resp, na.rm = TRUE),sd(x$pre_resp, na.rm = TRUE))))
pool_mean_resp
Reduce("+",pool_mean_resp)/length(pool_mean_resp)

# compute mean and standard deviation in each imputed dataset - oxi
pool_mean_oxi = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_oxi, na.rm = TRUE),sd(x$pre_oxi, na.rm = TRUE))))
pool_mean_oxi
Reduce("+",pool_mean_oxi)/length(pool_mean_oxi)


# compute mean and standard deviation in each imputed dataset - pulse
pool_mean_pulse = with(final_imp_data, by(final_imp_data, .imp, 
                            function(x) c(mean(x$pre_pulse, na.rm = TRUE),sd(x$pre_pulse, na.rm = TRUE))))
pool_mean_pulse

source("C:/Users/User/Documents/VHS-practicum-code/function_imputation_mice.R")
final_imp_data=sep_gcs(final_imp_data, gcs_eye, gcs_motor, gcs_verbal)

#POOL INDEPENDENT T-TEST ON PRE-OXI
fit_pre_oxi <- with(data=impData, exp=lm(pre_oxi ~ Outcome))
fit_pre_oxi_estimates <- pool(fit_pre_oxi)  
summary(fit_pre_oxi_estimates)


#POOL INDEPENDENT T-TEST ON PRE-RESP
fit_pre_resp <- with(data=impData, exp=lm(pre_resp ~ Outcome))
fit_pre_resp_estimates <- pool(fit_pre_resp)  
summary(fit_pre_resp_estimates)


#POOL INDEPENDENT T-TEST ON PRE-SBP
fit_pre_sbp <- with(data=impData, exp=lm(pre_sbp ~ Outcome))
fit_pre_sbp_estimates <- pool(fit_pre_sbp)  
summary(fit_pre_sbp_estimates)

#POOL INDEPENDENT T-TEST ON PRE-PULSE
fit_pre_pulse <- with(data=impData, exp=lm(pre_pulse ~ Outcome))
fit_pre_pulse_estimates <- pool(fit_pre_pulse)  
summary(fit_pre_pulse_estimates)


#DATASETS WITH GOOD MEAN AND GOOD TESTS - df5, df2
library('RPostgreSQL')

source("/home/rstudio/R/VHS_github/VHS-practicum-code/aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

dbSendQuery(con, "drop table new_feature_1")
dbWriteTable(con,c('new_feature_1'), value=df5, row.names=FALSE)

dbSendQuery(con, "drop table new_feature_2")
dbWriteTable(con,c('new_feature_2'), value=df2, row.names=FALSE)
