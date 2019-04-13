#-----------------IMPUTATION & HYPOTHESIS TEST OF IMPUTED FIELDS----------#
# THIS PROGRAM IMPUTES THE PREHOSPITAL SBP, PULSE RATE, OXIMETRY AND RESP RATE #
# MULTIPLE IMPUTATION (MICE) IS USED FOR THIS PURPOSE. MULTIPLE TESTS ARE      #
# TO DETERMINE THE CORRECT IMPUTED SET OUT OF 5 WHICH IS GENERATED USING       #
# OF INTEREST - AGE, GENDER, VEHICLE ACCIDENT, TRANSPORT, OUTCOME, AMPT SCORE  #
# ALL OTHER VARIABLES ARE RETAINED IN THE DATASET FOR MDODELING                #
#                                                                              #   
# IMPORTANT ** THIS PROGRAM MUST BE RUN AFTER HYPOTHESIS TEST AS THE OUTLIERS  #
# AND INCONSISTENCY IN THE PROGRAM IS CARRIED OVER PRIOR TO IMPUTATION         #
# 
# ALL IMPUTATIONS
# TWO TABLES ARE CREATED IN POSTGRESS WHICH CONTAINS THE BEST OF 5 IMPUTED SETS#
#  TO BE USED FOR MODELING                                                     #
#------------------------------------------------------------------------------#
library(mice)
library(finalfit)
library(VIM)
library(dplyr)
library(ggpubr)
library(naniar)

set.seed(9560)


#------------------------------------------------------------------------------#
# USE MODIFIED FEATURE SET FROM HYPOTHESIS_TEST.R PROGRAM DUE TO OUTLIER       #
# IDENTIFIED
#------------------------------------------------------------------------------#

feature_set2 = feature_set1

#------------------------------------------------------------------------------#
# CONVERT VARIABLES FOR TO FACTORS                                             #
#------------------------------------------------------------------------------#

feature_set2 = feature_set2 %>%
  mutate( Outcome = as.factor(Outcome),
          ampt_score_Outcome = as.factor(ampt_score_Outcome),
          transport = as.factor(transport_to_your_facility_by_tr8_8), 
          AgeBin = as.factor(AgeBin), 
          vehicle_accident = as.factor(vehicle_accident), 
          drug_use = as.factor(drug_use_indicator_tr18_45),
          alcohol_use = as.factor(alcohol_intervention_alcohol_screen_tr18_46),
          rural_ind = as.factor(rural_ind), 
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

#---------------------------------------------------------------------#
#CONVERT REQUIRED FEATURES THAT ARE CATEGORICAL TO FACTORS            #
# CAN BE USED FOR MODELING
#---------------------------------------------------------------------#
fac_list=feature_set2 %>%
  select(transport , AgeBin, vehicle_accident,
         drug_use, alcohol_use,
         rural_ind, 
         age, gender, 
         hospital_discharge_disp, 
         ed_discharge_disp, ed_death,
         method_of_payment, 
         ems_unit_notified_time_of_day, 
         trauma_level, years, ampt_score_Outcome, Outcome,
         gcs_severity_flag, gcs_total_match_flag, gcs_total_match_flag, incident_id,
         ems_service_name_tr7_3_y
  )

#---------------------------------------------------------------------#
# LIST NUMERICAL FEATURES                                             #
# CAN BE USED FOR MODELING                                            #
#---------------------------------------------------------------------#

num_list = feature_set2 %>%
  select(pre_oxi, pre_resp, decimal_years,
         pre_sbp, pre_pulse, drive_dist_from_ems, drive_dist_to_facility, ground_time_from_ems,
         ground_time_to_facility, incident_date_tr5_1, incident_time_tr5_18, ems_notify_time_hour,
         gcs_eye_num, gcs_motor_num, gcs_verbal_num,
         gcs_total_manual_imp
         )

new_feature=cbind(num_list, fac_list)         

#----------------------------------------------------------------------#
# SUMMARY OF MISSING DATA                                              #
# GRAPHICAL REPRESENTATION
#----------------------------------------------------------------------#

gg_miss_var(new_feature)

miss_var_summary(new_feature)

sort(sapply(new_feature, function(x) { sum(is.na(x)) }), decreasing=TRUE)


#----------------------------------------------------------------------#
#PATTERN OF MISSINGNESS - MORE PREHOSPITAL MISSING FOR FEMALES         #
# PREHOSPITAL PARMS CAN BE DETERMINED FROM OTHER FIELDS - MISSING AT   #
# RANDOM (MAR)                                                         #
#----------------------------------------------------------------------#

missing_by_gender=new_feature  %>% 
  group_by(gender) %>%
  miss_var_summary()

missing_by_age=new_feature  %>% 
  group_by(AgeBin) %>%
  miss_var_summary()

missing_by_age_gender= new_feature%>% 
  group_by(AgeBin, gender) %>%
  miss_var_summary()

#-------------------------------------------------------------------#
# ANALYZE THE MISSINGESS AND ENSURE IT IS MAR                       #
# MARGIN PLOT SHOWS ALL THESE FIELDS ARE MAR THAT IS THEY DO NOT    #
# HAVE RED/BLUE BOX OF SAME SIZE                                    #
#-------------------------------------------------------------------#

marginplot(new_feature[, c("pre_sbp", "pre_pulse")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "pre_oxi")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "pre_resp")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#------------------------------------IMPUTATION -------------------------------------------------#
# DRY RUN WITHOUT ITERATIONS TO GET THE PREDICTION MATRIX AND MARK THE IMPORTANT VARIABLES 
# TO BE USED FOR IMPUTATION OF THE MISSING VARIABLES - IMPORTANT VARIABLES CONSIDERED ARE AGE,
# ED DEATH, GENDER, VEHICLE ACCIDENT, DRUG, ALCOHOL, ED DISPOSITION, OUTCOME AND AMPT SCORE OUTCOME                 
#-----------------------------------------------------------------------------------------------#
dry_run = mice(new_feature, maxit=0, print=FALSE)

#SET PREDICTORS THAT WILL NOT BE USED TO DETERMINE THE IMPUTATION AS ZERO

pred=dry_run$predictorMatrix
pred[,c('method_of_payment', 
        'ems_notify_time_hour', 'ems_unit_notified_time_of_day', 'trauma_level',
         'hospital_discharge_disp', 'rural_ind', 'gcs_total_manual_imp', 'gcs_eye_num',
        'gcs_verbal_num', 'gcs_motor_num', 'gcs_severity_flag', 'gcs_total_match_flag', 
        'decimal_years', 'ground_time_to_facility', 'ground_time_from_ems', 'drive_dist_to_facility',
        'drive_dist_from_ems',  'incident_date_tr5_1', 
        'incident_time_tr5_18', 'age', 'incident_id',
        'ed_discharge_disp')] = 0

pred

#SET NO IMPUTATION FOR FIELDS THAT WILL NOT BE IMPUTED

meth=dry_run$meth
meth['ems_notify_time_hour'] = ''
meth['ed_discharge_disp'] = ''
meth['ed_death'] = ""
meth['ems_unit_notified_time_of_day'] = ""
meth['trauma_level']= ""
meth['hospital_discharge_disp'] = ""
meth['rural_ind'] = "" 
meth['method_of_payment'] = "" 
meth['ground_time_to_facility']  = ""
meth['drive_dist_to_facility']  = ""
meth['ground_time_from_ems']  = ""
meth['gcs_total_match_flag']  = ""
meth['gcs_severity_flag']  = ""
meth['drive_dist_from_ems']  = ""
meth
#----------------------------------PRODUCTION RUN FOR IMPUTATION --------------------#
# 5 IMPUTED SET WITH 30 ITERATIONS WILL BE GENERATED                                 #
#------------------------------------------------------------------------------------#

library(nnet)
impData=mice(new_feature , seed=500, 
             print=TRUE, maxit = 2, m=2, pred=pred, meth=meth, nnet.MaxNWts=4000)

#-------------------------------------------------------------------#
# ANALYZE THE IMPUTED DATA IF THEY ARE SIMILAR TO THE ORIGINAL DATA #
# ANALYSIS PLOTS SHOW SETS ??? ARE GOOD
#-------------------------------------------------------------------#
plot(impData)

stripplot(impData, pre_oxi + pre_sbp + pre_pulse + pre_resp ~ .imp, pch = 20, cex = 1.2)

densityplot(impData, ~pre_oxi + pre_sbp + pre_pulse + pre_resp)

xyplot(impData, pre_oxi ~ pre_sbp | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_pulse | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_resp | .imp, pch = 20, cex = 1.4)

#-------------------------------------------------------------------#
# SAVE ALL IMPUTED SETS   
#-------------------------------------------------------------------#

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

#--------------------------TESTS OF IMPUTATION-------------------------#
# STACK ALL IMPUTED DATASETS AND GET THE MEAN AND SD FOR EACH SET  AND 
# FOR EACH IMPUTED FIELD
#----------------------------------------------------------------------#
sum(is.na(impData$imp$pre_oxi))

sum(is.na(impData$imp$pre_resp))

sum(is.na(impData$imp$pre_sbp))

sum(is.na(impData$imp$pre_pulse))

final_imp_data =  mice::complete(impData, "long", include=TRUE)

#--------------------------SBP MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET
# SET 1:
# SET 2:
# SET 3:
# SET 4:
# SET 4:
#----------------------------------------------------------------------#
pool_mean_sbp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_sbp, na.rm = TRUE),sd(x$pre_sbp, na.rm = TRUE))))
pool_mean_sbp


#--------------------------RESP MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET
# SET 1:
# SET 2:
# SET 3:
# SET 4:
# SET 4:
#----------------------------------------------------------------------#

pool_mean_resp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_resp, na.rm = TRUE),sd(x$pre_resp, na.rm = TRUE))))
pool_mean_resp

#--------------------------OXI MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET
# SET 1:
# SET 2:
# SET 3:
# SET 4:
# SET 4:
#----------------------------------------------------------------------#

pool_mean_oxi = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_oxi, na.rm = TRUE),sd(x$pre_oxi, na.rm = TRUE))))
pool_mean_oxi

#--------------------------PULSE MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET
# SET 1:
# SET 2:
# SET 3:
# SET 4:
# SET 4:
#----------------------------------------------------------------------#

pool_mean_pulse = with(final_imp_data, by(final_imp_data, .imp, 
                            function(x) c(mean(x$pre_pulse, na.rm = TRUE),
                                          sd(x$pre_pulse, na.rm = TRUE))))
pool_mean_pulse

#----------TEST ZONE FIELD:  HYPOTHESIS TESTING USING IMPUTED SETS---------------#


imp_set=df1                 #----> CHANGE FOR ALLL IMPUTED SETS


#--------------------------------TEST SBP----------------------------------------#
#NULL HYP: There is no difference in sbp for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF SBP ALONE: CHECK NORMAL DISTRIBUTION, SUMMARY, OUTLIERS
#---------------------------------------------------------------------------------#

hist(imp_set$pre_sbp)
ggqqplot(imp_set$pre_sbp)

summary(imp_set$pre_sbp)

ggboxplot(imp_set, x = "ampt_score_Outcome", y = "pre_sbp", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "SBP", xlab = "Outcome")

#---------------------------------------------------------------------------------#
#ANALYSIS OF PAIRED TEST: COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF  #
# SBP IN THE 2 GROUPS AND CHECK NORMAL DISTRIBUTION
#---------------------------------------------------------------------------------#

diff <- with(imp_set, 
             pre_sbp[ampt_score_Outcome == "N"] - 
               pre_sbp[ampt_score_Outcome == "Y"])
hist(diff)

#---------------------------------------------------------------------------------#
#CHECK UNEQUAL VARIANCE. SET TRUE/FALSE BASED ON VARIANCE
#---------------------------------------------------------------------------------#

res =  bartlett.test(pre_sbp ~ ampt_score_Outcome, data = imp_set)
res
res =  bartlett.test(pre_sbp ~ interaction(AgeBin, gender), data = imp_set)
res

#---------------------------------------------------------------------------------#
#  WELCH'S T-TEST
#---------------------------------------------------------------------------------#

t.test(pre_sbp ~ ampt_score_Outcome, data = imp_set, var.equal=FALSE)

#-----------------------------------TEST RESP------------------------------------#
#NULL HYP: There is no difference in resp rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF SBP ALONE: CHECK NORMAL DISTRIBUTION, SUMMARY, OUTLIERS
#---------------------------------------------------------------------------------#

hist(imp_set$pre_resp)
ggqqplot(imp_set$pre_resp)

summary(imp_set$pre_resp)

ggboxplot(imp_set, x = "ampt_score_Outcome", y = "pre_resp", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "RESP RATE", xlab = "Outcome")

#---------------------------------------------------------------------------------#
#ANALYSIS OF PAIRED TEST: COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF  #
# SBP IN THE 2 GROUPS AND CHECK NORMAL DISTRIBUTION
#---------------------------------------------------------------------------------#
library(outliers)
outlier(imp_set$pre_resp)

ggboxplot(imp_set, x = "ampt_score_Outcome", y = "pre_resp", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "RESP RATE", xlab = "Outcome")

diff <- with(imp_set, 
             pre_resp[ampt_score_Outcome == "N"] - 
               pre_resp[ampt_score_Outcome == "Y"])
hist(diff)
ggqqplot(diff)

#---------------------------------------------------------------------------------#
#CHECK UNEQUAL VARIANCE. SET TRUE/FALSE BASED ON VARIANCE
#---------------------------------------------------------------------------------#
res =  bartlett.test(pre_resp ~ ampt_score_Outcome, data = imp_set)
res
res =  bartlett.test(pre_resp ~ interaction(AgeBin, gender), data = imp_set)
res

#---------------------------------------------------------------------------------#
#  WELCH'S T-TEST
#---------------------------------------------------------------------------------#
t.test(pre_resp ~ ampt_score_Outcome, data = imp_set, var.equal=FALSE)


#-----------------------------------TEST PULSE RATE------------------------------#
#NULL HYP: There is no difference in pulse rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF SBP ALONE: CHECK NORMAL DISTRIBUTION, SUMMARY, OUTLIERS
#---------------------------------------------------------------------------------#

library(ggplot2)
hist(imp_set$pre_pulse)
ggqqplot(imp_set$pre_pulse)

summary(imp_set$pre_pulse)

ggboxplot(imp_set, x = "ampt_score_Outcome", y = "pre_pulse", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "PULSE RATE", xlab = "Outcome")

#---------------------------------------------------------------------------------#
#ANALYSIS OF PAIRED TEST: COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF  #
# SBP IN THE 2 GROUPS AND CHECK NORMAL DISTRIBUTION
#---------------------------------------------------------------------------------#
diff <- with(imp_set, 
             pre_pulse[ampt_score_Outcome == "N"] - 
               pre_pulse[ampt_score_Outcome == "Y"])
hist(diff)

#---------------------------------------------------------------------------------#
#CHECK UNEQUAL VARIANCE. SET TRUE/FALSE BASED ON VARIANCE
#---------------------------------------------------------------------------------#
res =  bartlett.test(pre_pulse ~ ampt_score_Outcome, data = imp_set)
res
res =  bartlett.test(pre_pulse ~ interaction(AgeBin, gender), data = imp_set)
res

#---------------------------------------------------------------------------------#
#  WELCH'S T-TEST
#---------------------------------------------------------------------------------#
t.test(pre_pulse ~ ampt_score_Outcome, data = imp_set, var.equal=FALSE)

#-----------------------------------TEST OXI----------------------------------------#
#NULL HYP: There is no difference in oximetry rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF SBP ALONE: CHECK NORMAL DISTRIBUTION, SUMMARY, OUTLIERS
#---------------------------------------------------------------------------------#

hist(imp_set$pre_oxi)
ggqqplot(imp_set$pre_oxi)

summary(imp_set$pre_oxi)

ggboxplot(imp_set, x = "ampt_score_Outcome", y = "pre_oxi", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "OXIMETRY RATE", xlab = "Outcome")

#---------------------------------------------------------------------------------#
#ANALYSIS OF PAIRED TEST: COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF  #
# SBP IN THE 2 GROUPS AND CHECK NORMAL DISTRIBUTION
#---------------------------------------------------------------------------------#
diff <- with(imp_set, 
             pre_oxi[ampt_score_Outcome == "N"] - 
               pre_oxi[ampt_score_Outcome == "Y"])
hist(diff)

#---------------------------------------------------------------------------------#
#CHECK UNEQUAL VARIANCE. SET TRUE/FALSE BASED ON VARIANCE
#---------------------------------------------------------------------------------#
res =  bartlett.test(pre_oxi ~ ampt_score_Outcome, data = imp_set)
res
res =  bartlett.test(pre_oxi ~ interaction(AgeBin, gender), data = imp_set)
res

#---------------------------------------------------------------------------------#
#  WELCH'S T-TEST
#---------------------------------------------------------------------------------#
t.test(pre_oxi ~ ampt_score_Outcome, data = imp_set, var.equal=FALSE)

source("C:/Users/User/Documents/VHS-practicum-code/function_imputation_mice.R")
final_imp_data=sep_gcs(final_imp_data, gcs_eye, gcs_motor, gcs_verbal)

#DATASETS WITH GOOD MEAN AND GOOD TESTS - df5, imp_set
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
