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
# ALL IMPUTATIONS SETS GENERATED MUST BE TESTED FOR NORMAL DISTRIBUION AND     #
# SIGNIFICANCE BETWEEN THOSE WHO HAVE OUTCOME OF HELICOPTER REQUIRED AND THOSE #
# THAT DO NOT                                                                  #
#                                                                              #  
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
          age = as.factor(patient_age_tr1_12), 
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
          pre_resp = prehospital_respiratory_rate_tr18_70,
          patient_age_units_tr1_14=as.factor(patient_age_units_tr1_14))

#---------------------------------------------------------------------#
#CONVERT REQUIRED FEATURES THAT ARE CATEGORICAL TO FACTORS            #
# CAN BE USED FOR MODELING
#---------------------------------------------------------------------#
fac_list=feature_set2 %>%
  select(transport , AgeBin, vehicle_accident,
         drug_use, alcohol_use,
         rural_ind, 
         gender, 
         hospital_discharge_disp, 
         ed_discharge_disp, ed_death,
         method_of_payment, 
         ems_unit_notified_time_of_day, 
         trauma_level, ampt_score_Outcome, Outcome,
         gcs_severity_flag, gcs_total_match_flag, gcs_total_match_flag, incident_id,
         ems_service_name_tr7_3_y, patient_age_units_tr1_14
  )

#---------------------------------------------------------------------#
# LIST NUMERICAL FEATURES                                             #
# CAN BE USED FOR MODELING                                            #
#---------------------------------------------------------------------#

num_list = feature_set2 %>%
  select(pre_oxi, pre_resp, decimal_years, age,
         pre_sbp, pre_pulse, drive_dist_from_ems, drive_dist_to_facility, ground_time_from_ems,
         ground_time_to_facility, incident_date_tr5_1, incident_time_tr5_18, ems_notify_time_hour,
         gcs_eye_num, gcs_motor_num, gcs_verbal_num, years,
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
        'incident_time_tr5_18', 'age', 'incident_id', 'years', 'patient_age_units_tr1_14',
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
# 5 IMPUTED SET WITH 40 ITERATIONS WILL BE GENERATED                                 #
#------------------------------------------------------------------------------------#

library(nnet)
impData=mice(new_feature , seed=500, 
             print=TRUE, maxit = 40, m=5, pred=pred, meth=meth, nnet.MaxNWts=4000)

#-------------------------------------------------------------------#
# ANALYZE THE IMPUTED DATA IF THEY ARE SIMILAR TO THE ORIGINAL DATA #
# ANALYSIS PLOTS SHOW SETS ??? ARE GOOD VISUALLY
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
# ENSURE ALL IMPUTED SETS HAVE NO NA'S                                  
# STACK ALL IMPUTED DATASETS AND GET THE MEAN AND SD FOR EACH SET  AND 
# FOR EACH IMPUTED FIELD
#----------------------------------------------------------------------#
sum(is.na(impData$imp$pre_oxi))

sum(is.na(impData$imp$pre_resp))

sum(is.na(impData$imp$pre_sbp))

sum(is.na(impData$imp$pre_pulse))


final_imp_data =  mice::complete(impData, "long", include=TRUE)
write.csv(final_imp_data, "/home/rstudio/trauma/imputed_data_all.csv")

#------COMPARE BEST MEAN AND SD SETS WITH THOSE SELECTED VISUALLY------#
#--------------------------SBP MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET    141.46   30.57
# SET 1:          142.34    31.57
# SET 2:          142.52    31.6
# SET 3:          142.40    31.6
# SET 4:          142.40    31.69
# SET 5:          142.32    31.68
#   GOOD SET 1 / 5
#----------------------------------------------------------------------#
pool_mean_sbp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_sbp, na.rm = TRUE),
                                            sd(x$pre_sbp, na.rm = TRUE))))
pool_mean_sbp


#--------------------------RESP MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET    18.19     5.54
# SET 1:          18.06     5.47
# SET 2:          18.10     5.60
# SET 3:          18.07     5.55
# SET 4:          18.11     5.54
# SET 5:          18.04     5.43
#   REASONABLE SETS: 4 / 3
#----------------------------------------------------------------------#

pool_mean_resp = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_resp, na.rm = TRUE),sd(x$pre_resp, na.rm = TRUE))))
pool_mean_resp

#--------------------------OXI MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET    95.82     8.23
# SET 1:          95.46     9.69
# SET 2:          95.41     8.99
# SET 3:          95.41     10.05
# SET 4:          95.37     10.30
# SET 5:          95.48     9.81
#     REASONABLE 2/ 5
#----------------------------------------------------------------------#

pool_mean_oxi = with(final_imp_data, by(final_imp_data, .imp, 
                              function(x) c(mean(x$pre_oxi, na.rm = TRUE),sd(x$pre_oxi, na.rm = TRUE))))
pool_mean_oxi

#--------------------------PULSE MEAN AND SD ----------------------------#
#                 MEAN      SD
# ORIGINAL SET    89.44     21.89
# SET 1:          88.5      21.93
# SET 2:          88.42     21.89
# SET 3:          88.25     21.89
# SET 4:          88.39     21.92
# SET 5:          88.39     21.84
#       REASONABLE: 2/5
#----------------------------------------------------------------------#

pool_mean_pulse = with(final_imp_data, by(final_imp_data, .imp, 
                            function(x) c(mean(x$pre_pulse, na.rm = TRUE),
                                          sd(x$pre_pulse, na.rm = TRUE))))
pool_mean_pulse


#--------------------------------------------------------------------------------#
#----------TEST ZONE FIELD:  HYPOTHESIS TESTING USING IMPUTED SETS---------------#
#               AUTOMATED RUN OF THE SCRIPT NOT ALLOWED                          #
#--------------------------------------------------------------------------------#
#     USING  2 AND 5 IMPUTED SETS


imp_set=df5             #----> CHANGE TO DF2/3/4/5 TO TEST FOR ALLL IMPUTED SETS


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
t.test(pre_oxi ~ ampt_score_Outcome, data = imp_set, var.equal=TRUE)


#---------------------------SELECTION OF IMPUTED DATAFRAME-------------------------#
#     2/5
#-----------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------#
#                     ADD FEATURES FOR THE PREHOSPITAL PARAMETERS IMPUTED           #
#  CHECK FOR NORMAL RANGE Y/N AND GET DIFFERENCE FROM NORMAL BASED ON AGE. ADD THE  #
#  TO BOTH IMPUTED DATASETS SELECTED PRIOR TO LOADING DATABASE                      #
#-----------------------------------------------------------------------------------#
conversion=df2  #CHANGE TO DF1/2/3/4/5 BASED ON BEST IMPUTED SETS DETERMINED
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

conversion$respinrange<-ifelse(conversion$years<1, 
                               (ifelse(conversion$pre_resp>29 & conversion$pre_resp<54,1,0)),
                               ifelse(conversion$years>=1 & conversion$Years<3,
                                      (ifelse(conversion$pre_resp>21 & conversion$pre_resp<38,1,0)),
                                      ifelse(conversion$years>=3 & conversion$years<6,
                                             (ifelse(conversion$pre_resp>19 & conversion$pre_resp<29,1,0)),
                                             ifelse(conversion$years>=6 & conversion$years<12,       
                                                    (ifelse(conversion$pre_resp>17 & conversion$pre_resp<26,1,0)),
                                                    ifelse(conversion$years>=12 & conversion$years<16, 
                                                           (ifelse(conversion$pre_resp>11 & conversion$pre_resp<21,1,0)),
                                                           ifelse(conversion$years>=16, 
                                                                  (ifelse(conversion$pre_resp>11 & conversion$pre_resp<19,1,0)),
                                                                  0
                                                           )))))) 

conversion$respinrange[is.na(conversion$respinrange)] = '1'

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

dbWriteTable(con,c('imputed_feature2'), value=df2, row.names=FALSE)
