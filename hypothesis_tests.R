#------------------------------------------------------------------------------------------------#
#             HYPOTHESIS TESTS FOR DIFFERENT PREHOSPITAL PARAMETERS AND ITS IMPACTS ON THE       #
#               OUTCOMES
#------------------------------------------------------------------------------------------------#

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
feature_set=dbGetQuery(con, 'select * from feature_set_trauma')


feature_set1=feature_set

feature_set1 = feature_set1 %>%
  mutate( Outcome = as.factor(Outcome),
          ampt_score = as.factor(ampt_score_Outcome),
          transport = as.factor(transport_to_your_facility_by_tr8_8), 
          AgeBin = as.factor(AgeBin), 
          vehicle_accident = as.factor(vehicle_accident), 
          drug_use= as.factor(drug_use_indicator_tr18_45),
          alcohol_use = as.factor(alcohol_intervention_alcohol_screen_tr18_46),
          rural_ind = as.factor(rural_ind), 
          sbpinrange = as.factor(sbpinrange), 
          pulseinrange = as.factor(pulseinrange), 
          respinrange = as.factor(respinrange), 
          gcs_eye=as.factor(prehospital_gcs_eye),
          gcs_motor = as.factor(prehospital_gcs_motor),
          gcs_verbal = as.factor(prehospital_gcs_verbal),
          prehospital_gcs_eye_num = as.factor(prehospital_gcs_eye_num), 
          prehospital_gcs_motor_num = as.factor(prehospital_gcs_motor_num), 
          prehospital_gcs_verbal_num = as.factor(prehospital_gcs_verbal_num),
          prehospital_gcs_total_manual_tr18_64 = as.factor(prehospital_gcs_total_manual_tr18_64),
          prehospital_gcs_total_manual_tr18_64_imp = as.factor(prehospital_gcs_total_manual_tr18_64_imp), 
          age_units = as.factor(patient_age_units_tr1_14), 
          gender = as.factor(patient_gender_tr1_15), 
          ethinicity = as.factor(patient_ethnicity_tr1_17),
          oximetryinrange = as.factor(oximetryinrange), 
          hospital_discharge_disp = as.factor(hospital_discharge_disposition_tr25_27), 
          ed_discharge_disp = as.factor(ed_acute_care_disposition_tr17_27), 
          gcs_severity_flag = as.factor(gcs_severity_flag),
          gcs_total_match_flag = as.factor(gcs_total_match_flag), 
          financial_pay_method = as.factor(financial_primary_method_of_payment_tr2_5), 
          ems_unit_notified_time_hr = as.factor(ems_unit_notified_time_hr),
          ems_unit_notified_time_of_day = as.factor(ems_unit_notified_time_of_day), 
          ems_notify_time_hour = as.factor(ems_notify_time_hour), 
          ed_death = as.factor(ed_death_tr27_14),
          trauma_level = as.factor(trauma_level),
          years=as.factor(years))

#-------------------------------------TEST 1-----------------------------------#
#NULL HYP: There is no difference in the age groups that need helicopter
#CHI SQUARE USING CONTINGENCY TABLE
# There is significant difference in the need for helicopter for each group and senior citizens
#--------------------------------------------------------------------------------#
# tend to contribute most to the need for helicopters
dt_table=table(feature_set1$AgeBin, feature_set1$Outcome)
dt_table
  
chisq=chisq.test(dt_table)
chisq

library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib, is.cor = FALSE)

#---------------------------------------TEST 2 -----------------------------------#
#NULL HYP: There is no difference in the gender that need helicopter
#CHI SQUARE USING CONTINGENCY TABLE
# There is significant difference in the need for helicopter for each gender
#---------------------------------------------------------------------------------#
dt_table=table(feature_set1$gender, feature_set1$Outcome)
dt_table

chisq=chisq.test(dt_table)
chisq

library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib, is.cor = FALSE)

#-----------------------------------TEST 3----------------------------------------#
#NULL HYP: There is no difference in sbp for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF SBP ALONE
# HISTOGRAM SHOWS NORMAL DISTRIBUTION. QQPLOT ALSO SHOWS NORMAL DISTRIBUTION.
# USE ANOVA TEST 
# SUMMARY STATS OF SBP:  Median = 140, Mean = 141.5, NA = 21164, Max =290

library(ggpubr)
hist(feature_set1$prehospital_sbp_tr18_67)
ggqqplot(feature_set1$prehospital_sbp_tr18_67)

summary(feature_set1$prehospital_sbp_tr18_67)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_sbp_tr18_67", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "SBP", xlab = "Outcome")

#ANALYSIS OF PAIRED TEST
#COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF SBP IN THE 2 GROUPS
#THE HISTOGRAM SHOWS NORMAL DISTRIBUTION
diff <- with(feature_set1, 
              prehospital_sbp_tr18_67[ampt_score_Outcome == "N"] - 
                  prehospital_sbp_tr18_67[ampt_score_Outcome == "Y"])
hist(diff)

#DUE TO UNEQUAL SAMPLE SIZE IN TWO GROUP, TEST FOR EQUAL VARIANCE IN THE 2 GROUPS
#OUTCOME: SUGGEST THE VARIANCE IS SIGNIFICANTLY DIFFERENT IN BOTH SAMPLES by outcome and by age/gender
res =  bartlett.test(prehospital_sbp_tr18_67 ~ ampt_score_Outcome, data = feature_set1)
res
res =  bartlett.test(prehospital_sbp_tr18_67 ~ interaction(AgeBin, gender), data = feature_set1)
res

#SINCE VARIANCE OCCURS IN THE SAMPLE, USE WELCH'S T-TEST
#Outcome: There is significant difference in sbp for the two groups that need helicopter and that do not
t.test(prehospital_sbp_tr18_67 ~ ampt_score_Outcome, data = feature_set1, var.equal=FALSE)

#-----------------------------------TEST 4----------------------------------------#
#NULL HYP: There is no difference in resp rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF RESP RATE ALONE
# HISTOGRAM DOES NOT SHOW NORMAL DISTRIBUTION. QQPLOT DOES NOT SHOW NORMAL DISTRIBUTION.
# USE ANOVA TEST 
# SUMMARY STATS OF RESP RATE:  Median = 18, Mean = 18.2, NA = 21074, Max =290

hist(feature_set1$prehospital_respiratory_rate_tr18_70)
ggqqplot(feature_set1$prehospital_respiratory_rate_tr18_70)

summary(feature_set1$prehospital_respiratory_rate_tr18_70)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_respiratory_rate_tr18_70", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "RESP RATE", xlab = "Outcome")

#OUTLIER OBSERVED IN BOX PLOT. The outlier is 120. HENCE SET OUTLIER AS NA
library(outliers)
outlier(feature_set1$prehospital_respiratory_rate_tr18_70)
test1=feature_set1 %>% 
  filter(!prehospital_respiratory_rate_tr18_70 == 120 |
           is.na(prehospital_respiratory_rate_tr18_70) )
test2 = feature_set1%>%
  filter(prehospital_respiratory_rate_tr18_70 == 120) %>%
  mutate(prehospital_respiratory_rate_tr18_70 = NA)
feature_set1 = rbind(test1, test2)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_respiratory_rate_tr18_70", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "RESP RATE", xlab = "Outcome")

#ANALYSIS OF PAIRED TEST
#COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF SBP IN THE 2 GROUPS
#THE HISTOGRAM SHOWS NORMAL DISTRIBUTION
diff <- with(feature_set1, 
             prehospital_respiratory_rate_tr18_70[ampt_score_Outcome == "N"] - 
               prehospital_respiratory_rate_tr18_70[ampt_score_Outcome == "Y"])
hist(diff)

#DUE TO UNEQUAL SAMPLE SIZE IN TWO GROUP, TEST FOR EQUAL VARIANCE IN THE 2 GROUPS
#OUTCOME: SUGGEST THE VARIANCE IS SIGNIFICANTLY DIFFERENT IN BOTH SAMPLES by outcome and by age/gender
res =  bartlett.test(prehospital_respiratory_rate_tr18_70 ~ ampt_score_Outcome, data = feature_set1)
res
res =  bartlett.test(prehospital_respiratory_rate_tr18_70 ~ interaction(AgeBin, gender), data = feature_set1)
res

#SINCE VARIANCE OCCURS IN THE SAMPLE, USE WELCH'S T-TEST
#Outcome: There is significant difference in sbp for the two groups that need helicopter and that do not
t.test(prehospital_respiratory_rate_tr18_70 ~ ampt_score_Outcome, data = feature_set1, var.equal=FALSE)


#-----------------------------------TEST 5----------------------------------------#
#NULL HYP: There is no difference in pulse rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF pulse RATE ALONE
# HISTOGRAM DOES NOT SHOW NORMAL DISTRIBUTION. QQPLOT DOES NOT SHOW NORMAL DISTRIBUTION.
# USE ANOVA TEST 
# SUMMARY STATS OF RESP RATE:  Median = 88, Mean = 89.45, NA = 20735, Max =273

hist(feature_set1$prehospital_pulse_rate_tr18_69)
ggqqplot(feature_set1$prehospital_pulse_rate_tr18_69)

summary(feature_set1$prehospital_pulse_rate_tr18_69)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_pulse_rate_tr18_69", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "PULSE RATE", xlab = "Outcome")

#OUTLIER OBSERVED IN BOX PLOT. The outlier is 120. HENCE SET OUTLIER AS NA
outlier(feature_set1$prehospital_pulse_rate_tr18_69)
test1=feature_set1 %>% 
  filter(!prehospital_pulse_rate_tr18_69 == 273 |
           is.na(prehospital_pulse_rate_tr18_69) )
test2 = feature_set1%>%
  filter(prehospital_pulse_rate_tr18_69 == 273) %>%
  mutate(prehospital_pulse_rate_tr18_69 = NA)
feature_set1 = rbind(test1, test2)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_pulse_rate_tr18_69", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "RESP RATE", xlab = "Outcome")

#ANALYSIS OF PAIRED TEST
#COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF SBP IN THE 2 GROUPS
#THE HISTOGRAM SHOWS NORMAL DISTRIBUTION
diff <- with(feature_set1, 
             prehospital_pulse_rate_tr18_69[ampt_score_Outcome == "N"] - 
               prehospital_pulse_rate_tr18_69[ampt_score_Outcome == "Y"])
hist(diff)

#DUE TO UNEQUAL SAMPLE SIZE IN TWO GROUP, TEST FOR EQUAL VARIANCE IN THE 2 GROUPS
#OUTCOME: SUGGEST THE VARIANCE IS NOT SIGNIFICANTLY DIFFERENT IN SAMPLES by outcome but by gender
res =  bartlett.test(prehospital_pulse_rate_tr18_69 ~ ampt_score_Outcome, data = feature_set1)
res
res =  bartlett.test(prehospital_pulse_rate_tr18_69 ~ interaction(AgeBin, gender), data = feature_set1)
res

#SINCE VARIANCE OCCURS IN THE SAMPLE, USE WELCH'S T-TEST
#Outcome: There is significant difference in sbp for the two groups that need helicopter and that do not
t.test(prehospital_pulse_rate_tr18_69 ~ ampt_score_Outcome, data = feature_set1, var.equal=TRUE)

#-----------------------------------TEST 6----------------------------------------#
#NULL HYP: There is no difference in oximetry rate for patients that need a helicopter
#and those that do not using the ampt score
#RESULT: THERE IS SIGNIFICANT DIFF IN SBP FOR THE 2 GROUPS
#---------------------------------------------------------------------------------#
# ANALYSIS OF OXIMETRY RATE ALONE
# HISTOGRAM DOES NOT SHOW NORMAL DISTRIBUTION. QQPLOT DOES NOT SHOW NORMAL DISTRIBUTION.
# USE ANOVA TEST 
# SUMMARY STATS OF RESP RATE:  Median = 97, Mean = 95.82, NA = 22863, Max =100

hist(feature_set1$prehospital_pulse_oximetry_tr18_82)
ggqqplot(feature_set1$prehospital_pulse_oximetry_tr18_82)

summary(feature_set1$prehospital_pulse_oximetry_tr18_82)

ggboxplot(feature_set1, x = "ampt_score_Outcome", y = "prehospital_pulse_oximetry_tr18_82", 
          color = "ampt_score_Outcome", palette = c("red", "blue"),
          order = c("Y", "N"),
          ylab = "OXIMETRY RATE", xlab = "Outcome")

#OUTLIER OBSERVED IN BOX PLOT. The outlier is 0. THE MAJORITY ARE OBSERVED TO BE DECEASED IN ED ACUTE CARE
#HENCE 0 IS REASONABLE
outlier(feature_set1$prehospital_pulse_oximetry_tr18_82)

test2 = feature_set1%>%
  filter(prehospital_pulse_oximetry_tr18_82 == 0) %>%
  mutate(prehospital_pulse_oximetry_tr18_82 = NA)


#ANALYSIS OF PAIRED TEST
#COMPUTE DIFFERENCE OF TWO GROUPS AND TEST NORMALITY OF SBP IN THE 2 GROUPS
#THE HISTOGRAM SHOWS NORMAL DISTRIBUTION
diff <- with(feature_set1, 
             prehospital_pulse_oximetry_tr18_82[ampt_score_Outcome == "N"] - 
               prehospital_pulse_oximetry_tr18_82[ampt_score_Outcome == "Y"])
hist(diff)

#DUE TO UNEQUAL SAMPLE SIZE IN TWO GROUP, TEST FOR EQUAL VARIANCE IN THE 2 GROUPS
#OUTCOME: SUGGEST THE VARIANCE IS NOT SIGNIFICANTLY DIFFERENT IN SAMPLES by outcome but by gender
res =  bartlett.test(prehospital_pulse_oximetry_tr18_82 ~ ampt_score_Outcome, data = feature_set1)
res
res =  bartlett.test(prehospital_pulse_oximetry_tr18_82 ~ interaction(AgeBin, gender), data = feature_set1)
res

#SINCE VARIANCE OCCURS IN THE SAMPLE, USE WELCH'S T-TEST
#Outcome: There is significant difference in sbp for the two groups that need helicopter and that do not
t.test(prehospital_pulse_oximetry_tr18_82 ~ ampt_score_Outcome, data = feature_set1, var.equal=FALSE)



#---------------------------------------TEST 7 -----------------------------------#
#NULL HYP: There is no difference in the gcs that need helicopter
#CHI SQUARE USING CONTINGENCY TABLE
# There is significant difference in the need for helicopter for each gender
#---------------------------------------------------------------------------------#
dt_table=table(feature_set1$gcs_eye, feature_set1$ampt_score_Outcome)
dt_table

chisq=chisq.test(dt_table)
chisq

library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib, is.cor = FALSE)

#---------------------------------------TEST 8 -----------------------------------#
#NULL HYP: There is no difference in the gcs verbal that need helicopter
#CHI SQUARE USING CONTINGENCY TABLE
# There is significant difference in the need for helicopter for each gender
#---------------------------------------------------------------------------------#
dt_table=table(feature_set1$gcs_verbal, feature_set1$ampt_score_Outcome)
dt_table

chisq=chisq.test(dt_table)
chisq

library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib, is.cor = FALSE)

#---------------------------------------TEST 9 -----------------------------------#
#NULL HYP: There is no difference in the gcs motor that need helicopter
#CHI SQUARE USING CONTINGENCY TABLE
# There is significant difference in the need for helicopter for each gender
#---------------------------------------------------------------------------------#
dt_table=table(feature_set1$gcs_motor, feature_set1$ampt_score_Outcome)
dt_table

chisq=chisq.test(dt_table)
chisq

library(corrplot)
contrib <- 100*chisq$residuals^2/chisq$statistic
corrplot(contrib, is.cor = FALSE)

