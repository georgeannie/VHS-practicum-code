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

#REMOVE 
feature_set2 = feature_set2 %>%
  filter((!ems_service_name_tr7_3_y == 'NOT KNOWN/NOT RECORDED' &
            !is.na(drive_dist_from_ems) &
            !is.na(drive_dist_to_facility)) &
           !is.na(days_in_hospital))

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
response = feature_set2 %>%
  select(Outcome)

fac_list=feature_set2 %>%
  select(transport , AgeBin, vehicle_accident,
         drug_use, alcohol_use,
         rural_ind, gcs_eye, gcs_motor, gcs_verbal,
         gcs_eye_num, gcs_motor_num, gcs_verbal_num,
         gcs_total_manual,  gcs_total_manual_imp, 
         age, gender, ethinicity,
         hospital_discharge_disp, 
         ed_discharge_disp, 
         method_of_payment, 
         ems_unit_notified_time_of_day, 
         ems_notify_time_hour, trauma_level, years
  )

num_list = feature_set2 %>%
  select(decimal_years, pre_oxi, pre_resp,
         pre_sbp, pre_pulse, diff_from_normal_resp, diff_from_normal_pulse,
         diff_from_normal_sbp, diff_in_oximetry, drive_dist_from_ems, drive_dist_to_facility)

new_feature=cbind(num_list, fac_list, response)         

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

predictors=new_feature[, -c(36)]
response = feature_set1[, c(36)]


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
marginplot(new_feature[, c("pre_sbp", "gcs_eye")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "gcs_verbal")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(new_feature[, c("pre_sbp", "gcs_motor")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#--------------------------------------PRODUCTION CHANGE ----------------------------------------#
#change m and maxit in production to avoid bias due to simulation, The higher number of iterations
#the better
#------------------------------------------------------------------------------------------------#
#------------------------------------IMPUTATION -------------------------------------------------#
# RUN A DRY RUN WITHOUT ITERATIONS TO GET THE PREDICTION MATRIX AND MARK THE IMPORTANT VARIABLES 
# TO BE USED FOR IMPUTATION OF THE MISSING VARIABLES - IMPORTANT VARIABLES CONSIDERED ARE AGE,
# GENDER, VEHICLE ACCIDENT, DRUG, ALCOHOL, ED DISPOSITION                 
#-----------------------------------------------------------------------------------------------#
dry_run = mice(new_feature, maxit=0, print=FALSE)
pred=dry_run$predictorMatrix
pred[,c('method_of_payment', 'drive_dist_from_ems', 'drive_dist_to_facility', 'ethinicity',
                   'ems_notify_time_hour', 'ems_unit_notified_time_of_day', 'trauma_level',
                   'hospital_discharge_disp', 'rural_ind', 'diff_from_normal_resp', 'transport',
        'diff_from_normal_pulse', 'diff_in_oximetry', 'diff_from_normal_sbp', 'decimal_years')] = 0
pred

meth=dry_run$meth
meth['ems_notify_time_hour'] = ""
meth['ems_unit_notified_time_of_day'] = ""
meth['trauma_level']= ""
meth['ethinicity'] = ""
meth['hospital_discharge_disp'] = ""
meth['rural_ind'] = "" 
meth['method_of_payment'] = "" 
meth['drive_dist_from_ems'] =  ""
meth['drive_dist_to_facility'] = ""
meth['hospital_discharge_disp'] = ""  
meth['gcs_total_manual']  = ""

library(nnet)
impData=mice(new_feature , seed=500, 
             print=TRUE, maxit = 1, m=1, pred=pred, meth=meth, nnet.MaxNWts =4000)
summary(impData)

plot(impData, c('pre_oxi', 'pre_resp', 'pre_sbp', 'pre_pulse'))

#predictors used to impute the missing features
# pulse rate
# sbp
# pulse oximetry
# resp rate
# gcs eye
# gcs verbal
# gcs motor

#check if within range
impData$imp$pre_sbp
impData$imp$gcs_eye
impData$imp$gcs_motor
impData$imp$gcs_verbal

#check how different the numerical imputed data are
#stripplot(impData, pch = 20, cex = 1.2)

#densityplot(impData)
xyplot(impData, pre_oxi ~ pre_sbp | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_pulse | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ pre_resp | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_eye | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_verbal | .imp, pch = 20, cex = 1.4)
xyplot(impData, pre_oxi ~ gcs_motor | .imp, pch = 20, cex = 1.4)

final_imp_data=complete(impData, "long", inc=TRUE) 
write.csv(final_imp_data, "/home/trauma/imputed_data.csv")

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
Reduce("+",pool_mean_pulse)/length(pool_mean_pulse)

source("C:/Users/User/Documents/VHS-practicum-code/function_imputation_mice.R")
final_imp_data=sep_gcs(final_imp_data, gcs_eye, gcs_motor, gcs_verbal)

# compute mean and standard deviation in each imputed dataset - gcs-eye
pool_mean_gcs_eye = with(final_imp_data, by(final_imp_data, .imp, 
                      function(x) c(mean(x$gcs_eye_imp, na.rm = TRUE),sd(x$gcs_eye_imp, na.rm = TRUE))))
pool_mean_gcs_eye
Reduce("+",pool_mean_gcs_eye)/length(pool_mean_gcs_eye)

# compute mean and standard deviation in each imputed dataset - gcs-verbal
pool_mean_gcs_verbal = with(final_imp_data, by(final_imp_data, .imp, 
                    function(x) c(mean(x$gcs_verbal_imp, na.rm = TRUE),sd(x$gcs_verbal_imp, na.rm = TRUE))))
pool_mean_gcs_verbal
Reduce("+",pool_mean_gcs_verbal)/length(pool_mean_gcs_verbal)

# compute mean and standard deviation in each imputed dataset - gcs-motor
pool_mean_gcs_motor = with(final_imp_data, by(final_imp_data, .imp, 
                    function(x) c(mean(x$gcs_motor_imp, na.rm = TRUE),sd(x$gcs_motor_imp, na.rm = TRUE))))
pool_mean_gcs_motor
Reduce("+",pool_mean_gcs_motor)/length(pool_mean_gcs_motor)


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

#POOL CHI SQUARE TEST ON GCS EYE
final_imp_data$gcs_eye_imp = as.factor(final_imp_data$gcs_eye_imp)

gcs_eye1 = final_imp_data %>%
  filter(.imp == 1)
dt_table=table(gcs_eye1$gcs_eye_imp, gcs_eye1$Outcome)
dt_table

chisq1=chisq.test(dt_table)

gcs_eye2 = final_imp_data %>%
  filter(.imp == 2)
dt_table=table(gcs_eye12gcs_eye_imp, gcs_eye2$Outcome)
dt_table

chisq2=chisq.test(dt_table)

library(miceadds)
micombine.chisquare(c(chisq1$statistic, chisq2$statistic), 2, display = TRUE, version=1)











levels(feature_set1$financial_primary_method_of_payment_tr2_5) = c(levels(feature_set1$financial_primary_method_of_payment_tr2_5), 
                                                                   "None")
feature_set1$financial_primary_method_of_payment_tr2_5[is.na(feature_set1$financial_primary_method_of_payment_tr2_5)] = "None"

#Number of Outcomes Y/N
table(feature_set1$Outcome)

#Proportion of outcome - 92:8 - highly imbalanced
prop.table(table(feature_set1$Outcome))


#----------------------------------------------------------------------------------#
#                 CREATE NEW FEATURE SET WITH SELECTED COLUMNS                     #
#----------------------------------------------------------------------------------#
select_feature= feature_set1 %>%
  select(incident_id, vehicle_accident, rural_ind, AgeBin, 
         alcohol_intervention_alcohol_screen_tr18_46, drug_use_indicator_tr18_45, days_in_hospital, 
         decimal_years, diff_from_normal_pulse, diff_from_normal_resp, diff_from_normal_sbp, 
         respinrange, sbpinrange, pulseinrange, oximetryinrange, diff_in_oximetry, 
         drive_dist_from_ems, drive_dist_to_facility, ed_acute_care_disposition_tr17_27, 
         ed_death_tr27_14, ems_notify_time_hour,  
         ems_unit_notified_time_of_day, financial_primary_method_of_payment_tr2_5, 
         gcs_severity_flag, ground_time_from_ems, ground_time_to_facility, 
         hospital_discharge_disposition_tr25_27, patient_gender_tr1_15, prehospital_gcs_eye_num, 
         prehospital_gcs_motor_num, prehospital_gcs_verbal_num, 
         prehospital_gcs_total_manual_tr18_64_imp, transport_to_your_facility_by_tr8_8, trauma_level, Outcome)


#---------------------------------------------------------------------------------#
#       SEPARATE OUTCOMES INTO MINORITY AND MAJORITY                              #
#---------------------------------------------------------------------------------#
df_minority=select_feature[select_feature$Outcome == 'Y',]
df_majority = select_feature[select_feature$Outcome == 'N',]

#---------------------------------------------------------------------------------#
#       REMOVE 20% OF SAMPLE FROM BOTH CATEGORY AND USE AS TEST SAMPLE ONLY       #
#       NOT HOLD OUT                                                              #
#---------------------------------------------------------------------------------#
test_minority = df_minority %>%
  sample_frac(.2)

test_majority = df_majority %>%
  sample_frac(.2)

df_test = rbind(test_minority, test_majority)
prop.table(table(df_test$Outcome))

#---------------------------------------------------------------------------------#
#       USE REMAINING 80% DATA TO CREATE THE TRAINING AND HOLD OUT SET            #
#---------------------------------------------------------------------------------#
minority = df_minority %>%
  filter(!incident_id %in% test_minority$incident_id)

majority = df_majority %>%
  filter(!incident_id %in% test_majority$incident_id)

comb_set = rbind(minority, majority)

table(comb_set$Outcome)
prop.table(table(comb_set$Outcome))

#---------------------------------------------------------------------------------#
#       SELECT FEATURES FOR MODEL                                                 #
#---------------------------------------------------------------------------------#
comb_set = comb_set %>%
  select(i)
#---------------------------------------------------------------------------------#
#       CREATE TRAINING AND HOLD OUT SET                                          #
#---------------------------------------------------------------------------------#
index = createDataPartition(comb_set$Outcome, p = 0.8, list = FALSE)
train_data = comb_set[index, ]

table(train_data$Outcome)
prop.table(table(train_data$Outcome))

hold_out_data  = comb_set[-index, ]
table(hold_out_data$Outcome)
prop.table(table(hold_out_data$Outcome))

#---------------------------------------------------------------------------------#
#       DUE TO IMBALANCE IN DATASET OUTCOME, SAMPLE THE TRAINING SET FOR EACH     #
#       MODELING TECHNIQUE USED                                                   #
#---------------------------------------------------------------------------------#
library(e1071)
train_select=train_data %>%
  select(-gcs_severity_flag, - trauma_level, -ems_notify_time_hour,
         -ems_unit_notified_time_of_day, -ed_death_tr27_14, -hospital_discharge_disposition_tr25_27,
         -incident_id, -decimal_years)

hold_select=hold_out_data %>%
  select(-gcs_severity_flag, - trauma_level, -ems_notify_time_hour,
         -ems_unit_notified_time_of_day, -ed_death_tr27_14, -hospital_discharge_disposition_tr25_27,
         -incident_id, -decimal_years)

test_select=test_data %>%
  select(-gcs_severity_flag, - trauma_level, -ems_notify_time_hour,
         -ems_unit_notified_time_of_day, -ed_death_tr27_14, -hospital_discharge_disposition_tr25_27,
         -incident_id, -decimal_years)

#-----------------------------------------------------------------
#LOGISTIC REGRESSION WITH ALL VARIABLES AND NO SAMPLING
#-----------------------------------------------------------------

library((RANN))
preProc_values = preProcess(train_select, method=c('knnImpute', 'center', 'scale'))
preprocess_traing = predict(preProc_values, train_select)
sum(is.na(preprocess_traing))


preprocess_traing$Outcome<-ifelse(preprocess_traing$Outcome=='N',0,1)
id = preprocess_traing$incident_id
preprocess_traing$incident_id = NULL
str(preprocess_traing)

dummy = dummyVars( ~ .  , data = preprocess_traing, fullRank = T)
train_dummy =  data.frame(predict(dummy, newdata = preprocess_traing))


control = trainControl(method = "repeatedcv",
                       number = 10, 
                       repeats = 2, savePredictions = TRUE
)


fit1 = train(Outcome ~. , 
             data = train_dummy, 
             method = "glm",
             trControl= control)

print("Results for Model1")
fit1$results
print("AIC model1")
fit1$finalModel$aic

pred_log1 = predict(fit1, hold_select, type = "prob")
cm = table(hold_select$Outcome, pred_log1)

cm
fit2 = train(Outcome ~. , 
             data = train_select, 
             method = "treebag", metric = metric, 
             trControl= control)

pred_treebag_1 = predict(fit2, newdata = hold_select)
cm2 = table(hold_select$Outcome, pred_treebag_train)
paste('The accuracy for test set is ' , my_accuracy(cm,dim(binned_test_set)[1]))
paste('The accuracy for training set is ' , my_accuracy(cm2,dim(binned_training_set)[1]))
smoted_data <- SMOTE(Outcome~., select_feature, perc.under=200)


library(ROSE)
feature_set = feature_set %>%
  mutate(Outcome_cat=case_when(Outcome == 'Y' ~ 1,
                               TRUE ~ 2),
         Outcome_cat = factor(Outcome_cat))
prop.table(table(feature_set$Outcome_cat))

set.seed(42)
index = createDataPartition(feature_set$Outcome, p = 0.01, list = FALSE)
train_data = feature_set[index, ]
test_data  = feature_set[-index, ]

smoted_data <- SMOTE(Outcome_cat~., train_data, perc.under=200)

smoteest = list(name = "SMOTE my way",
                func = function(x, y){
                  library(DMwR)
                  dat = if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y = y
                  dat = SMOTE(.y ~., data = dat, k = 5, perc.over = 600, perc.under = 1000)
                  list(x = dat[,!grepl(".y", colnames(dat),fixed = TRUE)],
                       y = dat$.y)
                },
                first = TRUE)


model_rf = caret::train(Outcome ~ .,
                        data = train_data,
                        method = "rf",
                        preProcess = c("scale", "center"),
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 3, 
                                                 repeats = 2, 
                                                 verboseIter = FALSE,
                                                 na.action=na.omit))
final <- data.frame(actual = test_data$classes,
                    predict(model_rf, newdata = test_data, type = "prob"))
final$predict <- ifelse(final$benign > 0.5, "benign", "malignant")
cm_original <- confusionMatrix(final$predict, test_data$classes
                               ctrl <- trainControl(method = "repeatedcv", 
                                                    number = 3, 
                                                    repeats = 1, 
                                                    verboseIter = FALSE,
                                                    sampling = "down")
                               
                               set.seed(42)
                               model_rf_rose = train(Outcome ~ .,
                                                     data = feature_set,
                                                     method = "rf",
                                                     preProcess = c("scale", "center"),
                                                     trControl = ctrl)
                               
                               final_rose <- data.frame(actual = test_data$classes,
                                                        predict(model_rf_rose, newdata = test_data, type = "prob"))
                               final_rose$predict <- ifelse(final_rose$benign > 0.5, "benign", "malignant")
                               cm_rose <- confusionMatrix(final_rose$predict, test_data$classes)
                               