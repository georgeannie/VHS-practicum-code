library('RPostgreSQL')
library(caret)
library(DMwR)
library(e1071)
library((RANN))
library(dplyr)
library(rpart)
library(randomForest)

set.seed(9560)


source("C:/Users/User/Documents/VHS-practicum-code/function_aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#----------------------READ ALL ROWS FROM CLEAN_TRAUMA TABLE --------------#
imp_set1=dbGetQuery(con, 'select * from imputed_feature1')
imp_set2=dbGetQuery(con, 'select * from imputed_feature2')

feature_set1=imp_set1

#-----------------------------------------------------------------------#
# REMOVE EMPTY drive distance AND TIME OF DAY
#-----------------------------------------------------------------------#

feature_set1 = feature_set1 %>%
  filter((!is.na(drive_dist_to_facility)) &
           !is.na(ems_notify_time_hour) &
           !is.na(ems_unit_notified_time_of_day) &
           !is.na(pre_pulse)) 


#-----------------------------------------------------------------------#
# CONVERT CATEGORICAL TO FACTORS
#-----------------------------------------------------------------------#

feature_set1 = feature_set1 %>%
  mutate( Outcome = as.factor(Outcome),
          ampt_score_Outcome = as.factor(ampt_score_Outcome),
          transport = as.factor(transport), 
          AgeBin = as.factor(AgeBin), 
          vehicle_accident = as.factor(vehicle_accident), 
          drug_use = as.factor(drug_use),
          alcohol_use = as.factor(alcohol_use),
          rural_ind = as.factor(rural_ind), 
          sbpinrange = as.factor(sbpinrange), 
          pulseinrange = as.factor(pulseinrange), 
          respinrange = as.factor(respinrange), 
          oximetryinrange = as.factor(oximetryinrange), 
          patient_age_units_tr1_14 = as.factor(patient_age_units_tr1_14), 
          gender = as.factor(gender), 
          hospital_discharge_disp = as.factor(hospital_discharge_disp), 
          ed_discharge_disp = as.factor(ed_discharge_disp), 
          gcs_severity_flag = as.factor(gcs_severity_flag),
          gcs_total_match_flag = as.factor(gcs_total_match_flag), 
          method_of_payment = as.factor(method_of_payment), 
          ems_unit_notified_time_of_day = as.factor(ems_unit_notified_time_of_day), 
          ed_death = as.factor(ed_death),
          trauma_level = as.factor(trauma_level),
          years=as.factor(years),
          gcs_eye_num = as.factor(gcs_eye_num),
          gcs_motor_num  = as.factor(gcs_motor_num),
          gcs_verbal_num = as.factor(gcs_verbal_num),
          gcs_total_manual_imp = as.factor(gcs_total_manual_imp),
          ems_notify_time_hour = as.factor(ems_notify_time_hour))

#-----------------------------------------------------------------------#
#Number of Outcomes Y/N AND THE PROPORTION
# 92:8 -  IMBALANCED DATASET
#-----------------------------------------------------------------------#

table(feature_set1$ampt_score_Outcome)

prop.table(table(feature_set1$ampt_score_Outcome))

#---------------------------------------------------------------------------------#
#       DUE TO IMBALANCE IN DATASET OUTCOME, SAMPLE THE TRAINING SET FOR EACH     #
#       MODELING TECHNIQUE USED. CREATE 3 SAMPLE SETS - UPSAMPLE, DOWNSAMPLE,     #
#       SMOTE                                                                     #
#---------------------------------------------------------------------------------#
train_select=feature_set1 %>%
  select(- trauma_level,  -ed_death, -hospital_discharge_disp,
         -incident_id, -decimal_years, -method_of_payment, -years, 
         -patient_age_units_tr1_14, -age, -Outcome, -ems_service_name_tr7_3_y,
         -ed_discharge_disp, -ground_time_from_ems, -ground_time_to_facility, -drive_dist_from_ems,
         -drive_dist_to_facility, -incident_date_tr5_1, -incident_time_tr5_18,
         -gcs_severity_flag, -gcs_total_match_flag, -rural_ind, -drug_use, -alcohol_use,
          -transport) ##----> remove later

#---------------------------------------------------------------------------------#
#       CREATE TRAINING AND TEST SET. IT SHOULD HAVE SAME PROP OF OUTCOME         #
#---------------------------------------------------------------------------------#
index = createDataPartition(train_select$ampt_score_Outcome, p = 0.8, list = FALSE)
train_data = train_select[index, ]

table(train_data$ampt_score_Outcome)
prop.table(table(train_data$ampt_score_Outcome))

test_data  = train_select[-index, ]
table(test_data$ampt_score_Outcome)
prop.table(table(test_data$ampt_score_Outcome))

x = train_data[, !names(train_data) %in% c("ampt_score_Outcome")]
y = train_data$ampt_score_Outcome

library(skimr)
skimmed <- skim_to_wide(train_data)
skimmed
#-----------------------------------------------------------------
#   CENTER AND SCALE THE TRAINING SET AND SET OUTCOME AS 0/1
#-----------------------------------------------------------------

preProc_values = preProcess(train_data, method=c('center', 'scale', 'nzv'))
preprocess_traing = predict(preProc_values, train_data)
anyNA(preprocess_traing)


preProc_values = preProcess(test_data, method=c('center', 'scale', 'nzv'))
preprocess_test = predict(preProc_values, test_data)

#-----------------------------------------------------------------
#   CREATE DUMMY VARIABLES
#-----------------------------------------------------------------
preprocess_traing$ampt_score_Outcome = as.factor(preprocess_traing$ampt_score_Outcome)
dummy = dummyVars(ampt_score_Outcome ~ .  , data = preprocess_traing)
train_dummy =  data.frame(predict(dummy, newdata = preprocess_traing))

train_dummy$ampt_score = y

preprocess_test$ampt_score_Outcome = as.factor(preprocess_test$ampt_score_Outcome)
dummy = dummyVars(ampt_score_Outcome ~ .  , data = preprocess_test)
test_dummy =  data.frame(predict(dummy, newdata = preprocess_test))

#-----------------------------------------------------------------
#     FEATURE SELECTION (RFE)
#-----------------------------------------------------------------

featurePlot(x = train_dummy[, 1:79], 
            y = train_dummy$ampt_score, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

set.seed(100)
options(warn=-1)

ctrl <- rfeControl(functions = rfFuncs,
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

lmProfile <- rfe(x=train_dummy[1:83], y=train_dummy$ampt_score,
                     rfeControl = ctrl)

lmProfile

#-----------------------------------------------------------------
#   CREATE CROSS VALIDATION/HOLD OUT SET (CHANGE IN PROD)
#-----------------------------------------------------------------

control = trainControl(method = "repeatedcv",
                       number = 3, 
                       repeats = 5, 
                       savePredictions = TRUE,       # saves predictions for optimal tuning parameter
                       classProbs = T,                  # should class probabilities be returned
                       summaryFunction=twoClassSummary 
)


#-----------------------------------------------------------------
#1. Logistic regression
#---------------------------------------------------------------------
fit1_log_reg = train(ampt_score ~., data = train_dummy,
                     method = "glm", family=binomial(), metric="ROC",
                     trControl= control, tuneLength = 20)

print("Results for Model - NO SAMPLE SET")
fit1_log_reg$results

print("AIC model - NO SAMPLE SET")
fit1_log_reg$finalModel$aic

plot(varImp(object = fit1_log_reg), main="GLM of over utilization", top=20)

predict_log_prob = predict(object=fit1_log_reg, test_dummy, type = "prob")

predict_log = predict(object=fit1_log_reg, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_log, mode='everything', positive='Y')

saveRDS(fit1_log_reg, "fit_log_reg.rds")

#---------------------------------------------------------------------
#. Random Forest - ranger
#---------------------------------------------------------------------
fit1_ranger = train(ampt_score ~., data = train_dummy,
                method = "ranger",  metric="ROC", importance="permutation",
                trControl= control, tuneLength = 20)

print("Results for Model - NO SAMPLE SET")
fit1_ranger$results


plot(varImp(object = fit1_ranger, scale=FALSE), main="RF of over utilization", top=20)

predict_ranger_prob = predict(object=fit1_ranger, test_dummy, type = "prob")

predict_ranger = predict(object=fit1_ranger, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_ranger, mode='everything', positive='Y')

saveRDS(fit1_ranger, "fit_ranger.rds")

#--------------------------------ranger 2 -----------------------------------
ranger.grid = expand.grid(mtry=c(1:5, 8, 10, 12, 15), splitrule='gini', min.node.size=c(1:7))

fit2_ranger = train(ampt_score ~., data = train_dummy,
                method = "ranger",  metric="ROC", importance="permutation",
                trControl= control, tuneLength = 10, tuneGrid=ranger.grid)

fit2_ranger$results


plot(varImp(object = fit2_ranger, scale=FALSE), main="RF of over utilization", top=20)

predict_ranger2_prob = predict(object=fit2_ranger, test_dummy, type = "prob")

predict_ranger2 = predict(object=fit2_ranger, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_ranger2, mode='everything', positive='Y')

saveRDS(fit2_ranger, "fit2_ranger.rds")

#---------------------------------------------------------------------
#. Random Forest - rf
#---------------------------------------------------------------------
rf.grid = expand.grid(mtry=c(2:10))
fit1_rf = train(ampt_score ~., data = train_dummy,
                method = "rf",  metric="ROC", 
                trControl= control, tuneLength = 20, tuneGrid=rf.grid)

fit1_rf$results


plot(varImp(object = fit1_rf, scale=FALSE), main="RF of over utilization", top=20)

predict_rf_prob = predict(object=fit1_rf, test_dummy, type = "prob")

predict_rf = predict(object=fit1_rf, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_rf, mode='everything', positive='Y')

saveRDS(fit1_rf, "fit_rf.rds")

#---------------------------------------------------------------------
#. Bagging
#---------------------------------------------------------------------
metric="ROC"
fit1_bag = train(ampt_score ~ ., train_dummy, 
                  method = "treebag", metric = metric, 
                  trControl= control, tuneLength=10)

fit1_bag$results

plot(varImp(object = fit1_bag), main="Decision Tree without sampling", top=20)

library(rattle)
plot(fit1_bag)

predict_bag_prob = predict(object=fit1_bag, test_dummy, type = "prob")

predict_bag = predict(object=fit1_bag, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_bag, mode='everything', positive='Y')


#---------------------------------------------------------------------
#. Decision tree
#---------------------------------------------------------------------
metric="ROC"
rpart.grid = expand.grid(cp=seq(0, 0.1, 0.001))
fit1_tree = train(ampt_score ~ ., train_dummy, 
                  method = "rpart", metric = metric, 
                  trControl= control, tuneLength=10, tuneGrid=rpart.grid)

fit1_tree
plot(varImp(object = fit1_tree), main="Decision Tree without sampling", top=20)

library(rattle)
plot(fit1_tree)
summary(fit1_tree)

library(party)
fancyRpartPlot(fit1_tree$finalModel,palettes=c("Greys", "Oranges"), cex=0.8)
rpart.plot::prp(fit1_tree$finalModel)


predict_tree_prob = predict(object=fit1_tree, test_dummy, type = "prob")

predict_tree = predict(object=fit1_tree, test_dummy)
confusionMatrix(reference = test_data$ampt_score_Outcome, 
                data = predict_tree, mode='everything', positive='Y')
