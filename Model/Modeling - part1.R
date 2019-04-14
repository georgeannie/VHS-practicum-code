library('RPostgreSQL')
library(caret)
library(DMwR)
library(e1071)
library((RANN))
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
imp_set1=dbGetQuery(con, 'select * from imputed_feature1')
imp_set2=dbGetQuery(con, 'select * from imputed_feature2')

feature_set1=imp_set1

#-----------------------------------------------------------------------#
# REMOVE EMPTY drive distance AND TIME OF DAY
#-----------------------------------------------------------------------#

feature_set1 = feature_set1 %>%
  filter((!is.na(drive_dist_to_facility)) &
           !is.na(ems_notify_time_hour) &
           !is.na(ems_unit_notified_time_of_day))           

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
          prehospital_gcs_total_manual_tr18_64_imp = as.factor(prehospital_gcs_total_manual_tr18_64_imp), 
          prehospital_gcs_motor_num = as.factor(prehospital_gcs_motor_num), 
          prehospital_gcs_verbal_num = as.factor(prehospital_gcs_verbal_num),
          patient_age_units_tr1_14 = as.factor(patient_age_units_tr1_14), 
          gender = as.factor(gender), 
          ethinicity = as.factor(ethinicity),
          hospital_discharge_disp = as.factor(hospital_discharge_disp), 
          ed_discharge_disp = as.factor(ed_discharge_disp), 
          gcs_severity_flag = as.factor(gcs_severity_flag),
          gcs_total_match_flag = as.factor(gcs_total_match_flag), 
          financial_pay_method = as.factor(financial_pay_method), 
          ems_unit_notified_time_of_day = as.factor(ems_unit_notified_time_of_day), 
          ems_notify_time_hour = as.factor(ems_notify_time_hour), 
          ed_death = as.factor(ed_death),
          trauma_level = as.factor(trauma_level),
          years=as.factor(years))

#-----------------------------------------------------------------------#
#Number of Outcomes Y/N AND THE PROPORTION
# 92:8 -  IMBALANCED DATASET
#-----------------------------------------------------------------------#

table(feature_set1$Outcome)

prop.table(table(feature_set1$Outcome))

#---------------------------------------------------------------------------------#
#       DUE TO IMBALANCE IN DATASET OUTCOME, SAMPLE THE TRAINING SET FOR EACH     #
#       MODELING TECHNIQUE USED. CREATE 3 SAMPLE SETS - UPSAMPLE, DOWNSAMPLE,     #
#       SMOTE                                                                     #
#---------------------------------------------------------------------------------#
train_select=train_data %>%
  select(- trauma_level,  -ed_death_tr27_14, -hospital_discharge_disposition_tr25_27,
         -incident_id, -decimal_years, -financial_primary_method_of_payment_tr2_5, -years, 
        -patient_age_units_tr1_14, -patient_age_tr1_12, -ampt_score_Outcome)


#-----------------------------------------------------------------
#   CENTER AND SCALE THE TRAINING SET AND SET OUTCOME AS 0/1
#-----------------------------------------------------------------

preProc_values = preProcess(train_select, method=c('center', 'scale'))
preprocess_traing = predict(preProc_values, train_select)

preprocess_traing$Outcome = as.factor(ifelse(preprocess_traing$Outcome=='N',0,1))
#-----------------------------------------------------------------
#   CREATE DUMMY VARIABLES
#-----------------------------------------------------------------

dummy = dummyVars( ~ .  , data = preprocess_traing, fullRank = T)
train_dummy =  data.frame(predict(dummy, newdata = preprocess_traing))

#---------------------------------------------------------------------------------#
#       CREATE TRAINING AND TEST SET. IT SHOULD HAVE SAME PROP OF OUTCOME         #
#---------------------------------------------------------------------------------#
index = createDataPartition(train_dummy$Outcome, p = 0.8, list = FALSE)
train_data = feature_set1[index, ]

table(train_data$Outcome)
prop.table(table(train_data$Outcome))

test_data  = train_dummy[-index, ]
table(test_data$Outcome)
prop.table(table(test_data$Outcome))

#-----------------------------------------------------------------
#   DEFINE OUTCOME AND PREDICTORS
#-----------------------------------------------------------------------#

outcome = train_data %>%
  select(Outcome)

predictors = names(train_data)[!names(train_data) %in% 'Outcome']

#-----------------------------------------------------------------
#   CREATE CROSS VALIDATION/HOLD OUT SET (CHANGE IN PROD)
#-----------------------------------------------------------------

control = trainControl(method = "repeatedcv",
                       number = 2, 
                       repeats = 2, savePredictions = TRUE
)

#-----------------------------------------------------------------
#     FEATURE SELECTION (RFE)
#-----------------------------------------------------------------


#-----------------------------------------------------------------
#   NO SAMPLE SET
#-----------------------------------------------------------------

#1. Logistic regression
fit1_no_sample = train(train_data[,predictors], train_data[, outcome],
             method = "glm",
             trControl= control, tuneLength = 20)

print("Results for Model - NO SAMPLE SET")
fit1_np_sample$results

print("AIC model - NO SAMPLE SET")
fit1_no_sample$finalModel$aic

plot(fit1_no_sample)

plot(varImp(object = fit1_no_sample), main="GLM without sampling")

predict1 = predict.train(object=fit1_no_sample, testdata[,predictors], type = "prob")
print('CM with glm - no sample')
table(testdata$Outcome, predict1)


#1. DECISION TREE
metric="ROC"
fit2_no_sample = train(train_data[,predictors], train_data[, outcome], 
             method = "treebag", metric = metric, 
             trControl= control)

print('CM with decision tree - no sample')
pred_treebag_1 = predict(fit2_no_sample, newdata = testdata)
table(test_data$Outcome, pred_treebag_1)

plot(varImp(object = fit2_no_sample), main="Decision Tree without sampling")

paste('The accuracy for test set is ' , my_accuracy(cm,dim(binned_test_set)[1]))
paste('The accuracy for training set is ' , my_accuracy(cm2,dim(binned_training_set)[1]))

#-----------------------------------------------------------------
#   PREPARE UPSAMPLE
#-----------------------------------------------------------------
set.seed(9560)
up_train = upSample(x = train_data[, predictors],
                        y = train_data[, outcome])
table(up_train$Class)   

#-----------------------------------------------------------------
#   PREPARE DOWNSAMPLE
#-----------------------------------------------------------------
set.seed(9560)
down_train = downSample(x = train_data[, predictors],
                        y = train_data[, outcome])
table(down_train$Class)   

#-----------------------------------------------------------------
#   PREPARE SMOTE SAMPLE
#-----------------------------------------------------------------
et.seed(9560)
smote_train = SMOTE(Outcome ~ ., data  = train_data)                         
table(smote_train$Class) 

#-----------------------------------------------------------------
#   PREPARE ROSE  SAMPLE
#-----------------------------------------------------------------

library(ROSE)
set.seed(9560)
rose_train <- ROSE(Outcome ~ ., data  = train_data)$data                         
table(rose_train$Class)

#------------------LOGISTIC ----------------------------#


#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                    shrinkage=c(0.01,0.05,0.1,0.5),
                    n.minobsinnode = c(3,5,10),
                    interaction.depth=c(1,5,10))


#-----------------------------------------------------------------
#   CREATE HOLD OUT SET USING CARET
#-----------------------------------------------------------------

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
