#Cut age in
library('RPostgreSQL')
library(caret)
library(DMwR)
library(e1071)
library(dplyr)
library(rpart)
library(randomForest)
library(ggplot2)
library(stringr)
set.seed(9560)


source("C:/Users/User/Documents/VHS-practicum-code/function_aws_rds_access.R")

pg = dbDriver("PostgreSQL")
con=dbConnect(pg, 
              dbname = "vhs",
              host=host,
              user = user, 
              password = password)

#----------------------READ ALL ROWS FROM IMPUTED TABLE --------------#
imp_set1=dbGetQuery(con, 'select * from imputed_feature1')
imp_set2=dbGetQuery(con, 'select * from imputed_feature2')

#----------------------READ ALL ROWS FROM FEATURE_SET TABLE --------------#
imp_set1=dbGetQuery(con, 'select * from feature_set_trauma')

feature_set1=imp_set1

#-----------------------------------------------------------------------#
# REMOVE EMPTY pre hospitals for imputed data only
#-----------------------------------------------------------------------#

feature_set1 = feature_set1 %>%
  filter(!is.na(pre_pulse) &
           !is.na(pre_sbp) &
           !is.na(pre_resp) &
           !is.na(pre_oxi)) 

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
           gcs_eye_num = as.factor(gcs_eye_num),
           gcs_motor_num  = as.factor(gcs_motor_num),
           gcs_verbal_num = as.factor(gcs_verbal_num),
           gcs_total_manual_imp = as.factor(gcs_total_manual_imp),
           ems_notify_time_hour = as.factor(ems_notify_time_hour))


#-----------------------------------------------------------------------#
# Get only complete cases from feature_set
#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
# CONVERT CATEGORICAL TO FACTORS
#-----------------------------------------------------------------------#

# feature_set1 = feature_set1 %>%
#  mutate( Outcome = as.factor(Outcome),
#          ampt_score_Outcome = as.factor(ampt_score_Outcome),
#          AgeBin = as.factor(AgeBin),
#          vehicle_accident = as.factor(vehicle_accident),
#          patient_age_units_tr1_14 = as.factor(patient_age_units_tr1_14),
#          patient_gender_tr1_15 = as.factor(patient_gender_tr1_15),
#          gcs_severity_flag = as.factor(gcs_severity_flag),
#            prehospital_gcs_eye_num = as.factor(prehospital_gcs_eye_num),
#            prehospital_gcs_motor_num  = as.factor(prehospital_gcs_motor_num),
#            prehospital_gcs_verbal_num = as.factor(prehospital_gcs_verbal_num),
#            prehospital_gcs_total_manual_tr18_64_imp = as.factor(prehospital_gcs_total_manual_tr18_64_imp)
#          )
# 
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_sbp_tr18_67),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_respiratory_rate_tr18_70),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_pulse_rate_tr18_69),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_pulse_oximetry_tr18_82),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_gcs_eye_num),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_gcs_motor_num),]
# feature_set1 = feature_set1[complete.cases(feature_set1$prehospital_gcs_verbal_num),]
# feature_set1 = feature_set1[complete.cases(feature_set1$diff_from_normal_resp),]
# 
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
df.cont = feature_set1[c("ampt_score_Outcome",   "diff_from_normal_sbp",  "diff_from_normal_resp",
                        "diff_from_normal_pulse", "diff_in_oximetry")]   

 df.cat = feature_set1[c("ampt_score_Outcome", "gcs_eye_num", "gcs_motor_num", #"AgeBin",  #"gcs_verbal_num",  
                          "vehicle_accident",  "gender")]


#df.cat = feature_set1[c("ampt_score_Outcome", "prehospital_gcs_eye_num", 
#                        "prehospital_gcs_motor_num",  # "prehospital_gcs_verbal_num", #"AgeBin",  #"gcs_verbal_num",  
#                        "vehicle_accident",  "patient_gender_tr1_15" )]

df.test=cbind(df.cont, df.cat[,-1])

#CUTTING NUMERICALS
library(rattle)
library(tree)
mod=tree(diff_from_normal_pulse ~. ,
          data=df.test) #,  method="rpart")
plot(mod);text(mod)

mod=tree(diff_from_normal_resp ~. ,
         data=df.test) #,  method="rpart")
plot(mod);text(mod)

mod=tree(diff_from_normal_sbp ~. ,
         data=df.test) #,  method="rpart")
plot(mod);text(mod)

mod=tree(diff_in_oximetry ~. ,
         data=df.test) #,  method="rpart")
plot(mod);text(mod)

#-----------------------------------
 library(OneR)
 cont.to.cat=df.test
 cont.to.cat$diff_sbp1= cut(df.cont$diff_from_normal_sbp, breaks = c(-87, 16, 30, 170), include.lowest = T)     #6)
 prop.table(table(cont.to.cat$diff_sbp1, cont.to.cat$ampt_score_Outcome))
 
 cont.to.cat$diff_resp1 = cut(df.cont$diff_from_normal_resp, breaks = c(-19, 1, 3, 81), include.lowest = T)                  # 3)
 prop.table(table(cont.to.cat$diff_resp1, cont.to.cat$ampt_score_Outcome))
 
cont.to.cat$diff_pulse1 = cut(df.cont$diff_from_normal_pulse, 
                                        breaks = c(-98, -21 , 1, 4, 6, 144), include.lowest = T) 
 prop.table(table(cont.to.cat$diff_pulse1, cont.to.cat$ampt_score_Outcome))
 
 cont.to.cat$diff_oxi1= cut(df.cont$diff_in_oximetry, breaks = c(-94, -14, -6, -1, 0), include.lowest = T) 
 (table(cont.to.cat$diff_oxi1, cont.to.cat$ampt_score_Outcome))

# library(OneR)
# cont.to.cat=df.test
# cont.to.cat$diff_sbp1= cut(df.cont$diff_from_normal_sbp, breaks = c(-87, 16, 30, 170), include.lowest = T)     #6)
# prop.table(table(cont.to.cat$diff_sbp1, cont.to.cat$ampt_score_Outcome))
# 
# cont.to.cat$diff_resp1 = cut(df.cont$diff_from_normal_resp, breaks = c(-19, 1, 3, 81), include.lowest = T)                  # 3)
# prop.table(table(cont.to.cat$diff_resp1, cont.to.cat$ampt_score_Outcome))
# 
# cont.to.cat$diff_pulse1 = cut(df.cont$diff_from_normal_pulse, 
#                               breaks = c(-98, 2, 7, 144), include.lowest = T) 
# prop.table(table(cont.to.cat$diff_pulse1, cont.to.cat$ampt_score_Outcome))
# 
# cont.to.cat$diff_oxi1= cut(df.cont$diff_in_oximetry, breaks = c(-94, -5, -1, 0), include.lowest = T) 
# (table(cont.to.cat$diff_oxi1, cont.to.cat$ampt_score_Outcome))
# 
cont.to.cat =  cont.to.cat %>%
  mutate(diff_resp1 = as.factor(diff_resp1),
         diff_sbp1 = as.factor(diff_sbp1),
         diff_pulse1 = as.factor(diff_pulse1),
         diff_oxi1 = as.factor(diff_oxi1))
df.final=cont.to.cat[,-c(2:5)]

#---------------------------------------------------------------------------------#
#       CREATE TRAINING AND TEST SET. IT SHOULD HAVE SAME PROP OF OUTCOME         #
#---------------------------------------------------------------------------------#
index = createDataPartition(df.final$ampt_score_Outcome, p = 0.75, list = FALSE)
train_data = df.final[index, ]

table(train_data$ampt_score_Outcome)
prop.table(table(train_data$ampt_score_Outcome))


test_data  = df.final[-index, ]
table(test_data$ampt_score_Outcome)
prop.table(table(test_data$ampt_score_Outcome))

x = train_data[, !names(train_data) %in% c("ampt_score_Outcome")]
y = train_data$ampt_score_Outcome

train_data$ampt_score_Outcome = as.factor(ifelse (train_data$ampt_score_Outcome == 'Y',1,0))
test_data$ampt_score_Outcome = as.factor(ifelse (test_data$ampt_score_Outcome == 'Y',1,0))


train_data$diff_pulse1 = as.factor(train_data$diff_pulse1)
train_data$diff_oxi1 = as.factor(train_data$diff_oxi1)
train_data$diff_sbp1 = as.factor(train_data$diff_sbp1)
train_data$diff_resp1 = as.factor(train_data$diff_resp1)

#-----------------------------------------------------------------
#   CREATE CROSS VALIDATION/HOLD OUT SET (CHANGE IN PROD)
#-----------------------------------------------------------------

control = trainControl(method = "repeatedcv",
                       number = 2, 
                       repeats = 5, 
                       savePredictions = TRUE,       # saves predictions for optimal tuning parameter
                       classProbs = T,                  # should class probabilities be returned
                       summaryFunction=twoClassSummary 
)

#-------------------------------Logistic Regression ----------------#
#Best model: Accuracy: 61% AUC: 62%
#-------------------------------------------------------------------#
mod_lr=glm(ampt_score_Outcome ~. +vehicle_accident*gender,
        data=train_data, family="binomial", control = list(maxit = 5, epsilon=.001))

summary(mod_lr)

#-----FIND COEFFICIENTS AND CORRESPONDING SCORE
coef=round((exp(mod_lr$coefficients)),0)

#----ADDING SE TO GET THE SCORE
#se=coef(summary(mod_lr))[,2]
#score = round(coef/se,0)      

#-------------SAVE SCORES
score1=as.data.frame(coef)
score1

#----------ANOTHER METHOD OF SCORING
score2_imp=varImp(mod_lr)
score2_imp

#--------PREDICT RESPONSE WITH CUTOFF OF 0.4
library(pROC)
predict1=predict(mod_lr, test_data, type="response")
cutoff_prior = quantile(predict1, 0.4)

#----- Obtain the binary predictions and the accuracy
bin_pred_prior= ifelse(predict1 > cutoff_prior, 1, 0)

accuracy=table(bin_pred_prior, test_data$ampt_score_Outcome)
sum(diag(accuracy))/sum(accuracy)

#------AUC SCORE FOR VALIDITY OF MODEL
roc(test_data$ampt_score_Outcome, predict1)


#----------------------------RANDOM FOREST ---------------------------#
# BEST SCORES: Accuracy: 71% AUc = 54%
#---------------------------------------------------------------------#
#--REMOVE NULLS
train_data = train_data %>% filter(!is.na(diff_resp1))
test_data = test_data %>% filter(!is.na(diff_resp1))

#-----------------------------------------------------------------
#   CREATE DUMMY VARIABLES
#-----------------------------------------------------------------
dummy=dummyVars(~., train_data[,-1], fullRank = T)
train_dummy = as.data.frame(predict(dummy, train_data))
train_dummy=cbind("ampt_score_Outcome"=train_data[,1], train_dummy)

dummy=dummyVars(~., test_data[,-1], fullRank = T)
test_dummy = as.data.frame(predict(dummy, test_data))
test_dummy=cbind("ampt_score_Outcome"=test_data[,1], test_dummy)

#----------MODEL RF USING HYPER PARAMETERS -----------------#
#------- Establish a list of possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(train_dummy) * 0.8, 2)
nodesize = seq(3, 8, 2)
sampsize = nrow(train_dummy) * c(0.7, 0.8)

#----------Create a data frame containing all combinations 
hyper_grid = expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

#---- Create an empty vector to store OOB error values
oob_err = c()

#----------loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
#------------- Train a Random Forest model
  model <- randomForest(y=train_dummy[,1], x=train_dummy[,-1],
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
#-------------- Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

#--------------- Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
print(hyper_grid[opt_i,])

#----RANDOM FOREST USING BEST HYPERPARAMETER FOUND -MTRY = 8, NODESIZE=5

mod_rf=randomForest(y=train_dummy[,1], x=train_dummy[,-1], mtry = 8, nodesize=5,
                    tuneLength=5)
mod_rf

#----TRANSLATE VARIABLE IMPORTANCE TO A SCORE BY USING THE TOTAL NUMBER OF IMP VARIABLES
imp=varImp(mod_rf)
score_rf=round(imp/nrow(imp), 0)
score_rf

library(pROC)

#----PREDICT USING THE RF MODEL
predict1=stats::predict(mod_rf, newdata =  test_dummy[,-1], type="response")

#----FIND ACCURACY OF RF MODEL - 75%
accuracy=table(predict1, test_dummy$ampt_score_Outcome)
sum(diag(accuracy))/sum(accuracy)

#---PREDICT PROBABILITIES FOR RF MODEL
predict1=predict(mod_rf, test_dummy[,-1], type="prob")

#----FIND AUC AND ROC SCORES 50%, 70% cm
library(ROCR)
forestpred=prediction(predict1[,2], test_dummy$ampt_score_Outcome)
forestperf = performance(forestpred, "tpr", "fpr")
plot(forestperf)
forestperf = performance(forestpred, "auc")
auc=unlist(slot(forestperf, "y.values"))
auc


#----------------------------SVM BASIC RUN ONLY---------------------------#
# BEST SCORE: ACCURACY: 70% AUC 50%
#-------------------------------------------------------------------------#
library(e1071)
tune_svm = svm(x=train_dummy[,-1], y=train_dummy[,1]) 
tune_svm

library(pROC)
predict1=predict(tune_svm, test_dummy[,-1])
accuracy=table(predict1, test_dummy$ampt_score_Outcome)
sum(diag(accuracy))/sum(accuracy)

library(MLmetrics)
predict1=predict(tune_svm, test_dummy[,-1], type="prob")
AUC(predict1, test_dummy$ampt_score_Outcome)

#---------MODEL 2 OF SVM
library(rminer)
mod_svm=svm(x=train_dummy[,-1], y=train_dummy[,1], 
            kernel="linear", cost=0.1, gamma=0.025)
predict1=predict(mod_svm, test_dummy[,-1])
accuracy=table(predict1, test_dummy$ampt_score_Outcome)
sum(diag(accuracy))/sum(accuracy)

library(MLmetrics)
predict1=predict(mod_svm, test_dummy[,-1], type="prob")
AUC(predict1, test_dummy$ampt_score_Outcome)

#-------------------------------------------------------------------------------------------#
#   PREPARE DATA TO ADD SCORES OBTAINED FROM THE BEST MODEL ABOVE
#-------------------------------------------------------------------------------------------#
train_data=train_data %>%
  mutate(diff_resp1 = 
           str_c("diff_resp1", diff_resp1),
         diff_pulse1 = 
           str_c("diff_pulse1", diff_pulse1),
         diff_oxi1 = 
           str_c("diff_oxi1", diff_oxi1),
         diff_sbp1 = 
                str_c("diff_sbp1", diff_sbp1),
         prehospital_gcs_eye_num = 
           str_c("prehospital_gcs_eye_num", prehospital_gcs_eye_num),
         prehospital_gcs_motor_num = 
           str_c("prehospital_gcs_motor_num", prehospital_gcs_motor_num)
#         gcs_verbal_num = 
#           str_c("gcs_verbal_num", gcs_verbal_num)
  )


test_data=test_data %>%
  mutate(#diff_from_normal_resp = 
        #   str_c("diff_from_normal_resp", diff_from_normal_resp),
         diff_pulse1 = 
           str_c("'diff_pulse1", diff_pulse1, ","),
         diff_oxi1 = 
           str_c("'diff_oxi1'", diff_oxi1, ","),
         #   diff_from_normal_sbp = 
         #     str_c("diff_from_normal_sbp", diff_from_normal_sbp),
         prehospital_gcs_eye_num = 
           str_c("prehospital_gcs_eye_num", prehospital_gcs_eye_num),
         prehospital_gcs_motor_num = 
           str_c("prehospital_gcs_motor_num", prehospital_gcs_motor_num)
         #gcs_verbal_num = 
          # str_c("gcs_verbal_num", gcs_verbal_num)
  )


#---------------DISCONNECT POSTGRES ------#
dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)

#--------------------------ADD SCORES OBTAINED TO THE RAW DATA -----------#
library(sqldf)
df_final=train_data
df_final=sqldf('select a.*, score as score_resp
               from df_final a
               left join score1 b
               on a.diff_resp1=b.rn')

df_final=sqldf('select a.*, score as score_sbp
               from df_final a
               left join score1 b
               on a.diff_sbp1=b.rn')

df_final=sqldf('select a.*, score as score_pulse
               from df_final a
               left join score1 b
               on a.diff_pulse1=b.rn')

df_final=sqldf('select a.*, score as score_oximetry
               from df_final a
               left join score1 b
               on a.diff_oxi1=b.rn')

df_final=sqldf('select a.*, score as score_gcs_eye
               from df_final a
               left join score1 b
               on a.prehospital_gcs_eye_num=b.rn')

df_final=sqldf('select a.*, score as score_gcs_motor
               from df_final a
               left join score1 b
               on a.prehospital_gcs_motor_num=b.rn')


df_final$score_resp[is.na(df_final$score_resp)] =  0
df_final$score_sbp[is.na(df_final$score_sbp)] =  0
df_final$score_pulse[is.na(df_final$score_pulse)] =  0
df_final$score_oximetry[is.na(df_final$score_oximetry)] = 0
df_final$score_gcs_eye[is.na(df_final$score_gcs_eye)] = 0
df_final$score_gcs_motor[is.na(df_final$score_gcs_motor)] = 0
df_final$score_gcs_verbal[is.na(df_final$score_gcs_verbal)] = 0

df_final=df_final %>%
  mutate(total_score = score_gcs_eye +  score_gcs_motor +
            score_oximetry + score_pulse + score_resp + score_sbp)

#-----------ADD SCORES OBTAINED TO THE RAW TEST SET ---------------
df_final_test=test_data
df_final_test=sqldf('select a.*, score as score_resp
               from df_final_test a
               left join score1 b
               on a.diff_resp1=b.rn')

df_final_test=sqldf('select a.*, score as score_sbp
               from df_final_test a
               left join score1 b
               on a.diff_sbp1=b.rn')

df_final_test=sqldf('select a.*, score as score_pulse
               from df_final_test a
               left join score1 b
               on a.diff_pulse1=b.rn')

df_final_test=sqldf('select a.*, score as score_oximetry
               from df_final_test a
               left join score1 b
               on a.diff_oxi1=b.rn')

df_final_test=sqldf('select a.*, score as score_gcs_eye
               from df_final_test a
               left join score1 b
               on a.prehospital_gcs_eye_num=b.rn')

df_final_test=sqldf('select a.*, score as score_gcs_motor
               from df_final_test a
               left join score1 b
               on a.prehospital_gcs_motor_num=b.rn')


df_final_test$score_resp[is.na(df_final_test$score_resp)] =  0
df_final_test$score_sbp[is.na(df_final_test$score_sbp)] =  0
df_final_test$score_pulse[is.na(df_final_test$score_pulse)] =  0
df_final_test$score_oximetry[is.na(df_final_test$score_oximetry)] = 0
df_final_test$score_gcs_eye[is.na(df_final_test$score_gcs_eye)] = 0
df_final_test$score_gcs_motor[is.na(df_final_test$score_gcs_motor)] = 0
df_final_test$score_gcs_verbal[is.na(df_final_test$score_gcs_verbal)] = 0

df_final_test=df_final_test %>%
  mutate(total_score = score_gcs_eye +  score_gcs_motor +
            score_oximetry + score_pulse + score_resp + score_sbp)


#-------RUN SECOND MODEL TO CHECK RELATION OF AMPT SCORE AND TOTAL SCORE -------------#
#      ALSO CHECK ACCURACY OF THE MODEL USING THE TOTAL SCORE                         #
#BEST RESULTS: ACCURACY:
#-------------------------------------------------------------------------------------#
model2=glm(ampt_score_Outcome ~ total_score,
                 data=df_final, family="binomial") #)

summary(model2)

predict_test= predict(model2, newdata=df_final_test)
accuracy=table(predict_test, df_final_test$ampt_score_Outcome)
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data=predict_test, df_final_test$ampt_score_Outcome)

newx=seq(min(df_final$total_score),
         max(df_final$total_score),1)

prd=predict(model2,
            newdata=data.frame(total_score=newx), se.fit=T
            )
prd$fit

#------------------------------CREATE BAR PLOT OF PROBABILITY OF -------------#
#  BAR PLOT OF PROBABILITY OF USING HELICOPTER FOR A TOTAL SCORE              #
#-----------------------------------------------------------------------------#
count=as.matrix(table(cut(df_final$total_score,
                          breaks=seq(0, 6, 1)), df_final$ampt_score_Outcome))
t(count)
par(mar=c(5,4,4,5)+.1)

barplot(t(count),
        main="Scores versus probability of taking helicopter",
        xlab="Scores",
        ylab="observed number of patients",
        col=c("lightblue", "red"))

legend("topright", fill=c("red", "lightblue", NA),
       lty=c(NA, NA, 1), lwd=c(NA, NA, 2),
       legend=c("Need raid transport", "Do not need rapid transport",
                "Predicted Prob"),
       col=c("black"),
       border=c("black", "black", NA))

par(new=TRUE)

plot(prd$fit ~ newx,
     col="black",
     lwd=2, xaxt="n", yaxt="n",
     xlab="", ylab="")

polygon(c(rev(newx), newx),
        c(rev(prd$fit + 1.96*prd$se.fit),
          prd$fit - 1.96*prd$se.fit),
        col=adjustcolor("grey80", alpha=0.5),
        border=NA)

lines(newx, prd$fit + 1.96*prd$se.fit,
      lty="dashed", col="red")

lines(newx, prd$fit - 1.96*prd$se.fit,
      lty="dashed",  col="red")
axis(4)
mtext("predicted probability of helicopter",
      side=4, line=3)

#---------------------SET SCORE ---------------------------------------#
#ADD SCORE TO THE ORIGINAL DATA SET AND COMPUTE THE TOTAL FOR          #
# DOWNSTREAM USE AND FURTHER ANALYSIS                                  #
# DOWNSTREAM BEING USE IN OVER UTILIZATION ANALYSIS AND PREDICTION IF  #
# NEED BE. ANALYSIS SHOULD ALSO BE MADE OF HOW DIFFERENT THE SCORES    #
# ARE BY AGE, GENDER AND OTHER RELEVANT FIELDS                         #
#----------------------------------------------------------------------#

feature_set1$gcs_score_motor[feature_set1$prehospital_gcs_motor_num == 2 |
                               feature_set1$prehospital_gcs_motor_num == 4] = 1
feature_set1$gcs_score_eye[feature_set1$prehospital_gcs_eye_num == 2 | 
                             feature_set1$prehospital_gcs_eye_num == 3 |
                             feature_set1$prehospital_gcs_eye_num == 4] = 1
feature_set1$veh_acc_score[feature_set1$vehicle_accident == 'Yes'] = 1
feature_set1$gender_score[feature_set1$patient_gender_tr1_15 == 'Male'] = 1
feature_set1$sbp_score[feature_set1$diff_from_normal_sbp > 30] = 1
feature_set1$resp_score[feature_set1$diff_from_normal_resp > 1] = 1
feature_set1$pulse_score[feature_set1$diff_from_normal_pulse > 2] = 1
feature_set1$oxi_score[feature_set1$diff_in_oximetry > -5 &
                         feature_set1$diff_in_oximetry < -1] = 1

feature_set1$gcs_score_eye[is.na(feature_set1$gcs_score_eye)] = 0
feature_set1$gcs_score_motor[is.na(feature_set1$gcs_score_motor)] = 0
feature_set1$sbp_score[is.na(feature_set1$sbp_score)] = 0
feature_set1$pulse_score[is.na(feature_set1$pulse_score)] = 0
feature_set1$resp_score[is.na(feature_set1$resp_score)] = 0
feature_set1$oxi_score[is.na(feature_set1$oxi_score)] = 0
feature_set1$gender_score[is.na(feature_set1$gender_score)] = 0
feature_set1$veh_acc_score[is.na(feature_set1$veh_acc_score)] = 0

feature_set1 = feature_set1 %>%
  mutate(
    triage_score = gcs_score_eye + gcs_score_motor + sbp_score + pulse_score +
           resp_score + oxi_score)

#------------------------------TRIAGE SCORE ANALYSIS ---------------------#

#----------------BY TRANSPORT
helicopter_over=feature_set1 %>%
  filter(transport_to_your_facility_by_tr8_8 == 'Medevac/HEMS' | 
                         transport_to_your_facility_by_tr8_8 == 'Fixed-Wing/Airplane')
heli_tb=as.data.frame(prop.table(table(helicopter_over$triage_score, helicopter_over$Outcome)))

ggplot(heli_tb, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Var2) +
  ggtitle("Triage Score to understand Helicopter Overutilization")


#----------------BY OUTCOME
heli_out=feature_set1 %>%
  filter(Outcome == 'Y')

all_trans=as.data.frame(prop.table(table(heli_out$triage_score, heli_out$transport_to_your_facility_by_tr8_8)))

ggplot(all_trans, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity") +
  facet_wrap(~Var2) +
  ggtitle("Triage Score to understand Helicopter Overutilization")
