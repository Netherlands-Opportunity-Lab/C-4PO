# load the necessary libraries 
library(tidyverse)
library(haven)
library(lubridate)
library(ggplot2)
library(dplyr)
library(caret) # for confusionMatrix
library(ranger)
library(readxl)
library(pROC)
library(mice)
setwd("H:/Mirthe/")

#################################################################################
# 04_ANALYSIS: PREDICT PRETERM BIRTH
#################################################################################
#### RESTRICTED MODEL ####
# this model includes a restricted set of variables; the restricted model includes only variables that are available 
# in a clinical, health care setting (EHR, and are currently registered by Perined) or can reasonably be collected in this setting

#### 1. Look at the set of predictors ####
# read the birth cohort data with records at the moment of conception created in 02_preprocessing_preterm_birth.R
predictors <- readRDS("case1_preterm_birth/PTB_data/predictors_rest_model.rds")
# look at the predictors full model PTB <32w data set
glimpse(predictors)

# look at the proportion of preterm births in the data 
# preterm birth < 37 weeks
table(predictors$premature_birth37)
round(prop.table(table(predictors$premature_birth37))*100, digits = 2)
# 0.81% of the children in the cohort are born extremely premature


#### 2. Dealing with missing data: load multiple imputed data ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# For the restricted models, we used the MICE algorithm for multiple imputation of the missing data: see 03_MI_restmodel_PTB

# read the multiple imputed birth cohort train data with records at the moment of conception created in 03_MI_restmodel_PTB.R
mi_complete <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_complete.rds")
# look at the data set
glimpse(mi_complete)

mi_test_comp <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_test_comp.rds")



#### 3. Random Forest model ####

#### 3.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# set seed for reproducibility
set.seed(8151)

# Train the random forest model; remember we have m = 3 multiple imputed data sets, we train 3 RF models, for each of the imputed data sets 
rf_rest <- mi_complete %>% 
  # use lapply to apply the function to the list of m = 3
  lapply(ranger, formula = premature_birth37 ~ ., num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object.
#saveRDS(rf_rest, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest.rds")

# look at the trained models. Remember you have m = 3 trained models
rf_rest$`1`
rf_rest$`2`
rf_rest$`3`
rf_rest$`1`$variable.importance
rf_rest$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the PTB outcome
# look at variable importance score, ordered and standardized (% out of 100%)  
var_imp_norm1 <- data.frame(var = names(rf_rest$`1`$variable.importance), 
                            imp = rf_rest$`1`$variable.importance/sum(rf_rest$`1`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm2 <- data.frame(var = names(rf_rest$`1`$variable.importance), 
                            imp = rf_rest$`2`$variable.importance/sum(rf_rest$`2`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm3 <- data.frame(var = names(rf_rest$`1`$variable.importance), 
                            imp = rf_rest$`3`$variable.importance/sum(rf_rest$`3`$variable.importance)*100) %>% 
  arrange(desc(imp))

# create a plot of the 23th most important variables
# change var_imp_norm1 to 2 or 3 if you want to look at the plots for the other m's
var_imp_norm1[1:22,] %>% # change this number in case you want to look at more independent variables in the model
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF model variable importance plot preterm birth < 32 weeks")

varimp_sheet_names <- list('var_imp_norm1' = var_imp_norm1, 'var_imp_norm2' = var_imp_norm2, 'var_imp_norm3' = var_imp_norm3)
openxlsx::write.xlsx(varimp_sheet_names, file = 'case1_preterm_birth/PTB_output/ptb_37weeks/RF_VarImp_rest37_MI.xlsx')

# looking at predictions in the trained model 
summary(rf_rest$`1`$predictions[,2])
hist(rf_rest$`1`$predictions[,2], xlim = c(0, 1), freq = TRUE)

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds; threshold at prevalence = 5.48, threshold for 8% highest risks, threshold for 15% highest risks
# you can do this for all m = 3 rf_rest models by changing rf_rest$`1` to rf_rest$`2` or rf_rest$`3`
rf_rest_prob <- ifelse(rf_rest$`1`$predictions[,2] > 0.1085, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest_cm <- confusionMatrix(rf_rest_prob, mi_complete$`1`$premature_birth37, positive = "1")
rf_rest_cm
rf_rest_cmp <- round(prop.table(confusionMatrix(rf_rest_prob, mi_complete$`1`$premature_birth37, positive = "1")$table),4)*100
rf_rest_cmp

#### 3.2 Test the random forest model: population sample ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_rest <- lapply(rf_rest, function(model){
  predict(model, data = mi_test_comp$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_rest_pool <- data.frame(
  pred_rf_rest1 = pred_rf_rest[[1]],
  pred_rf_rest2 = pred_rf_rest[[2]],
  pred_rf_rest3 = pred_rf_rest[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_rest_pool <- pred_rf_rest_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_rest1, pred_rf_rest2, pred_rf_rest3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_rest_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_rest37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 5.48% of pregnancies
pred_rf_rest_pool_class1 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.108, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm1 <- confusionMatrix(pred_rf_rest_pool_class1, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cm1
pred_rf_rest_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class1, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp1

# try different thresholds: > 0.0912 approx. 10% highest risks
pred_rf_rest_pool_class10 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.0912, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm10 <- confusionMatrix(pred_rf_rest_pool_class10, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cm10
pred_rf_rest_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class10, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp10

# try different thresholds: fixed false positive rate 10%, > 0.08766
pred_rf_rest_pool_class10fp <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.08766, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm10fp <- confusionMatrix(pred_rf_rest_pool_class10fp, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cm10fp
pred_rf_rest_pool_class_cmp10fp <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class10fp, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp10fp

# try different thresholds: > 0.08 approx. 15% highest risks
pred_rf_rest_pool_class15 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.08, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm15 <- confusionMatrix(pred_rf_rest_pool_class15, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cm15
pred_rf_rest_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class15, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp15

# try different thresholds: > 0.0735 approx. 20% highest risks
pred_rf_rest_pool_class20 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.0735, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm20 <- confusionMatrix(pred_rf_rest_pool_class20, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cm20
pred_rf_rest_pool_class_cmp20 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class20, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp20

# look at the area under the ROC curve to assess overall model performance
rf_rest_roc <- roc(mi_test_comp$'1'$premature_birth37, pred_rf_rest_pool$pred_outcome_prob)
auc(rf_rest_roc)
rf_rest_auc <- auc(rf_rest_roc)
#plot(rf_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_rest_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_roc.rds") 

# try different thresholds: use roc to obtain the 'best' threshold, optimizing the specificity/sensitivity balance
# however the roc function does not take into account that false positives (FP) or false negatives (FN) might be less desirable in certain cases
# when predicting PTB, we advice to not use the 'best' threshold but think critically about the desired balance of FP/ FN
coords(rf_rest_roc, "best", ret = "threshold")
pred_rf_rest_pool_classb <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.05589806, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cmb <- confusionMatrix(pred_rf_rest_pool_classb, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_rf_rest_pool_class_cmb
pred_rf_rest_pool_class_cmpb <- round(prop.table(confusionMatrix(pred_rf_rest_pool_classb, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmpb

### save the predicted probabilities by outcome: population sample
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_rest_pool$premature_birth37 <- mi_test_comp$'1'$premature_birth37
table(pred_rf_rest_pool$premature_birth37)
prop.table(table(pred_rf_rest_pool$premature_birth37)) * 100

# look at predicted probabilities by PTB outcome
PTB_rf_rest_pool_cdf_1 <- pred_rf_rest_pool %>% filter(premature_birth37 == 1)
PTB_rf_rest_pool_cdf_0 <- pred_rf_rest_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_rest_pool_cdf_1_list <- PTB_rf_rest_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_rest_pool_cdf_0_list <- PTB_rf_rest_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_rest_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_rest_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
# create a sample of only multipara (not the first pregnancy) for m = 3
mi_test_comp_multi <- lapply(mi_test_comp, function(mi_list){
  filter(mi_list, interpreg_cat != "nvt" & N_vroeg_24_37 != "nvt")})
prop.table(table(mi_test_comp_multi$'1'$premature_birth37))*100
sapply(mi_test_comp_multi$'1', summary)

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_rest_multi <- lapply(rf_rest, function(model){
  predict(model, data = mi_test_comp_multi$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_rest_multi_pool <- data.frame(
  pred_rf_rest_multi1 = pred_rf_rest_multi[[1]],
  pred_rf_rest_multi2 = pred_rf_rest_multi[[2]],
  pred_rf_rest_multi3 = pred_rf_rest_multi[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_rest_multi_pool <- pred_rf_rest_multi_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_rest_multi1, pred_rf_rest_multi2, pred_rf_rest_multi3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_rest_multi_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_rest_multi37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 4.1% of pregnancies
pred_rf_rest_multi_pool_class1 <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.155, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cm1 <- confusionMatrix(pred_rf_rest_multi_pool_class1, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cm1
pred_rf_rest_multi_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_class1, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmp1

# try different thresholds: > 0.08 approx. 10% highest risks
pred_rf_rest_multi_pool_class10 <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.08, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cm10 <- confusionMatrix(pred_rf_rest_multi_pool_class10, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cm10
pred_rf_rest_multi_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_class10, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmp10

# try different thresholds: fixed false positive rate 10%, > 0.0715
pred_rf_rest_multi_pool_class10fp <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.0715, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cm10fp <- confusionMatrix(pred_rf_rest_multi_pool_class10fp, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cm10fp
pred_rf_rest_multi_pool_class_cmp10fp <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_class10fp, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmp10fp

# try different thresholds: > 0.0585 approx. 15% highest risks
pred_rf_rest_multi_pool_class15 <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.0585, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cm15 <- confusionMatrix(pred_rf_rest_multi_pool_class15, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cm15
pred_rf_rest_multi_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_class15, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmp15

# try different thresholds: > 0.0484 approx. 20% highest risks
pred_rf_rest_multi_pool_class20 <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.0484, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cm20 <- confusionMatrix(pred_rf_rest_multi_pool_class20, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cm20
pred_rf_rest_multi_pool_class_cmp20 <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_class20, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmp20

# look at the area under the ROC curve to assess overall model performance
rf_rest_multi_roc <- roc(mi_test_comp_multi$'1'$premature_birth37, pred_rf_rest_multi_pool$pred_outcome_prob)
auc(rf_rest_multi_roc)
rf_rest_multi_auc <- auc(rf_rest_multi_roc)
plot(rf_rest_multi_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_rest_multi_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_multi_roc.rds") 


# try different thresholds: use roc to obtain the 'best' threshold, optimizing the specificity/sensitivity balance
# however the roc function does not take into account that false positives (FP) or false negatives (FN) might be less desirable in certain cases
# when predicting PTB, we advice to not use the 'best' threshold but think critically about the desired balance of FP/ FN
coords(rf_rest_multi_roc, "best", ret = "threshold")
pred_rf_rest_multi_pool_classb <- ifelse(pred_rf_rest_multi_pool$pred_outcome_prob > 0.0383106, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_multi_pool_class_cmb <- confusionMatrix(pred_rf_rest_multi_pool_classb, mi_test_comp_multi$'1'$premature_birth37, positive = "1")
pred_rf_rest_multi_pool_class_cmb
pred_rf_rest_multi_pool_class_cmpb <- round(prop.table(confusionMatrix(pred_rf_rest_multi_pool_classb, mi_test_comp_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_multi_pool_class_cmpb

### save the predicted probabilities by outcome: sample of multipara
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_rest_multi_pool$premature_birth37 <- mi_test_comp_multi$'1'$premature_birth37
table(pred_rf_rest_multi_pool$premature_birth37)
prop.table(table(pred_rf_rest_multi_pool$premature_birth37)) * 100

# look at predicted probabilities by PTB outcome
PTB_rf_rest_multi_pool_cdf_1 <- pred_rf_rest_multi_pool %>% filter(premature_birth37 == 1)
PTB_rf_rest_multi_pool_cdf_0 <- pred_rf_rest_multi_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_rest_multi_pool_cdf_1_list <- PTB_rf_rest_multi_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_rest_multi_pool_cdf_0_list <- PTB_rf_rest_multi_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_rest_multi_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_multi_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_rest_multi_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_multi_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability




#### 3.4 Test the random forest model: sample of primipara   ####
# we use the previously trained RF model and test it on a sample of primipara
# create a sample of only primipara (not the first pregnancy) for m = 3
mi_test_comp_primi <- lapply(mi_test_comp, function(mi_list){
  filter(mi_list, interpreg_cat == "nvt" & N_vroeg_24_37 == "nvt")})
table(mi_test_comp_primi$'2'$premature_birth37)
prop.table(table(mi_test_comp_primi$'1'$premature_birth37))*100
sapply(mi_test_comp_primi$'3', summary)

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_rest_primi <- lapply(rf_rest, function(model){
  predict(model, data = mi_test_comp_primi$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_rest_primi_pool <- data.frame(
  pred_rf_rest_primi1 = pred_rf_rest_primi[[1]],
  pred_rf_rest_primi2 = pred_rf_rest_primi[[2]],
  pred_rf_rest_primi3 = pred_rf_rest_primi[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_rest_primi_pool <- pred_rf_rest_primi_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_rest_primi1, pred_rf_rest_primi2, pred_rf_rest_primi3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_rest_primi_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_rest_primi37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 7.02% of pregnancies
pred_rf_rest_primi_pool_class1 <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.1, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cm1 <- confusionMatrix(pred_rf_rest_primi_pool_class1, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cm1
pred_rf_rest_primi_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_class1, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmp1

# try different thresholds: > 0.094 approx. 10% highest risks
pred_rf_rest_primi_pool_class10 <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.094, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cm10 <- confusionMatrix(pred_rf_rest_primi_pool_class10, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cm10
pred_rf_rest_primi_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_class10, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmp10

# try different thresholds: fixed false positive rate 10%, > 0.092
pred_rf_rest_primi_pool_class10 <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.092, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cm10 <- confusionMatrix(pred_rf_rest_primi_pool_class10, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cm10
pred_rf_rest_primi_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_class10, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmp10

# try different thresholds: > 0.0863 approx. 15% highest risks
pred_rf_rest_primi_pool_class15 <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.0863, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cm15 <- confusionMatrix(pred_rf_rest_primi_pool_class15, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cm15
pred_rf_rest_primi_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_class15, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmp15

# try different thresholds: > 0.0145 approx. 20% highest risks
pred_rf_rest_primi_pool_class20 <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.0806, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cm20 <- confusionMatrix(pred_rf_rest_primi_pool_class20, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cm20
pred_rf_rest_primi_pool_class_cmp20 <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_class20, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmp20

# look at the area under the ROC curve to assess overall model performance
rf_rest_primi_roc <- roc(mi_test_comp_primi$'1'$premature_birth37, pred_rf_rest_primi_pool$pred_outcome_prob)
auc(rf_rest_primi_roc)
rf_rest_primi_auc <- auc(rf_rest_primi_roc)
plot(rf_rest_primi_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_rest_primi_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_primi_roc.rds") 


# try different thresholds: use roc to obtain the 'best' threshold, optimizing the specificity/sensitivity balance
# however the roc function does not take into account that false positives (FP) or false negatives (FN) might be less desirable in certain cases
# when predicting PTB, we advice to not use the 'best' threshold but think critically about the desired balance of FP/ FN
coords(rf_rest_primi_roc, "best", ret = "threshold")
pred_rf_rest_primi_pool_classb <- ifelse(pred_rf_rest_primi_pool$pred_outcome_prob > 0.07045619, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_primi_pool_class_cmb <- confusionMatrix(pred_rf_rest_primi_pool_classb, mi_test_comp_primi$'1'$premature_birth37, positive = "1")
pred_rf_rest_primi_pool_class_cmb
pred_rf_rest_primi_pool_class_cmpb <- round(prop.table(confusionMatrix(pred_rf_rest_primi_pool_classb, mi_test_comp_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_rest_primi_pool_class_cmpb

### save the predicted probabilities by outcome: sample of primipara
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_rest_primi_pool$premature_birth37 <- mi_test_comp_primi$'1'$premature_birth37
table(pred_rf_rest_primi_pool$premature_birth37)
prop.table(table(pred_rf_rest_primi_pool$premature_birth37)) * 100

# look at predicted probabilities by PTB outcome
PTB_rf_rest_primi_pool_cdf_1 <- pred_rf_rest_primi_pool %>% filter(premature_birth37 == 1)
PTB_rf_rest_primi_pool_cdf_0 <- pred_rf_rest_primi_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_rest_primi_pool_cdf_1_list <- PTB_rf_rest_primi_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_rest_primi_pool_cdf_0_list <- PTB_rf_rest_primi_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_rest_primi_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_primi_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_rest_primi_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_rest_primi_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability

# save all auc
rf_rest_auc_names <- list('rf_rest_auc' = rf_rest_auc, 'rf_rest_multi_auc' = rf_rest_multi_auc, 'rf_rest_primi_auc' = rf_rest_primi_auc)
openxlsx::write.xlsx(rf_rest_auc_names, file = 'case1_preterm_birth/PTB_output/ptb_37weeks/RF_auc_rest37_MI.xlsx')




#### 4. Logistic regression model ####
# we also train and test a logistic regression model and consider its predictive performance in the case of PTB
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

#### 4.1 Train the Logistic regression model using glm ####
glm_rest <- mi_complete %>% 
  lapply(glm, formula = premature_birth37 ~ ., family = binomial)
# look at the trained model by pooling the m =3 logistic regression models
glm_rest_pool <- summary(pool(glm_rest))
glm_rest_pool
# ! be cautious with the interpretation of the N_vroeg and vooraf_zw_vroeg coefficients
# there are some issues due to these variables being highly correlated

# you can save the trained model but this is a very big object
#saveRDS(glm_rest, "case1_preterm_birth/PTB_output/glm_rest32.rds")
# maybe save the pooled summary instead
write_excel_csv2(glm_rest_pool, "case1_preterm_birth/PTB_output/glm_rest32_pool.csv")

#### 4.2 Test the logistic regression model: population sample ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance

# predict preterm_birth outcomes in test data using probability estimation 
pred_glm_rest <- glm_rest %>% 
  lapply(predict, mi_test_comp$`1`)
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference
# warning: prediction from a rank-deficient fit may be misleading

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_glm_rest_pool <- data.frame(
  pred_glm_rest1 = pred_glm_rest[[1]],
  pred_glm_rest2 = pred_glm_rest[[2]],
  pred_glm_rest3 = pred_glm_rest[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_glm_rest_pool <- pred_glm_rest_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_glm_rest1, pred_glm_rest2, pred_glm_rest3), na.rm = T))

# for the logistic regression, compute the exponential function of the predicted probabilities to get a scale of 0-1
pred_glm_rest_pool <- pred_glm_rest_pool %>%
  mutate(pred_outcome_prob_exp = exp(pred_outcome_prob) / (1+exp(pred_outcome_prob)))

# save the predicted probabilities for future use
write_excel_csv2(pred_glm_rest_pool, "case1_preterm_birth/PTB_output/pred_glm_rest32_pool.csv")


# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.81% of pregnancies
pred_glm_rest_pool_class <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.047, 1, 0) %>% as.factor() # CHECK THRESHOLD!!
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm <- confusionMatrix(pred_glm_rest_pool_class, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_glm_rest_pool_class_cm
pred_glm_rest_pool_class_cmp <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp

# try different thresholds: > 0.18 approx. 10% highest risks
pred_glm_rest_pool_class <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.018, 1, 0) %>% as.factor() # CHECK THRESHOLD!!
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm <- confusionMatrix(pred_glm_rest_pool_class, mi_test_comp$'1'$premature_birth37, positive = "1")
pred_glm_rest_pool_class_cm
pred_glm_rest_pool_class_cmp <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class, mi_test_comp$'1'$premature_birth37, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp

# look at the area under the ROC curve to assess overall model performance
glm_rest_roc <- roc(mi_test_comp$'1'$premature_birth37, pred_glm_rest_pool$pred_outcome_prob_exp)
auc(glm_rest_roc)
plot(glm_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(glm_rest_roc, "case1_preterm_birth/PTB_output/glm_rest_roc.rds") 


### save the predicted probabilities by outcome; population sample
# add the observed outcome to the data frame with predicted probabilities 
pred_glm_rest_pool$premature_birth37 <- mi_test_comp$'1'$premature_birth37
table(pred_glm_rest_pool$premature_birth37)
prop.table(table(pred_glm_rest_pool$premature_birth37)) * 100

# look at predicted probabilities by PTB outcome
PTB_glm_rest_pool_cdf_1 <- pred_glm_rest_pool %>% filter(premature_birth37 == 1)
PTB_glm_rest_pool_cdf_0 <- pred_glm_rest_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_glm_rest_pool_cdf_1_list <- PTB_glm_rest_pool_cdf_1 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)
PTB_glm_rest_pool_cdf_0_list <- PTB_glm_rest_pool_cdf_0 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)

write_excel_csv2(PTB_glm_rest_pool_cdf_1_list, "case1_preterm_birth/PTB_output/PTB_glm_rest32_pool_cdf_1_list.csv")
write_excel_csv2(PTB_glm_rest_pool_cdf_0_list, "case1_preterm_birth/PTB_output/PTB_glm_rest32_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability

# ! we advice running 4b. Logistic regression model adapted (line 527) for potentially more reliable coefficients


#### 4b. Logistic regression model adapted ####
# we also train and test a logistic regression model and consider its predictive performance in the case of PTB
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

# the adapted logistic regression model for PTB excludes the subcategories of the variables N_vroeg_24_37 and vooraf_zw_vroeg_24_37;
# N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37 and vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37
# because logistic regression models do not do well with highly correlated variables; their coefficients might misrepresent the relation to the outcome variable

# create the adapted data set by excluding subcategories of N_vroeg_24_37 and vooraf_zw_vroeg_24_37;
func_deselect_subcategories <- function(mi_list) {
  mi_list %>% 
    select(-c(N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37, 
              vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37))
}
mi_complete_adapted <- lapply(mi_complete, func_deselect_subcategories)
mi_test_comp_adapted <- lapply(mi_test_comp, func_deselect_subcategories)

#### 4.1 Train the Logistic regression model using glm ####
glm_rest_adapted <- mi_complete_adapted %>% 
  lapply(glm, formula = premature_birth37 ~ ., family = binomial)
# look at the trained model by pooling the m =3 logistic regression models
glm_rest_adapted_pool <- summary(pool(glm_rest_adapted))
glm_rest_adapted_pool

# you can save the trained model but this is a very big object
#saveRDS(glm_rest_adapted, "case1_preterm_birth/PTB_output/glm_rest_adapted.rds")
# maybe save the pooled summary instead
write_excel_csv2(glm_rest_adapted_pool, "case1_preterm_birth/PTB_output/glm_rest_adapted_pool.csv")

#### 4.2 Test the logistic regression model: population sample ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance

# predict preterm_birth outcomes in test data using probability estimation 
pred_glm_rest_adapted <- glm_rest_adapted %>% 
  lapply(predict, mi_test_comp_adapted$`1`)
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference
# warning: prediction from a rank-deficient fit may be misleading

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_glm_rest_adapted_pool <- data.frame(
  pred_glm_rest_adapted1 = pred_glm_rest_adapted[[1]],
  pred_glm_rest_adapted2 = pred_glm_rest_adapted[[2]],
  pred_glm_rest_adapted3 = pred_glm_rest_adapted[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_glm_rest_adapted_pool <- pred_glm_rest_adapted_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_glm_rest_adapted1, pred_glm_rest_adapted2, pred_glm_rest_adapted3), na.rm = T))

# for the logistic regression, compute the exponential function of the predicted probabilities to get a scale of 0-1
pred_glm_rest_adapted_pool <- pred_glm_rest_adapted_pool %>%
  mutate(pred_outcome_prob_exp = exp(pred_outcome_prob) / (1+exp(pred_outcome_prob)))

# save the predicted probabilities for future use
write_excel_csv2(pred_glm_rest_adapted_pool, "case1_preterm_birth/PTB_output/pred_glm_rest_adapted_pool.csv")


# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.81% of pregnancies
pred_glm_rest_adapted_pool_class <- ifelse(pred_glm_rest_adapted_pool$pred_outcome_prob_exp > 0.047, 1, 0) %>% as.factor() # CHECK THRESHOLD!!
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_adapted_pool_class_cm <- confusionMatrix(pred_glm_rest_adapted_pool_class, mi_test_comp_adapted$'1'$premature_birth37, positive = "1")
pred_glm_rest_adapted_pool_class_cm
pred_glm_rest_adapted_pool_class_cmp <- round(prop.table(confusionMatrix(pred_glm_rest_adapted_pool_class, mi_test_comp_adapted$'1'$premature_birth37, positive = "1")$table),4)*100
pred_glm_rest_adapted_pool_class_cmp

# try different thresholds: > 0.18 approx. 10% highest risks
pred_glm_rest_adapted_pool_class <- ifelse(pred_glm_rest_adapted_pool$pred_outcome_prob_exp > 0.018, 1, 0) %>% as.factor() # CHECK THRESHOLD!!
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_adapted_pool_class_cm <- confusionMatrix(pred_glm_rest_adapted_pool_class, mi_test_comp_adapted$'1'$premature_birth37, positive = "1")
pred_glm_rest_adapted_pool_class_cm
pred_glm_rest_adapted_pool_class_cmp <- round(prop.table(confusionMatrix(pred_glm_rest_adapted_pool_class, mi_test_comp_adapted$'1'$premature_birth37, positive = "1")$table),4)*100
pred_glm_rest_adapted_pool_class_cmp

# look at the area under the ROC curve to assess overall model performance
glm_rest_adapted_roc <- roc(mi_test_comp_adapted$'1'$premature_birth37, pred_glm_rest_adapted_pool$pred_outcome_prob_exp)
auc(glm_rest_adapted_roc)
plot(glm_rest_adapted_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(glm_rest_adapted_roc, "case1_preterm_birth/PTB_output/glm_rest_adapted_roc.rds") 


### save the predicted probabilities by outcome; population sample
# add the observed outcome to the data frame with predicted probabilities 
pred_glm_rest_adapted_pool$premature_birth37 <- mi_test_comp_adapted$'1'$premature_birth37
table(pred_glm_rest_adapted_pool$premature_birth37)
prop.table(table(pred_glm_rest_adapted_pool$premature_birth37)) * 100

# look at predicted probabilities by PTB outcome
PTB_glm_rest_adapted_pool_cdf_1 <- pred_glm_rest_adapted_pool %>% filter(premature_birth37 == 1)
PTB_glm_rest_adapted_pool_cdf_0 <- pred_glm_rest_adapted_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_glm_rest_adapted_pool_cdf_1_list <- PTB_glm_rest_adapted_pool_cdf_1 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)
PTB_glm_rest_adapted_pool_cdf_0_list <- PTB_glm_rest_adapted_pool_cdf_0 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)

write_excel_csv2(PTB_glm_rest_adapted_pool_cdf_1_list, "case1_preterm_birth/PTB_output/PTB_glm_rest_adapted_pool_cdf_1_list.csv")
write_excel_csv2(PTB_glm_rest_adapted_pool_cdf_0_list, "case1_preterm_birth/PTB_output/PTB_glm_rest_adapted_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability




#### 5. Compare predicted probabilities of RF and logistic regression ####
# Visualize the predicted probabilities of both models using ggplot
# histogram
ggplot()+
  geom_histogram(data = pred_rf_rest_pool, aes(pred_outcome_prob), binwidth = 0.05, alpha = 0.5, fill = "darkblue")+
  geom_histogram(data = pred_glm_rest_pool, aes(pred_outcome_prob_exp), binwidth = 0.05, alpha = 0.5, fill = "pink")

# scatterplot
rest_pred_comb <- tibble(rest_rf_pred = as.numeric(), rest_glm_pred = as.numeric())
rest_pred_comb <- bind_cols(pred_rf_rest_pool$pred_outcome_prob,  
                            pred_glm_rest_pool$pred_outcome_prob_exp) %>% 
  rename(rest_rf_pred = `...1`,
         rest_glm_pred = `...2`)

ggplot()+
  geom_point(data = rest_pred_comb, aes(x = rest_rf_pred, y = rest_glm_pred))+
  xlim(0, 0.65)+
  ylim(0, 0.65)

# inspect the visualizations to see if predicted probabilities of RF and logistic regression are similar

#### 6. Compare models using ROC curves ####

# read roc objects or used the ones saved in your environment
# full models
rf_full_roc<- readRDS("case1_preterm_birth/PTB_output/rf_full32_roc.rds")
auc(rf_full_roc)

rf_rest_roc <- readRDS("case1_preterm_birth/PTB_output/rf_rest32_roc.rds")
auc(rf_rest_roc)
rf_rest_multi_roc <- readRDS("case1_preterm_birth/PTB_output/rf_rest32_multi_roc.rds")
auc(rf_rest_multi_roc)
rf_rest_primi_roc <- readRDS("case1_preterm_birth/PTB_output/rf_rest32_primi_roc.rds")
auc(rf_rest_primi_roc)

plot(rf_full_roc, print.auc= TRUE, print.auc.x = 0.2, print.auc.y = 0.00, col = "orange")
plot(rf_rest_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.05, add = TRUE, col = "violet")
plot(rf_rest_multi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.1, add = TRUE, col = "blue")
plot(rf_rest_primi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.15, add = TRUE, col = "lightgreen")


legend("topleft", legend = c("PTB <32w full", "PTB <32w restricted",
                             "PTB <32w rest multi", "PTB <32w rest primi"), 
       col = c("orange", "violet","blue", "lightgreen"), lwd =2)


