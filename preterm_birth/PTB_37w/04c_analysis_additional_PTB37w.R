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
setwd("H:/Mirthe/")

#################################################################################
# 04_ANALYSIS: PREDICT PRETERM BIRTH
# PTB of <37 weeks gestational age
#################################################################################
#### ADDITIONAL ANALYSES  ####
# run additional analyses to compare the full and restricted PTB models to prediction models with 
# only PTB history records or with only medical records


#### PTB history only model  ####
# this model includes a single predictor variable; PTB history categorized as yes (has had previous PTB), no (has not had previous PTB), or nvt (first child)
# the underlying intention is to mimic PTB risk assessment in clinical practice, and compare this to the restricted model 

#### 1. Look at the set of predictors ####
# read the birth cohort data with records at the moment of conception created in 02_preprocessing_preterm_birth.R
pretbirth_dat_atto_conception <- readRDS("case1_preterm_birth/PTB_data/pretbirth_dat_atto_conception_singelton_20102020.rds")
# look at the pretbirth_dat_atto_conception singelton 2010-2020 data set
glimpse(pretbirth_dat_atto_conception)

# look at the proportion of preterm births in the data 
# preterm birth < 37 weeks
table(pretbirth_dat_atto_conception$premature_birth37)
round(prop.table(table(pretbirth_dat_atto_conception$premature_birth37))*100, digits = 2)
# 5.48% of the children in the cohort are born extremely premature


#### 2. Dealing with missing data: load multiple imputed data ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# For the restricted models, we used the MICE algorithm for multiple imputation of the missing data: see 03_MI_restmodel_PTB

# read the multiple imputed birth cohort train data with records at the moment of conception created in 03_MI_restmodel_PTB.R
mi_complete <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_complete.rds")
# look at the data set
glimpse(mi_complete)

mi_test_comp <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_test_comp.rds")



#### 3. Create the data set for the PTB history only model ####
# include both PTB<32w outcome variables and PTB history variable N_vroeg_24_37
func_postprocess_ptbhist <- function(mi_list) {
  mi_list %>% 
    select(c(premature_birth37, N_vroeg_24_37))
}
# apply the function to the multiple imputed train data and test data 
mi_complete_ptbhist <- lapply(mi_complete, func_postprocess_ptbhist)
mi_test_ptbhist <- lapply(mi_test_comp, func_postprocess_ptbhist)

# in the m = 3 imputed data sets there are small differences in the N_vroeg_24_37 variable
# because the 'geen data' category is imputed
table(mi_complete_ptbhist$'1'$N_vroeg_24_37)
table(mi_complete_ptbhist$'3'$N_vroeg_24_37)

# create the eerder_vroeg_24_37 variable for the PTB history only model: is there a previous PTB? yes, no or nvt (first child)
func_postprocess_eerdervroeg <- function(mi_list) {
  mi_list %>% 
    mutate(eerder_vroeg_24_37 = case_when(N_vroeg_24_37 == 'nvt' ~ "nvt",
                                          N_vroeg_24_37 == "1" ~ "ja",
                                          N_vroeg_24_37 == "2" ~ "ja",
                                          N_vroeg_24_37 == "3" ~ "ja",
                                          N_vroeg_24_37 == "3+" ~ "ja",
                                          N_vroeg_24_37 == "0" ~ "nee"), 
           eerder_vroeg_24_37 = as.factor(eerder_vroeg_24_37))
}
# apply the function to the multiple imputed train data and test data
mi_complete_ptbhist <- lapply(mi_complete_ptbhist, func_postprocess_eerdervroeg)
mi_test_ptbhist <- lapply(mi_test_ptbhist, func_postprocess_eerdervroeg)
# look at the created variable
table(mi_complete_ptbhist$'3'$eerder_vroeg_24_37)
table(mi_test_ptbhist$'3'$eerder_vroeg_24_37)

#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# set seed for reproducibility
set.seed(8151)

# Train the random forest model; remember we have m = 3 multiple imputed data sets, we train 3 RF models, for each of the imputed data sets 
rf_ptbhist <- mi_complete_ptbhist %>% 
  # use lapply to apply the function to the list of m = 3
  lapply(ranger, formula = premature_birth37 ~ eerder_vroeg_24_37, num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object.
#saveRDS(rf_ptbhist, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_ptbhist.rds")

# look at the trained models. Remember you have m = 3 trained models
rf_ptbhist$`1`
rf_ptbhist$`2`
rf_ptbhist$`3`
rf_ptbhist$`2`$variable.importance

# looking at predictions in the trained model 
summary(rf_ptbhist$`1`$predictions[,2])
# predicted probabilities are on the scale 0-1, we plot a smaller scale because most predicted probabilities are very low 
hist(rf_ptbhist$`1`$predictions[,2], xlim = c(0, 0.1), freq = TRUE)

#### 4.2 Test the random forest model: population sample ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_ptbhist <- lapply(rf_ptbhist, function(model){
  predict(model, data = mi_test_ptbhist$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_ptbhist_pool <- data.frame(
  pred_rf_ptbhist1 = pred_rf_ptbhist[[1]],
  pred_rf_ptbhist2 = pred_rf_ptbhist[[2]],
  pred_rf_ptbhist3 = pred_rf_ptbhist[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_ptbhist_pool <- pred_rf_ptbhist_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_ptbhist1, pred_rf_ptbhist2, pred_rf_ptbhist3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_ptbhist_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_ptbhist37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set so that everyone with eerder_vroeg_24_37 = 'ja' is classified as 1 high risk (at 0.015)
pred_rf_ptbhist_pool_class1 <- ifelse(pred_rf_ptbhist_pool$pred_outcome_prob > 0.03, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_ptbhist_pool_class_cm1 <- confusionMatrix(pred_rf_ptbhist_pool_class1, mi_test_ptbhist$'1'$premature_birth37, positive = "1")
pred_rf_ptbhist_pool_class_cm1
pred_rf_ptbhist_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_ptbhist_pool_class1, mi_test_ptbhist$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_ptbhist_pool_class_cmp1

# look at the area under the ROC curve to assess overall model performance
rf_ptbhist_roc <- roc(mi_test_ptbhist$'1'$premature_birth37, pred_rf_ptbhist_pool$pred_outcome_prob)
auc(rf_ptbhist_roc)
rf_ptbhist_auc <- auc(rf_ptbhist_roc)
plot(rf_ptbhist_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_ptbhist_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_ptbhist37_roc.rds") 


### save the predicted probabilities by outcome: population sample
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_ptbhist_pool$premature_birth37 <- mi_test_ptbhist$'1'$premature_birth37

# look at predicted probabilities by PTB outcome
PTB_rf_ptbhist_pool_cdf_1 <- pred_rf_ptbhist_pool %>% filter(premature_birth37 == 1)
PTB_rf_ptbhist_pool_cdf_0 <- pred_rf_ptbhist_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_ptbhist_pool_cdf_1_list <- PTB_rf_ptbhist_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_ptbhist_pool_cdf_0_list <- PTB_rf_ptbhist_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_ptbhist_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_ptbhist_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_ptbhist_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_ptbhist_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability



#### 4.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
# create a sample of only multipara (not the first pregnancy) for m = 3
mi_test_ptbhist_multi <- lapply(mi_test_ptbhist, function(mi_list){
  filter(mi_list, N_vroeg_24_37 != "nvt")})
prop.table(table(mi_test_ptbhist_multi$'1'$premature_birth37))*100
sapply(mi_test_ptbhist_multi$'1', summary)

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_ptbhist_multi <- lapply(rf_ptbhist, function(model){
  predict(model, data = mi_test_ptbhist_multi$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_ptbhist_multi_pool <- data.frame(
  pred_rf_ptbhist_multi1 = pred_rf_ptbhist_multi[[1]],
  pred_rf_ptbhist_multi2 = pred_rf_ptbhist_multi[[2]],
  pred_rf_ptbhist_multi3 = pred_rf_ptbhist_multi[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_ptbhist_multi_pool <- pred_rf_ptbhist_multi_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_ptbhist_multi1, pred_rf_ptbhist_multi2, pred_rf_ptbhist_multi3), na.rm = T))
hist(pred_rf_ptbhist_multi_pool$pred_outcome_prob, freq = TRUE, xlim =c(0, 0.1))
summary(pred_rf_ptbhist_multi_pool$pred_outcome_prob)

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_ptbhist_multi_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_ptbhist_multi37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set so that everyone with eerder_vroeg_24_37 = 'ja' is classified as 1 high risk (at 0.015)
pred_rf_ptbhist_multi_pool_class1 <- ifelse(pred_rf_ptbhist_multi_pool$pred_outcome_prob > 0.03, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_ptbhist_multi_pool_class_cm1 <- confusionMatrix(pred_rf_ptbhist_multi_pool_class1, mi_test_ptbhist_multi$'1'$premature_birth37, positive = "1")
pred_rf_ptbhist_multi_pool_class_cm1
pred_rf_ptbhist_multi_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_ptbhist_multi_pool_class1, mi_test_ptbhist_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_ptbhist_multi_pool_class_cmp1


# look at the area under the ROC curve to assess overall model performance
rf_ptbhist_multi_roc <- roc(mi_test_ptbhist_multi$'1'$premature_birth37, pred_rf_ptbhist_multi_pool$pred_outcome_prob)
auc(rf_ptbhist_multi_roc)
rf_ptbhist_multi_auc <- auc(rf_ptbhist_multi_roc)
plot(rf_ptbhist_multi_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_ptbhist_multi_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_ptbhist37_multi_roc.rds") 


### save the predicted probabilities by outcome: sample of multipara
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_ptbhist_multi_pool$premature_birth37 <- mi_test_ptbhist_multi$'1'$premature_birth37

# look at predicted probabilities by PTB outcome
PTB_rf_ptbhist_multi_pool_cdf_1 <- pred_rf_ptbhist_multi_pool %>% filter(premature_birth37 == 1)
PTB_rf_ptbhist_multi_pool_cdf_0 <- pred_rf_ptbhist_multi_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_ptbhist_multi_pool_cdf_1_list <- PTB_rf_ptbhist_multi_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_ptbhist_multi_pool_cdf_0_list <- PTB_rf_ptbhist_multi_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_ptbhist_multi_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_ptbhist_multi_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_ptbhist_multi_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_ptbhist_multi_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability

# save all auc
rf_ptbhist_auc_names <- list('rf_ptbhist_auc' = rf_ptbhist_auc, 'rf_ptbhist_multi_auc' = rf_ptbhist_multi_auc)
openxlsx::write.xlsx(rf_ptbhist_auc_names, file = 'case1_preterm_birth/PTB_output/ptb_37weeks/RF_auc_ptbhist37_MI.xlsx')







#### Medical birth records model  ####
# this model includes medical records available in birth care clinical practice such as birth history, age of the pregnant individual and care history
# The underlying intention is to compare the restricted model with medical birth records and socio-economic factors, 
# to this prediction model that includes only the medical birth records, in order to say something about the added value of 
# socio-economic factors. 

#### 1. Look at the set of predictors ####
# read the birth cohort data with records at the moment of conception created in 02_preprocessing_preterm_birth.R
predictors <- readRDS("case1_preterm_birth/PTB_data/predictors_rest_model.rds")
# look at the predictors full model PTB <32w data set
glimpse(predictors)

# look at the proportion of preterm births in the data 
# preterm birth < 37 weeks
table(predictors$premature_birth3737)
round(prop.table(table(predictors$premature_birth3737))*100, digits = 2)


#### 2. Dealing with missing data: load multiple imputed data ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# For the restricted models, we used the MICE algorithm for multiple imputation of the missing data: see 03_MI_restmodel_PTB

# read the multiple imputed birth cohort train data with records at the moment of conception created in 03_MI_restmodel_PTB.R
mi_complete <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_complete.rds")
# look at the data set
glimpse(mi_complete)

mi_test_comp <- readRDS("case1_preterm_birth/PTB_data/ptb_37weeks/predictors_rest_model37_mi_test_comp.rds")

#### 3. Create the data set for the medical birth records model ####
# select only the medical birth records not the socio-economic factors 
func_preprocess_medical <- function(mi_list) {
  mi_list %>% 
    select(-c(educationlevel_mo, educationlevel_fa, COROP2020_mo, STED_mo, 
              house_ownership_mo, plhh_partner_child, income_hh_mo_cat, age_at_concp_fa_cat))
} 
# apply the function to the multiple imputed train data and test data 
mi_complete_medical <- lapply(mi_complete, func_preprocess_medical)
mi_test_medical <- lapply(mi_test_comp, func_preprocess_medical)
summary(mi_complete_medical$'1')
prop.table(table(mi_complete_medical$'1'$premature_birth37))*100

#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# set seed for reproducibility
set.seed(8151)

# Train the random forest model; remember we have m = 3 multiple imputed data sets, we train 3 RF models, for each of the imputed data sets 
rf_medical <- mi_complete_medical %>% 
  # use lapply to apply the function to the list of m = 3
  lapply(ranger, formula = premature_birth37 ~ ., num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object.
#saveRDS(rf_medical, "case1_preterm_birth/PTB_output/rf_medical.rds")

# look at the trained models. Remember you have m = 3 trained models
rf_medical$`1`
rf_medical$`2`
rf_medical$`3`
rf_medical$`1`$variable.importance
rf_medical$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the PTB outcome
# look at variable importance score, ordered and standardized (% out of 100%)  
var_imp_norm1 <- data.frame(var = names(rf_medical$`1`$variable.importance), 
                            imp = rf_medical$`1`$variable.importance/sum(rf_medical$`1`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm2 <- data.frame(var = names(rf_medical$`1`$variable.importance), 
                            imp = rf_medical$`2`$variable.importance/sum(rf_medical$`2`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm3 <- data.frame(var = names(rf_medical$`1`$variable.importance), 
                            imp = rf_medical$`3`$variable.importance/sum(rf_medical$`3`$variable.importance)*100) %>% 
  arrange(desc(imp))

# create a plot of the 23th most important variables
# change var_imp_norm1 to 2 or 3 if you want to look at the plots for the other m's
var_imp_norm1[1:14,] %>% # change this number in case you want to look at more independent variables in the model
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF medical model variable importance plot preterm birth < 32 weeks")

varimp_sheet_names <- list('var_imp_norm1' = var_imp_norm1, 'var_imp_norm2' = var_imp_norm2, 'var_imp_norm3' = var_imp_norm3)
openxlsx::write.xlsx(varimp_sheet_names, file = 'case1_preterm_birth/PTB_output/ptb_37weeks/RF_VarImp_medical37_MI.xlsx')

# looking at predictions in the trained model 
summary(rf_medical$`1`$predictions[,2])
hist(rf_medical$`1`$predictions[,2], xlim = c(0, 1), freq = TRUE)

#### 4.2 Test the random forest model: population sample ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_medical <- lapply(rf_medical, function(model){
  predict(model, data = mi_test_medical$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_medical_pool <- data.frame(
  pred_rf_medical1 = pred_rf_medical[[1]],
  pred_rf_medical2 = pred_rf_medical[[2]],
  pred_rf_medical3 = pred_rf_medical[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_medical_pool <- pred_rf_medical_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_medical1, pred_rf_medical2, pred_rf_medical3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_medical_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_medical37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 5.48% of pregnancies
pred_rf_medical_pool_class1 <- ifelse(pred_rf_medical_pool$pred_outcome_prob > 0.0868, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_pool_class_cm1 <- confusionMatrix(pred_rf_medical_pool_class1, mi_test_medical$'1'$premature_birth37, positive = "1")
pred_rf_medical_pool_class_cm1
pred_rf_medical_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_medical_pool_class1, mi_test_medical$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_pool_class_cmp1

# try different thresholds: > 0.083 approx. 10% highest risks
pred_rf_medical_pool_class10 <- ifelse(pred_rf_medical_pool$pred_outcome_prob > 0.083, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_pool_class_cm10 <- confusionMatrix(pred_rf_medical_pool_class10, mi_test_medical$'1'$premature_birth37, positive = "1")
pred_rf_medical_pool_class_cm10
pred_rf_medical_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_medical_pool_class10, mi_test_medical$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_pool_class_cmp10

# try different thresholds: > 0.0695 approx. 15% highest risks
pred_rf_medical_pool_class15 <- ifelse(pred_rf_medical_pool$pred_outcome_prob > 0.0695, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_pool_class_cm15 <- confusionMatrix(pred_rf_medical_pool_class15, mi_test_medical$'1'$premature_birth37, positive = "1")
pred_rf_medical_pool_class_cm15
pred_rf_medical_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_medical_pool_class15, mi_test_medical$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_pool_class_cmp15

# look at the area under the ROC curve to assess overall model performance
rf_medical_roc <- roc(mi_test_medical$'1'$premature_birth37, pred_rf_medical_pool$pred_outcome_prob)
auc(rf_medical_roc)
rf_medical_auc <- auc(rf_medical_roc)
plot(rf_medical_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_medical_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_medical37_roc.rds") 


### save the predicted probabilities by outcome: population sample
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_medical_pool$premature_birth37 <- mi_test_medical$'1'$premature_birth37

# look at predicted probabilities by PTB outcome
PTB_rf_medical_pool_cdf_1 <- pred_rf_medical_pool %>% filter(premature_birth37 == 1)
PTB_rf_medical_pool_cdf_0 <- pred_rf_medical_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_medical_pool_cdf_1_list <- PTB_rf_medical_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_medical_pool_cdf_0_list <- PTB_rf_medical_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_medical_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_medical_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 4.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
# create a sample of only multipara (not the first pregnancy) for m = 3
mi_test_medical_multi <- lapply(mi_test_medical, function(mi_list){
  filter(mi_list, interpreg_cat != "nvt" & N_vroeg_24_37 != "nvt")})
prop.table(table(mi_test_medical_multi$'1'$premature_birth37))*100
sapply(mi_test_medical_multi$'1', summary)

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_medical_multi <- lapply(rf_medical, function(model){
  predict(model, data = mi_test_medical_multi$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_medical_multi_pool <- data.frame(
  pred_rf_medical_multi1 = pred_rf_medical_multi[[1]],
  pred_rf_medical_multi2 = pred_rf_medical_multi[[2]],
  pred_rf_medical_multi3 = pred_rf_medical_multi[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_medical_multi_pool <- pred_rf_medical_multi_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_medical_multi1, pred_rf_medical_multi2, pred_rf_medical_multi3), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_medical_multi_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_medical_multi37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 4.1% of pregnancies
pred_rf_medical_multi_pool_class1 <- ifelse(pred_rf_medical_multi_pool$pred_outcome_prob > 0.138, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_multi_pool_class_cm1 <- confusionMatrix(pred_rf_medical_multi_pool_class1, mi_test_medical_multi$'1'$premature_birth37, positive = "1")
pred_rf_medical_multi_pool_class_cm1
pred_rf_medical_multi_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_medical_multi_pool_class1, mi_test_medical_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_multi_pool_class_cmp1

# try different thresholds: > 0.058 approx. 10% highest risks
pred_rf_medical_multi_pool_class10 <- ifelse(pred_rf_medical_multi_pool$pred_outcome_prob > 0.058, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_multi_pool_class_cm10 <- confusionMatrix(pred_rf_medical_multi_pool_class10, mi_test_medical_multi$'1'$premature_birth37, positive = "1")
pred_rf_medical_multi_pool_class_cm10
pred_rf_medical_multi_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_medical_multi_pool_class10, mi_test_medical_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_multi_pool_class_cmp10

# try different thresholds: > 0.0484 approx. 15% highest risks
pred_rf_medical_multi_pool_class15 <- ifelse(pred_rf_medical_multi_pool$pred_outcome_prob > 0.0484, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_multi_pool_class_cm15 <- confusionMatrix(pred_rf_medical_multi_pool_class15, mi_test_medical_multi$'1'$premature_birth37, positive = "1")
pred_rf_medical_multi_pool_class_cm15
pred_rf_medical_multi_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_medical_multi_pool_class15, mi_test_medical_multi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_multi_pool_class_cmp15

# look at the area under the ROC curve to assess overall model performance
rf_medical_multi_roc <- roc(mi_test_medical_multi$'1'$premature_birth37, pred_rf_medical_multi_pool$pred_outcome_prob)
auc(rf_medical_multi_roc)
rf_medical_multi_auc <- auc(rf_medical_multi_roc)
plot(rf_medical_multi_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_medical_multi_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_medical37_multi_roc.rds") 


### save the predicted probabilities by outcome: sample of multipara
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_medical_multi_pool$premature_birth37 <- mi_test_medical_multi$'1'$premature_birth37

# look at predicted probabilities by PTB outcome
PTB_rf_medical_multi_pool_cdf_1 <- pred_rf_medical_multi_pool %>% filter(premature_birth37 == 1)
PTB_rf_medical_multi_pool_cdf_0 <- pred_rf_medical_multi_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_medical_multi_pool_cdf_1_list <- PTB_rf_medical_multi_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_medical_multi_pool_cdf_0_list <- PTB_rf_medical_multi_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_medical_multi_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_multi_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_medical_multi_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_multi_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability




#### 4.4 Test the random forest model: sample of primipara   ####
# we use the previously trained RF model and test it on a sample of primipara
# create a sample of only primipara (not the first pregnancy) for m = 3
mi_test_medical_primi <- lapply(mi_test_medical, function(mi_list){
  filter(mi_list, interpreg_cat == "nvt" & N_vroeg_24_37 == "nvt")})
table(mi_test_medical_primi$'2'$premature_birth37)
prop.table(table(mi_test_medical_primi$'1'$premature_birth37))*100
sapply(mi_test_medical_primi$'3', summary)

# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_medical_primi <- lapply(rf_medical, function(model){
  predict(model, data = mi_test_medical_primi$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use m = 2 or 3, it should not make much of a difference

# for each individual in the test set you obtain m = 3 predicted probabilities because you have trained 3 RF models on 3 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_medical_primi_pool <- data.frame(
  pred_rf_medical_primi1 = pred_rf_medical_primi[[1]],
  pred_rf_medical_primi2 = pred_rf_medical_primi[[2]],
  pred_rf_medical_primi3 = pred_rf_medical_primi[[3]]
)

# pool the m = 3 predicted probabilities per individual by taking the mean
pred_rf_medical_primi_pool <- pred_rf_medical_primi_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_medical_primi1, pred_rf_medical_primi2, pred_rf_medical_primi3), na.rm = T))
hist(pred_rf_medical_primi_pool$pred_outcome_prob)

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_medical_primi_pool, "case1_preterm_birth/PTB_output/ptb_37weeks/pred_rf_medical_primi37_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 7.02% of pregnancies
pred_rf_medical_primi_pool_class1 <- ifelse(pred_rf_medical_primi_pool$pred_outcome_prob > 0.0865, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_primi_pool_class_cm1 <- confusionMatrix(pred_rf_medical_primi_pool_class1, mi_test_medical_primi$'1'$premature_birth37, positive = "1")
pred_rf_medical_primi_pool_class_cm1
pred_rf_medical_primi_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_medical_primi_pool_class1, mi_test_medical_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_primi_pool_class_cmp1

# try different thresholds: > 0.086 approx. 10% highest risks
pred_rf_medical_primi_pool_class10 <- ifelse(pred_rf_medical_primi_pool$pred_outcome_prob > 0.086, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_primi_pool_class_cm10 <- confusionMatrix(pred_rf_medical_primi_pool_class10, mi_test_medical_primi$'1'$premature_birth37, positive = "1")
pred_rf_medical_primi_pool_class_cm10
pred_rf_medical_primi_pool_class_cmp10 <- round(prop.table(confusionMatrix(pred_rf_medical_primi_pool_class10, mi_test_medical_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_primi_pool_class_cmp10

# try different thresholds: > 0.0825 approx. 15% highest risks
pred_rf_medical_primi_pool_class15 <- ifelse(pred_rf_medical_primi_pool$pred_outcome_prob > 0.0825, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_medical_primi_pool_class_cm15 <- confusionMatrix(pred_rf_medical_primi_pool_class15, mi_test_medical_primi$'1'$premature_birth37, positive = "1")
pred_rf_medical_primi_pool_class_cm15
pred_rf_medical_primi_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_medical_primi_pool_class15, mi_test_medical_primi$'1'$premature_birth37, positive = "1")$table),4)*100
pred_rf_medical_primi_pool_class_cmp15


# look at the area under the ROC curve to assess overall model performance
rf_medical_primi_roc <- roc(mi_test_medical_primi$'1'$premature_birth37, pred_rf_medical_primi_pool$pred_outcome_prob)
auc(rf_medical_primi_roc)
rf_medical_primi_auc <- auc(rf_medical_primi_roc)
plot(rf_medical_primi_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_medical_primi_roc, "case1_preterm_birth/PTB_output/ptb_37weeks/rf_medical37_primi_roc.rds") 

### save the predicted probabilities by outcome: sample of primipara
# add the observed outcome to the data frame with predicted probabilities 
pred_rf_medical_primi_pool$premature_birth37 <- mi_test_medical_primi$'1'$premature_birth37

# look at predicted probabilities by PTB outcome
PTB_rf_medical_primi_pool_cdf_1 <- pred_rf_medical_primi_pool %>% filter(premature_birth37 == 1)
PTB_rf_medical_primi_pool_cdf_0 <- pred_rf_medical_primi_pool %>% filter(premature_birth37 == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_rf_medical_primi_pool_cdf_1_list <- PTB_rf_medical_primi_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
PTB_rf_medical_primi_pool_cdf_0_list <- PTB_rf_medical_primi_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(PTB_rf_medical_primi_pool_cdf_1_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_primi_pool_cdf_1_list.csv")
write_excel_csv2(PTB_rf_medical_primi_pool_cdf_0_list, "case1_preterm_birth/PTB_output/ptb_37weeks/PTB37_rf_medical_primi_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


# save all auc
rf_medical_auc_names <- list('rf_medical_auc' = rf_medical_auc, 'rf_medical_multi_auc' = rf_medical_multi_auc, 'rf_medical_primi_auc' = rf_medical_primi_auc)
openxlsx::write.xlsx(rf_medical_auc_names, file = 'case1_preterm_birth/PTB_output/ptb_37weeks/RF_auc_medical37_MI.xlsx')










#### 5. Compare models using ROC curves ####

# read roc objects or used the ones saved in your environment
# full models
rf_full_roc<- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_full37_roc.rds")
auc(rf_full_roc)

rf_rest_roc <- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_roc.rds")
auc(rf_rest_roc)
rf_rest_multi_roc <- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_multi_roc.rds")
auc(rf_rest_multi_roc)
rf_rest_primi_roc <- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_rest37_primi_roc.rds")
auc(rf_rest_primi_roc)

rf_ptbhist_roc <- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_ptbhist37_roc.rds") 
auc(rf_ptbhist_roc)
rf_ptbhist_multi_roc <- readRDS("case1_preterm_birth/PTB_output/ptb_37weeks/rf_ptbhist37_multi_roc.rds") 
auc(rf_ptbhist_multi_roc)

plot(rf_full_roc, print.auc= TRUE, print.auc.x = 0.2, print.auc.y = 0.00, col = "orange")
plot(rf_rest_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.05, add = TRUE, col = "violet")
plot(rf_rest_multi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.1, add = TRUE, col = "blue")
plot(rf_rest_primi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.15, add = TRUE, col = "lightgreen")


legend("topleft", legend = c("PTB <37w full", "PTB <37w restricted",
                             "PTB <37w rest multi", "PTB <37w rest primi"), 
       col = c("orange", "violet","blue", "lightgreen"), lwd =2)


plot(rf_rest_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.15, col = "violet")
plot(rf_rest_multi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.1, add = TRUE, col = "blue")
plot(rf_ptbhist_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.05, add = TRUE, col = "orange")
plot(rf_ptbhist_multi_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.00, add = TRUE, col = "lightgreen")


legend("topleft", legend = c("PTB <37w restricted", "PTB <37w restricted multi", "PTB <37w history only", "PTB <37w history only multi"), 
       col = c("violet","blue", "orange", "lightgreen"), lwd =2)




#### Single predictor var_i models ####
output_path <- "H:/Mirthe/case1_preterm_birth/PTB_output/"

df_auc <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(df_auc) <- c("predictor", "auc")

for (var_i in names(mi_complete$'1')[-1]) {
  print(var_i) # this is the variable 
  formula_i <- as.formula(paste("premature_birth37 ~", var_i))
  
  # train the RF model with var_i as a single predictor
  rf_rest_i <- mi_complete %>%
    # use lapply to apply the function to the list of m = 3 (this is not necessary for variables without missings in original set)
    lapply(ranger, formula = formula_i, num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)
  
  print(rf_rest_i$`1`)
  # predict on test data
  pred_rf_rest_i <- lapply(rf_rest_i, function(model){
    predict(model, data = mi_test_comp$`1`)$predictions[,2]
  })
  #write_rds(pred_rf_rest_i, file.path(output_path, paste0("rfmodel_pred_vari", var_i, ".rds")))
  
  # pool predictions
  pred_rf_rest_pool_i <- data.frame(
    pred_rf_rest_i1 = pred_rf_rest_i[[1]],
    pred_rf_rest_i2 = pred_rf_rest_i[[2]],
    pred_rf_rest_i3 = pred_rf_rest_i[[3]]
  )
  
  pred_rf_rest_pool_i <- pred_rf_rest_pool_i %>%
    mutate(pred_outcome_prob = rowMeans(select(., pred_rf_rest_i1, pred_rf_rest_i2, pred_rf_rest_i3), na.rm = T))
  
  # look at the area under the ROC curve to assess overall model performance
  rf_rest_roc_i <- roc(mi_test_comp$'1'$premature_birth37, pred_rf_rest_pool_i$pred_outcome_prob)
  auc_i <- auc(rf_rest_roc_i)
  print(auc_i)
  # add AUC for the var_i model to a dataframe
  auc_vari <- data.frame(predictor = var_i, auc = auc_i)
  df_auc <- rbind(df_auc, auc_vari)
  print(df_auc)
  
  # save model output
  write_rds(rf_rest_roc_i, file.path(output_path, paste0("roc_rfmodel_varimp_vari", var_i, ".rds")))
  
  
}
# save all AUC scores in one document 
openxlsx::write.xlsx(df_auc, "case1_preterm_birth/PTB_output/ptb_37weeks/rfmodel_df_auc.xlsx")

