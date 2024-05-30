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
#################################################################################
#### FULL MODEL ####
# this model includes all the available variables from the perined and CBS data

### 1. Look at the set of predictors
# read the birth cohort data with records at the moment of conception created in 02_preprocessing_preterm_birth.R
predictors <- readRDS("case1_preterm_birth/repo/PTB_data/predictors_full_model32.rds")
# look at the predictors full model PTB <32w data set
glimpse(predictors)

# look at the proportion of preterm births in the data 
# preterm birth < 32 weeks
table(predictors$premature_birth)
round(prop.table(table(predictors$premature_birth))*100, digits = 2)
# 0.82% of the children in the cohort are born extremely premature


### 2. Dealing with missing data
# Right now there are missing values in the dataset, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# For the restricted models, we used the MICE algorithm for multiple imputation of the missing data (see 03_MI_restmodel_PTB)
# For the full models, we also tried to use multiple imputation to deal with the missing data, but this turned out to be infeasible
# due to computational issues (the data set is too big with too many complicated categorical variables)
# Thus, for the full models we used median imputation for numerical data and we use dummies to indicate missings in categorical data
# Please be aware of the limitations of this approach.

# look at the percentage missingness per variable
round(colSums(is.na(predictors)/nrow(predictors)*100), digits = 2)

# look at the character, numeric and factor variables separately
predictors %>% 
  keep(is.numeric) %>% 
  summary()

predictors %>% 
  keep(is.factor) %>% 
  summary()

# Handling missings in categorical data: create NA dummies for factor variables 
# function to replace the NA's in all factors with NAdummy as a level 
replace_factor_na <- function(x){
  # select only variables with more than 0 NA's 
  if (sum(is.na(x)) > 0) { 
    # for each variable add NA as a factor level and labels this level 88
    x <- factor(x, levels = levels(addNA(x)), labels = c(levels(x), "NAdummy"), exclude = NULL)
  } else { 
    x }
}
predictors <- predictors %>% 
  mutate_if(is.factor, replace_factor_na)


# Handling missings in numeric data
##Extract name of numeric variables with missings in your dataframe (this version does not require manual list of variables, so less sensitive to changes in de dataframe)
numeric_vars_with_miss <- names(which(colSums(is.na(predictors%>%keep(is.numeric)))>0))

##create dummy indicating missingness
for (var_name in numeric_vars_with_miss) {
  dummy_var_name <- paste0("NA_dummy_", var_name)
  predictors[dummy_var_name] <- ifelse(is.na(predictors[[var_name]]), 0, 1)
}

#Explore the data summaries to make a choice for imputation method 
for (var_name in numeric_vars_with_miss) {
  #make summary of variables with missings to look at distribution
  #Median seems the best universal choice for single imputation (in particular, for costs variables)
  print(var_name)
  print(summary(predictors[[var_name]]))
  print(histogram(predictors[[var_name]]))
}

##Impute median if there is a missing value
for (var_name in numeric_vars_with_miss) {
  predictors[[var_name]] <- ifelse(is.na( predictors[[var_name]]), median(predictors[[var_name]], na.rm=TRUE), predictors[[var_name]])
}

#check there are no missing data in the dataset
sum(!complete.cases(predictors))


### 3. Create train and test split for the full models

# set seed for reproducibility
set.seed(8151)

# assign split value per row, with probability of 0.7 for train and 0.3 for test
split <- sample(c(TRUE, FALSE), nrow(predictors), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
pretbirth_train <- predictors[split, ]
pretbirth_test <- predictors[!split, ]

# create seperate multi and primi test sets 
table(pretbirth_test$par_cat)
pretbirth_test_multi <- pretbirth_test %>% 
  filter(par_cat != "0")
pretbirth_test_primi <- pretbirth_test %>% 
  filter(par_cat == "0")

# exclude par_cat from analysis because this is unknown at 16 weeks gestational age
# pretbirth_train <- pretbirth_train %>% select(-c(par_cat))
# pretbirth_test <- pretbirth_test %>% select(-c(par_cat))
# pretbirth_test_multi <- pretbirth_test_multi %>% select(-c(par_cat))
# pretbirth_test_primi <- pretbirth_test_primi %>% select(-c(par_cat))

#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# Train the random forest model
rf_full <- ranger(premature_birth ~ . , data = pretbirth_train, num.trees = 1000, #mtry = 5, min.node.size = 15,
                        importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object. 
# saveRDS(rf_full, "case1_preterm_birth/repo/PTB_output/rf_full32.rds")

# look at the trained model
rf_full
rf_full$variable.importance
rf_full$prediction.error

# variable importance scores show how 'important' a variable is in predicting the PTB outcome
# look at variable importance score, ordered and standardized (% out of 100%)  
var_imp_norm <- data.frame(var = names(rf_full$variable.importance), # make sure to not include the outcome variable here 
                           imp = rf_full$variable.importance/sum(rf_full$variable.importance)*100) %>% 
  arrange(desc(imp))
# create a plot of the Nth most important variables
var_imp_norm[1:25,] %>% # specify the N 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF model variable importance plot PTB <32w full model") # change title if you use PTB of <37w as an outcome

write_excel_csv2(var_imp_norm, "case1_preterm_birth/repo/PTB_output/RF_VarImp_32full.csv")

# looking at predictions in the trained model 
head(rf_full$predictions)
summary(rf_full$predictions)
hist(rf_full$predictions[,2], xlim = c(0, 1))

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds 0.07 (threshold at prevalence = 0.82), 0.025 (threshold for 9% highest risks), (threshold for 15% highest risks)
rf_full_prob <- ifelse(rf_full$predictions[,2] >= 0.07, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_full_cm <- confusionMatrix(rf_full_prob, pretbirth_train$premature_birth, positive = "1")
rf_full_cm
rf_full_cmp <- round(prop.table(confusionMatrix(rf_full_prob, pretbirth_train$premature_birth, positive = "1")$table),4)*100
rf_full_cmp

rf_full_prob2 <- ifelse(rf_full$predictions[,2] >= 0.025, 1, 0) %>% as.factor()  
rf_full_cm2 <- confusionMatrix(rf_full_prob2, pretbirth_train$premature_birth, positive = "1")
rf_full_cm2
rf_full_cmp2 <- round(prop.table(confusionMatrix(rf_full_prob2, pretbirth_train$premature_birth, positive = "1")$table),4)*100
rf_full_cmp2

#### 4.2 Test the random forest model ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance
# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_full <- predict(rf_full, data = pretbirth_test)
saveRDS(pred_rf_full, "case1_preterm_birth/repo/PTB_output/pred_rf_full.rds")
# look at the predicted probabilities
summary(pred_rf_full$predictions[,2])
hist(pred_rf_full$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_full_class <- ifelse(pred_rf_full$predictions[,2] >= 0.3, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_full_pred_cm <- confusionMatrix(pred_rf_full_class, pretbirth_test$premature_birth, positive = "1")
rf_full_pred_cm
rf_full_pred_cmp <- round(prop.table(confusionMatrix(pred_rf_full_class, pretbirth_test$premature_birth, positive = "1")$table),4)*100
rf_full_pred_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_full_class2 <- ifelse(pred_rf_full$predictions[,2] >= 0.025, 1, 0) %>% as.factor()
rf_full_pred_cm2 <- confusionMatrix(pred_rf_full_class2, pretbirth_test$premature_birth, positive = "1")
rf_full_pred_cm2
rf_full_pred_cmp2 <- round(prop.table(confusionMatrix(pred_rf_full_class2, pretbirth_test$premature_birth, positive = "1")$table),4)*100
rf_full_pred_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_full_roc <- roc(pretbirth_test$premature_birth, pred_rf_full$predictions[,2])
auc(rf_full_roc)
plot(rf_full_roc, print.auc = TRUE)
saveRDS(rf_full_roc, "case1_preterm_birth/repo/PTB_output/rf_full32_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_cdf <- pretbirth_test
PTB_test_cdf$pred_prob <- pred_rf_full$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_cdf_1 <- PTB_test_cdf %>% filter(premature_birth == 1)
PTB_test_cdf_0 <- PTB_test_cdf %>% filter(premature_birth == 0)
summary(PTB_test_cdf_1$pred_prob)
summary(PTB_test_cdf_0$pred_prob)

# plot cdf 
ecdf(PTB_test_cdf_1$pred_prob)
plot(ecdf(PTB_test_cdf_1$pred_prob), xlab = "predicted probability") +  #do.points = TRUE,
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(PTB_test_cdf_1$pred_prob), col = "red")+
  abline(v = median(PTB_test_cdf_1$pred_prob), col = "orange")

plot(ecdf(PTB_test_cdf_0$pred_prob), xlab = "predicted probability") +  #do.points = TRUE,
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(PTB_test_cdf_0$pred_prob), col = "red")+
  abline(v = median(PTB_test_cdf_0$pred_prob), col = "orange")

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_cdf_1_list <- PTB_test_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_cdf_0_list <- PTB_test_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_cdf_1_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_cdf_1_list.csv")
write_excel_csv2(PTB_test_cdf_0_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
pred_rf_full_multi <- predict(rf_full, data = pretbirth_test_multi)
saveRDS(pred_rf_full_multi, "case1_preterm_birth/repo/PTB_output/pred_rf_full32_multi.rds")
# look at the predicted probabilities
summary(pred_rf_full_multi$predictions[,2])
hist(pred_rf_full_multi$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_full_multi_class <- ifelse(pred_rf_full_multi$predictions[,2] >= 0.3, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_full_pred_multi_cm <- confusionMatrix(pred_rf_full_multi_class, pretbirth_test_multi$premature_birth, positive = "1")
rf_full_pred_multi_cm
rf_full_pred_multi_cmp <- round(prop.table(confusionMatrix(pred_rf_full_multi_class, pretbirth_test_multi$premature_birth, positive = "1")$table),4)*100
rf_full_pred_multi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_full_multi_class2 <- ifelse(pred_rf_full_multi$predictions[,2] >= 0.025, 1, 0) %>% as.factor()
rf_full_pred_multi_cm2 <- confusionMatrix(pred_rf_full_multi_class2, pretbirth_test_multi$premature_birth, positive = "1")
rf_full_pred_multi_cm2
rf_full_pred_multi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_full_multi_class2, pretbirth_test_multi$premature_birth, positive = "1")$table),4)*100
rf_full_pred_multi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_full_multi_roc <- roc(pretbirth_test_multi$premature_birth, pred_rf_full_multi$predictions[,2])
auc(rf_full_multi_roc)
plot(rf_full_multi_roc, print.auc = TRUE)
saveRDS(rf_full_multi_roc, "case1_preterm_birth/repo/PTB_output/rf_full32_multi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_multi_cdf <- pretbirth_test_multi
PTB_test_multi_cdf$pred_prob <- pred_rf_full_multi$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_multi_cdf_1 <- PTB_test_multi_cdf %>% filter(premature_birth == 1)
PTB_test_multi_cdf_0 <- PTB_test_multi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_multi_cdf_1_list <- PTB_test_multi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_multi_cdf_0_list <- PTB_test_multi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_multi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_multi_cdf_1_list.csv")
write_excel_csv2(PTB_test_multi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_multi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability





#### 3.4 Test the random forest model: sample of primipara   ####
# we use the previously trained RF model and test it on a sample of primipara
# create a sample of only primipara (not the first pregnancy)
pred_rf_full_primi <- predict(rf_full, data = pretbirth_test_primi)
saveRDS(pred_rf_full_primi, "case1_preterm_birth/repo/PTB_output/pred_rf_full32_primi.rds")
# look at the predicted probabilities
summary(pred_rf_full_primi$predictions[,2])
hist(pred_rf_full_primi$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_full_primi_class <- ifelse(pred_rf_full_primi$predictions[,2] >= 0.3, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_full_pred_primi_cm <- confusionMatrix(pred_rf_full_primi_class, pretbirth_test_primi$premature_birth, positive = "1")
rf_full_pred_primi_cm
rf_full_pred_primi_cmp <- round(prop.table(confusionMatrix(pred_rf_full_primi_class, pretbirth_test_primi$premature_birth, positive = "1")$table),4)*100
rf_full_pred_primi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_full_primi_class2 <- ifelse(pred_rf_full_primi$predictions[,2] >= 0.025, 1, 0) %>% as.factor()
rf_full_pred_primi_cm2 <- confusionMatrix(pred_rf_full_primi_class2, pretbirth_test_primi$premature_birth, positive = "1")
rf_full_pred_primi_cm2
rf_full_pred_primi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_full_primi_class2, pretbirth_test_primi$premature_birth, positive = "1")$table),4)*100
rf_full_pred_primi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_full_primi_roc <- roc(pretbirth_test_primi$premature_birth, pred_rf_full_primi$predictions[,2])
auc(rf_full_primi_roc)
plot(rf_full_primi_roc, print.auc = TRUE)
saveRDS(rf_full_primi_roc, "case1_preterm_birth/repo/PTB_output/rf_full32_primi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_primi_cdf <- pretbirth_test_primi
PTB_test_primi_cdf$pred_prob <- pred_rf_full_primi$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_primi_cdf_1 <- PTB_test_primi_cdf %>% filter(premature_birth == 1)
PTB_test_primi_cdf_0 <- PTB_test_primi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_primi_cdf_1_list <- PTB_test_primi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_primi_cdf_0_list <- PTB_test_primi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_primi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_primi_cdf_1_list.csv")
write_excel_csv2(PTB_test_primi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/PTB32_test_primi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability







#### 5. Logistic regression model ####
# we also train and test a logistic regression model and consider its predictive performance in the case of PTB
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

#### 5.1 Train the Logistic regression model using glm ####
glm_full <- glm(premature_birth ~ ., 
                 data = pretbirth_train, family = binomial)
# look at the trained model
glm_full_summary <- summary(glm_full)
glm_full_summary
glm_full_summary_names <- attributes(glm_full_summary$coefficients)[[2]][[1]]

# you can save the trained model but this is a very big object
#saveRDS(glm_full, "case1_preterm_birth/repo/PTB_output/glm_full.rds")
# maybe save the coefficients instead
write_excel_csv2(as.data.frame(glm_full_summary_names), "case1_preterm_birth/repo/PTB_output/glm_full_variablenames.csv")
write_excel_csv2(as.data.frame(glm_full_summary$coefficients), "case1_preterm_birth/repo/PTB_output/glm_full_coefficients.csv")
# ! FIND A BETTER WAY TO SAVE VARIABLES AND THEIR COEFFICIENTS AT ONCE

# in logistic regression we interpret the model coefficients as the change in the log-odds
# of the response as a result of a unit change in the predictor variable
# we take the exponent the coefficients to interpret them, explaining the change in odds rather than log-odds
exp(coef(glm_full)["N_vooraf_sga1"])  #or choose a different coefficient

# if there are very little observations in a certain category in the test data group them, this might give unreliable results
table(pretbirth_test$N_vooraf_sga)
#pretbirth_test$N_vooraf_sga <- fct_collapse(pretbirth_test$N_vooraf_sga, "6" = c("6", "7"))

#### 5.2 Test the logistic regression model ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance
pred_glm_full <- predict(glm_full, pretbirth_test, type = "response")
summary(pred_glm_full)
hist(pred_glm_full)

# Classify the predictions of training data; predict binary outcome using a predicted probability threshold
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds 0.05555 (threshold at prevalence = 0.82), 0.025 (threshold for 6% highest risks), (threshold for 15% highest risks)
pred_glm_full_class <- ifelse(pred_glm_full >= 0.05555, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_full_cm <- confusionMatrix(pred_glm_full_class, pretbirth_test$premature_birth, positive = "1")
pred_glm_full_cm
pred_glm_full_cmp <- round(prop.table(confusionMatrix(pred_glm_full_class, pretbirth_test$premature_birth, positive = "1")$table),4)*100
pred_glm_full_cmp

# try a different threshold value
pred_glm_full_class2 <- ifelse(pred_glm_full >= 0.025, 1, 0) %>% as.factor()
pred_glm_full_cm2 <- confusionMatrix(pred_glm_full_class2, pretbirth_test$premature_birth, positive = "1")
pred_glm_full_cm2
pred_glm_full_cmp2 <- round(prop.table(confusionMatrix(pred_glm_full_class2, pretbirth_test$premature_birth, positive = "1")$table),4)*100
pred_glm_full_cmp2

# look at the area under the ROC curve to assess overall model performance
glm_full_roc <- roc(pretbirth_test$premature_birth, pred_glm_full)
auc(glm_full_roc)
plot(glm_full_roc, print.auc = TRUE) # add = TRUE (to see both roc curves in one plot)
saveRDS(glm_full_roc, "case1_preterm_birth/repo/PTB_output/glm_full_roc.rds") 


# remove files you no longer need from environment
# rm(rf_full, rf_full_cm, var_imp_norm2, pred_rf_full, rf_full_pred_cm, rf_full_pred_cm2,
#    roc_rfr, PTB_test_cdf, PTB_test_cdf_0, PTB_test_cdf_0_list, 
#    PTB_test_cdf_1, PTB_test_cdf_1_list, pred_rf_full_class,
#    pred_rf_full_class2, rf_full_cmp, rf_full_pred_cmp, rf_full_pred_cm2p,
#    rf_full_prob)




####################################################################################
# Additional (robustness) checks

# 1. COMPARE MODELS
### PTB HISTORY ONLY MODEL ###
# To be able to interpret the predictive performance of the trained models, specifically the AUC score,
# we compare the full model (and the restricted model) with the PTB history only-model, which is the current practice 
# The PTB history only model contains a single variable: eerder_vroeg_24_37, was there a previous PTB of <37w? yes, no, or nvt (first child)
# This allows us to say something about whether additional predictors in the full/restricted model (including socioeconomic factors), 
# increase predictive performance (AUC) compared to predictive performance based on this single medical birth history variable

predictors_cp <- pretbirth_dat_atto_conception %>% 
  select(c(premature_birth, premature_birth37, N_vroeg_24_37, vooraf_zw_vroeg_24_37))

sum(is.na(predictors_cp$N_vroeg_24_37))
# !! DELETE THIS LATER: temp solution to fix missings in generated birth history variables
# MIRTHE TO POLINA: not sure why there are missings here?
predictors_cp <- predictors_cp %>%
  mutate(N_vroeg_24_37 = case_when(is.na(N_vroeg_24_37) & vooraf_zw_vroeg_24_37 == 'ja' ~ "1",
                                   is.na(N_vroeg_24_37) ~ "0",
                                   !is.na(N_vroeg_24_37) ~ N_vroeg_24_37))

# Create the variable eerder_vroeg_24_37, was there a previous PTB <37w? yes, no, or nvt (first child)
table(predictors_cp$N_vroeg_24_37)
predictors_cp <- predictors_cp %>%
  mutate(eerder_vroeg_24_37 = case_when(N_vroeg_24_37 == 'nvt' ~ "nvt",
                                        N_vroeg_24_37 == "1" ~ "ja",
                                        N_vroeg_24_37 == "2" ~ "ja",
                                        N_vroeg_24_37 == "3" ~ "ja",
                                        N_vroeg_24_37 == "4" ~ "ja",
                                        N_vroeg_24_37 == "5" ~ "ja",
                                        N_vroeg_24_37 == "0" ~ "nee"))
table(predictors_cp$eerder_vroeg_24_37)

# set seed for reproducibility
set.seed(8151)
# assign split value per row, with probability of 0.7 for train and 0.3 for test
split_cp <- sample(c(TRUE, FALSE), nrow(predictors_cp), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
pretbirth_train_cp <- predictors_cp[split_cp, ]
pretbirth_test_cp <- predictors_cp[!split_cp, ]


# Train the random forest model using ranger
# if you are interested in PTB <37w (instead of <32w), change the outcome variable to premature_birth37
rf_cp <- ranger(premature_birth ~ eerder_vroeg_24_37, data = pretbirth_train_cp, num.trees = 1000,
                      importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# potentially try different hyperparameter specifications for mtry and min.node.size

# look at trained model 
rf_cp
rf_cp$variable.importance
rf_cp$prediction.error

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
head(rf_cp$predictions)
summary(rf_cp$predictions[,2])
hist(rf_cp$predictions[,2], xlim = c(0, 1))
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds 0.02 (threshold closes to prevalence = 0.82),
rf_cp_prob <- ifelse(rf_cp$predictions[,2] >= 0.02, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_cp_cm <- confusionMatrix(rf_cp_prob, pretbirth_train_cp$premature_birth, positive = "1")
rf_cp_cm
rf_cp_cmp <- round(prop.table(confusionMatrix(rf_cp_prob, pretbirth_train_cp$premature_birth, positive = "1")$table),4)*100
rf_cp_cmp


### Use the trained model to predict individual outcomes in the test data & evaluate predictive performance 
pred_rf_cp <- predict(rf_cp, data = pretbirth_test_cp)
#saveRDS(pred_rf_cp, "case1_preterm_birth/repo/PTB_output/pred_rf_cp.rds")
summary(pred_rf_cp$predictions[,2])
hist(pred_rf_cp$predictions[,2])
# as you can see there is little diversity in predicted probabilities, this what we would expect because we train the model
# with only one predictor; a categorical variable with three classes. As a result, there are three risk groups. 

# classify predictions of test data; predict binary outcome using predicted probability threshold
pred_rf_cp_class <- ifelse(pred_rf_cp$predictions[,2] >= 0.02, 1, 0) %>% as.factor()
rf_cp_pred_cm <- confusionMatrix(pred_rf_cp_class, pretbirth_test_cp$premature_birth, positive = "1")
rf_cp_pred_cm
rf_cp_pred_cmp <- round(prop.table(confusionMatrix(pred_rf_cp_class, pretbirth_test_cp$premature_birth, positive = "1")$table),4)*100
rf_cp_pred_cmp

# look at the area under the ROC curve to assess overall model performance
rf_cp_roc <- roc(pretbirth_test_cp$premature_birth, pred_rf_cp$predictions[,2])
auc(rf_cp_roc)
plot(rf_cp_roc, print.auc = TRUE)
saveRDS(rf_cp_roc, "case1_preterm_birth/repo/PTB_output/rf_cp_roc.rds") 

#### Test model - multiparous 

# Using the test data, create a sample of only multiparous (not the first child)
pretbirth_test_multi <- pretbirth_test_cp %>%
  filter(N_vroeg_24_37 != "geen data" & N_vroeg_24_37 != "nvt")
table(pretbirth_test_multi$eerder_vroeg_24_37)
prop.table(table(pretbirth_test_multi$premature_birth))*100

### Use the trained model to predict individual outcomes in the multiparous test data & evaluate predictive performance 
pred_rf_cp_multi <- predict(rf_cp, data = pretbirth_test_multi)
summary(pred_rf_cp_multi$predictions[,2])
hist(pred_rf_cp_multi$predictions[,2])
# as you can see there is little diversity in predicted probabilities, this what we would expect because we train the model
# with only one predictor; a categorical variable with two classes. As a result, there are two risk groups. 

# classify predictions of test data; predict binary outcome using predicted probability threshold
pred_rf_cp_multi_class <- ifelse(pred_rf_cp_multi$predictions[,2] >= 0.02, 1, 0) %>% as.factor()
rf_cp_multi_pred_cm <- confusionMatrix(pred_rf_cp_multi_class, pretbirth_test_multi$premature_birth, positive = "1")
rf_cp_multi_pred_cm
rf_cp_multi_pred_cmp <- round(prop.table(confusionMatrix(pred_rf_cp_multi_class, pretbirth_test_multi$premature_birth, positive = "1")$table),4)*100
rf_cp_multi_pred_cmp

# look at the area under the ROC curve to assess overall model performance
rf_cp_multi_roc <- roc(pretbirth_test_multi$premature_birth, pred_rf_cp_multi$predictions[,2])
auc(rf_cp_multi_roc)
plot(rf_cp_multi_roc, print.auc = TRUE)+
  abline(h = 0.348)
saveRDS(rf_cp_multi_roc, "case1_preterm_birth/repo/PTB_output/rf_cp_multi_roc.rds") 



### COMPARE MODELS: ROC CURVES ###

# read roc objects or used the ones saved in your environment
# full models
rf_full_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_full_roc.rds")
auc(rf_full_roc)

rf_full37_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_full37_roc.rds")
auc(rf_full37_roc)

# PTB history only models - complete sample and multiparous
rf_cp_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_cp_roc.rds") 
auc(rf_cp_roc)

rf_cp37_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_cp37_roc.rds") 
auc(rf_cp37_roc)

rf_cp_multi_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_cp_multi_roc.rds")
auc(rf_cp_multi_roc)

rf_cp_multi37_roc <- readRDS("case1_preterm_birth/repo/PTB_output/rf_cp_multi37_roc.rds")
auc(rf_cp_multi37_roc)

plot(rf_full_roc, print.auc= TRUE, print.auc.x = 0.2, print.auc.y = 0.05, col = "black")
plot(rf_cp_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.1, add = TRUE, col = "darkgreen")
plot(rf_cp_multi_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.15, add = TRUE, col = "lightgreen")
plot(rf_full37_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.2, add = TRUE, col = "grey")
plot(rf_cp37_roc , print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0.25, add = TRUE, col = "darkblue")
plot(rf_cp_multi37_roc, print.auc = TRUE, print.auc.x = 0.2, print.auc.y = 0, add = TRUE, col = "lightblue")+
  abline(h = 0.348)
legend("topleft", legend = c("PTB <32w full", "PTB <32w history only",  "PTB <32w history only multi", "PTB <37w full", "PTB <37w history only",  "PTB <37w history only multi", ), 
       col = c("black", "darkgreen", "lightgreen","grey", "darkblue", "lightblue"), lwd =4)




# 2. DEALING WITH CLASS IMBALANCE 
# A challenge working with this data set and with the outcome PTB is class imbalance
# Class imbalance is a common issue in classification tasks where the number of instances in each class of the outcome is not equal.
# In this data set, a significant proportion of the observed instances of PTB belong to one class: non-preterm birth (the majority class).
# This biases the model towards the majority class at the expense of the minority class; preterm birth, our class of interest.
# The class imbalance problem is usually addressed by oversampling and under-sampling approaches.
# We used WERCS to balance the data set. Subsequently, we trained the RF model on the balanced data set. 
# This however resulted in a trained model that was clearly overfit; predictive performance of the trained balanced model was better compared to unbalanced, 
# but performance in test data did not improve.
table(predictors$premature_birth)
prop.table(table(predictors$premature_birth)) * 100

