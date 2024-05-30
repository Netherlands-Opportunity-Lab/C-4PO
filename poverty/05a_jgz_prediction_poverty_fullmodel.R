#### JGZ DATA ANALYSIS ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R 
# and (05_jgz_preprocess_MI) we prepare the JGZ data for analysis in 05_jgz_prediction.R

#### 05_jgz_prediction_poverty.R ####
# THE GOAL OF THIS SCRIPT IS TO IDENTIFY POVERTY 

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
library(xlsx)
library(mice)
setwd("H:/Mirthe/")


#################################################################################
# 05_ANALYSIS: PREDICT POVERTY
# in this case, we do not predict a future outcome, we use the model to identify children in a situation (at risk) of poverty
#################################################################################
#### FULL MODEL ####
# this model includes all the available data from JGZ, CBS and Perined

#### 1. Look at the set of predictors (before multiple imputation) ####
predictors <- readRDS("case_jgz/repo/poverty/data/poverty_predictors_p.rds")

# look at the outcome variable: taal_ont3y9m
table(predictors$l_income_hh_pov_binary)
prop.table(table(predictors$l_income_hh_pov_binary)) * 100

predictors %>% 
  group_by(l_income_hh_pov_binary, geslacht) %>% 
  summarise(n = n())


#### 2. Dealing with missing data - multiple imputation with MICE ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# To deal with missing data, we use the indicator-method and impute NA dummies for categorical varaibles. 
# we are aware of the limitations of this method and advice using multiple imputation (with MICE) in the future 

# look at missings
round(colSums(is.na(predictors)) / nrow(predictors)*100, 2)

# look at the character, numeric and factor variables separately
predictors %>% 
  keep(is.numeric) %>% 
  summary()

predictors %>% 
  keep(is.factor) %>% 
  summary()

# create NA dummies for numeric and/or character variables
predictors <- predictors %>% 
  mutate(across(c(lft_concept_mo,                           # NA dummy for age of father at birth, as proxy for missing mother
                  lft_concept_fa,                           # NA dummy for age of father at birth, as proxy for missing father
                  geboortegew,                              # NA dummy for children not in new perined data
                  bmi_2yrs_znl,                             # NA dummy for those without length/weight observations at doe-moment
                  ED_woz,                                   # NA dummy for woz 
                  SPOLIS_wages_mo, SPOLIS_wages_fa,         # NA dummy for all SPOLIS variables
                  ZVWK_GP_basic_mo, ZVWK_GP_basic_fa,       # NA dummy for health care costs 
                  ZVWK_mentalhealth_bas_mo, ZVWK_mentalhealth_bas_fa),       # NA dummy for health care costs  
                .fns = list(NA_dummy = ~as.factor(ifelse(is.na(.x), 1, 0))), 
                .names = "{fn}_{col}"))

# for all numeric variables set NA to 0 (you could do this for the whole data set, not just predictors, but that is more computationally intensive and takes longer)
predictors <- predictors %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


# create NA dummies for factor variables 
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

# check to make sure there are no missings in the predictor variables
colSums(is.na(predictors))

#### 3. Generate train and test set ####

# set seed for reproducibility
set.seed(8151)

# assign split value per row, with probability of 0.7 for train and 0.3 for test
split <- sample(c(TRUE, FALSE), nrow(predictors), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
pov_train <- predictors[split, ]
pov_test <- predictors[!split, ]

# frequency table of the outcome l_income_hh_pov_binary
table(pov_train$l_income_hh_pov_binary)
prop.table(table(pov_train$l_income_hh_pov_binary)) * 100
table(pov_test$l_income_hh_pov_binary)
prop.table(table(pov_test$l_income_hh_pov_binary)) * 100


#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 
rf_full <- ranger(l_income_hh_pov_binary ~ . , data = pov_train, num.trees = 1000, 
                   importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction, e.g. mtry = 9, #min.node.size = 5, min.bucket = 5.
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object.
#saveRDS(rf_full, "case_jgz/repo/poverty/output/full/rf_full.rds")


# look at the trained model
rf_full
# rf_full$variable.importance
rf_full$prediction.error

# variable importance scores show how 'important' a variable is in predicting the outcome delay in language development
# look at variable importance score, ordered and standardized (% out of 100%) 
var_imp_norm <- data.frame(var = names(pov_train)[-1],
                           imp = rf_full$variable.importance/sum(rf_full$variable.importance)*100) %>% 
  arrange(desc(imp))
# create a plot of the 20th most important variables
var_imp_norm[1:30,] %>% 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF model variable importance plot armoede")

write_excel_csv2(var_imp_norm, "case_jgz/repo/poverty/output/var_imp_norm_full.csv")

# looking at predictions in the trained model 
summary(rf_full$predictions)

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of poverty), 
# otherwise negative =0 (low risk of poverty)
# try different thresholds; e.g. threshold at prevalence 8.2%
rf_full_prob <- ifelse(rf_full$predictions[,2] > 0.31, 1, 0) %>% as.factor()  
cm_rf_full_prob <- confusionMatrix(rf_full_prob, pov_train$l_income_hh_pov_binary, positive = "1")
cm_rf_full_prob
cmp_rf_full_prob <- round(prop.table(confusionMatrix(rf_full_prob, pov_train$l_income_hh_pov_binary, positive = "1")$table),4)*100
cmp_rf_full_prob

#### 4.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# predict poverty in test data using probability estimation
pred_rf_full_prob <- predict(rf_full, data = pov_test)
hist(pred_rf_full_prob$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# try different thresholds; e.g. threshold at prevalence 8.2%
pred_rf_full_prob_class <- ifelse(pred_rf_full_prob$predictions[,2] > 0.31, 1, 0) %>% as.factor()
rf_full_prob_cm <- confusionMatrix(pred_rf_full_prob_class, pov_test$l_income_hh_pov_binary, positive = "1")
rf_full_prob_cm
rf_full_prob_cmp <- round(prop.table(confusionMatrix(pred_rf_full_prob_class, pov_test$l_income_hh_pov_binary, positive = "1")$table),4)*100
rf_full_prob_cmp

# try different thresholds: > 0.164 at 15% highest risks
pred_rf_full_prob_class2 <- ifelse(pred_rf_full_prob$predictions[,2] > 0.164, 1, 0) %>% as.factor()
rf_full_prob_cm2 <- confusionMatrix(pred_rf_full_prob_class2, pov_test$l_income_hh_pov_binary, positive = "1")
rf_full_prob_cm2
rf_full_prob_cmp2 <- round(prop.table(confusionMatrix(pred_rf_full_prob_class2, pov_test$l_income_hh_pov_binary, positive = "1")$table),4)*100
rf_full_prob_cmp2


# look at the area under the ROC curve to assess overall model performance
roc_rfr <- roc(pov_test$l_income_hh_pov_binary, pred_rf_full_prob$predictions[,2])
auc(roc_rfr)
plot(roc_rfr, print.auc = TRUE)
saveRDS(rf_full_roc, "case_jgz/repo/poverty/output/pred_rf_full_pool.csv")

# ! Note: in this case, we do not predict a future outcome, we use the model to identify children in a situation (at risk) of poverty

