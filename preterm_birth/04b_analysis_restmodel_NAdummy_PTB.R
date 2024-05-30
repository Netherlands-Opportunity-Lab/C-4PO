# load the necessary libraries 
library(tidyverse)
library(haven)
library(lubridate)
library(ggplot2)
library(dplyr)
library(caret) # for confusionMatrix
library(ranger)
library(readxl)
library(mice)
require(naniar)
require(missForest)
require(xlsx)
require(RColorBrewer)
require(reshape2)
require(ggpubr)
setwd("H:/Mirthe/")

#################################################################################
# 03_MI_restmodel_PTB:  Multiple Imputation (MI) with MICE for the case PTB
#################################################################################
#### RESTRICTED MODEL ####
# this model includes a restricted set of variables; the restricted model includes only variables that are available 
# in a clinical, health care setting (EHR, and are currently registered by Perined) or can reasonably be collected in this setting

# look at the data set 
pretbirth_dat_atto_conception <- readRDS("case1_preterm_birth/repo/PTB_data/pretbirth_dat_atto_conception_singelton_20102020.rds")
glimpse(predictors)


predictors <- pretbirth_dat_atto_conception %>%
  # select variables included in analysis 
  select(c(#Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, rins_fa, rin_fa,
           premature_birth, #premature_birth37,
           N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37, 
           vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
           # disregard the vroeg_24_37 variables; we'll use this predictors set for multiple imputation and recreate these variables after
           N_vroeg_24_37, vooraf_zw_vroeg_24_37,
           N_vooraf_sga, vooraf_sga,
           interpreg_cat, amddd1ond_cat, grav_cat,
           lft_concep, age_at_concp_fa, # instead age_at_concp_mo_cat, age_at_concp_fa_cat,
           COROP2020_mo, STED_mo, educationlevel_mo, educationlevel_fa, 
           house_ownership_mo, plhh_partner_child, 
           income_hh_mo, # we made the choice to include household income of the mother (instead of personal income) as household income might have more of an effect on preterm birth 
           year_conception, # use this for income categorisation after MI
           par # use this for checking data validity after MI
  ))


# If multiple imputations have been generated before, you can simply load the mids object, skip to line 150

#### 1. Prepare predictors data for multiple imputation ####
table(predictors$N_vroeg_28_34)
#Prepare dataset, recode few variables to avoid very small cell counts in categorical variables
predictors<-predictors%>%
  mutate(
    N_vroeg_28_34 = recode(N_vroeg_28_34, 
                           '1' = '1',
                           '2' = '2',
                           '3' = '3+',
                           '4' = '3+', 
                           '5' = '3+', 
                           '6' = '3+'),
    N_vroeg_34_37 = recode(N_vroeg_34_37, 
                           '1' = '1',
                           '2' = '2',
                           '3' = '3+',
                           '4' = '3+', 
                           '5' = '3+', 
                           '6' = '3+'),
    N_vooraf_sga = recode(N_vooraf_sga, 
                          '1' = '1',
                          '2' = '2',
                          '3' = '3',
                          '4' = '4', 
                          '5' = '5+', 
                          '6' = '5+', 
                          '7' = '5+'))
table(predictors$N_vroeg_24_28)
table(predictors$N_vroeg_28_34)
table(predictors$N_vroeg_34_37)

predictors <- predictors%>%
  mutate(
    N_vroeg_24_37 = fct_collapse(N_vroeg_24_37, "3+" = c("4", "5")))
table(predictors$N_vroeg_24_37)

# consider including household income of the mother as a categorical variable
# to create the categories we use year-dependent cutoffs at the 10th, 40th and 80th percentile
hh_income_perc_year_cbs <- readRDS("case1_preterm_birth/data/hh_income_perc_year_cbs.rds")
hh_income_perc_year_cbs <- hh_income_perc_year_cbs %>% 
  select(c(year, perc_10, perc_40, perc_80)) %>% 
  mutate(year = as.factor(year))

predictors <- predictors%>% 
  left_join(hh_income_perc_year_cbs, by = c("year_conception" = "year")) %>% 
  mutate(income_hh_mo_cat = case_when(
    income_hh_mo < perc_10 ~ "bestaansmin",
    income_hh_mo < perc_40 ~ "laag",
    income_hh_mo < perc_80 ~ "midden",
    income_hh_mo >= perc_80 ~ "hoog",
    TRUE ~ NA_character_),
    income_hh_mo_cat = as.factor(income_hh_mo_cat)) %>% 
  select(-c(perc_10, perc_40, perc_80, year_conception, income_hh_mo))

table(predictors$income_hh_mo_cat)
sum(is.na(predictors$income_hh_mo_cat))

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



predictors<-predictors%>%
  mutate(
    # in consultation with birth care professionals we decided to include age of father as a categorical variable, 
    # and create a category for cases in which the father is missing
    age_at_concp_fa_cat = cut(age_at_concp_fa, 
                              breaks = seq(min(age_at_concp_fa), max(age_at_concp_fa), by = 5),
                              include.lowest = TRUE,
                              # use these two lines for train data 
                              labels = paste(seq(min(age_at_concp_fa), max(age_at_concp_fa), by = 5)[-length(seq(min(age_at_concp_fa), max(age_at_concp_fa), by = 5))],
                                             "-", seq(min(age_at_concp_fa), max(age_at_concp_fa), by = 5)[-1])),
    
    # use these two lines for test data (temp solution)
    # labels = paste(seq(11, 81, by = 5)[-length(seq(11, 81, by = 5))],
    #                "-", seq(11, 81, by = 5)[-1])),
    
    age_at_concp_fa_cat = if_else(NA_dummy_age_at_concp_fa == 0, "vader_NA", age_at_concp_fa_cat),
    age_at_concp_fa_cat = as.factor(age_at_concp_fa_cat),
    age_at_concp_fa_cat = fct_collapse(age_at_concp_fa_cat, "71+" = c("71 - 76",  "76 - 81")),
    age_at_concp_fa_cat = if_else(is.na(age_at_concp_fa_cat), "71+", age_at_concp_fa_cat), # temp solution because cut deletes highest observation, age 89, we do want to include this individual in the 71+ group
    age_at_concp_fa_cat = as.factor(age_at_concp_fa_cat),
    
    # we incorporate gravidity as a categorical variable and create a 10+ category
   )
table(predictors$age_at_concp_fa_cat)

predictors<-predictors%>%
  select(-c(age_at_concp_fa, NA_dummy_age_at_concp_fa))

predictors<-predictors%>%
  select(-c(NA_dummy_par))

summary(predictors)

### 3. Create train and test split for the rf_rest1 models

# set seed for reproducibility
set.seed(8151)

# assign split value per row, with probability of 0.7 for train and 0.3 for test
split <- sample(c(TRUE, FALSE), nrow(predictors), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
pretbirth_train <- predictors[split, ]
pretbirth_test <- predictors[!split, ]

# create seperate multi and primi test sets 
table(pretbirth_test$par)
pretbirth_test_multi <- pretbirth_test %>% 
  filter(par != "0")
pretbirth_test_primi <- pretbirth_test %>% 
  filter(par == "0")

# exclude par_cat from analysis because this is unknown at 16 weeks gestational age
pretbirth_train <- pretbirth_train %>% select(-c(par))
pretbirth_test <- pretbirth_test %>% select(-c(par))
pretbirth_test_multi <- pretbirth_test_multi %>% select(-c(par))
pretbirth_test_primi <- pretbirth_test_primi %>% select(-c(par))

#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# Train the random forest model
rf_rest1 <- ranger(premature_birth ~ . , data = pretbirth_train, num.trees = 1000, #mtry = 5, min.node.size = 15,
                  importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object. 
# saveRDS(rf_rest1, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_rest1_32.rds")

# look at the trained model
rf_rest1
rf_rest1$variable.importance
rf_rest1$prediction.error

# variable importance scores show how 'important' a variable is in predicting the PTB outcome
# look at variable importance score, ordered and standardized (% out of 100%)  
var_imp_norm <- data.frame(var = names(rf_rest1$variable.importance), # make sure to not include the outcome variable here 
                           imp = rf_rest1$variable.importance/sum(rf_rest1$variable.importance)*100) %>% 
  arrange(desc(imp))
# create a plot of the Nth most important variables
var_imp_norm[1:22,] %>% # specify the N 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF model variable importance plot PTB <32w rf_rest1 model") # change title if you use PTB of <37w as an outcome

write_excel_csv2(var_imp_norm, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/RF_VarImp_32rf_rest1.csv")

# looking at predictions in the trained model 
head(rf_rest1$predictions)
summary(rf_rest1$predictions)
hist(rf_rest1$predictions[,2], xlim = c(0, 1))
prop.table(table(predictors$premature_birth))*100

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds 0.07 (threshold at prevalence = 0.82), 0.025 (threshold for 9% highest risks), (threshold for 15% highest risks)
rf_rest1_prob <- ifelse(rf_rest1$predictions[,2] > 0.0645, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest1_cm <- confusionMatrix(rf_rest1_prob, pretbirth_train$premature_birth, positive = "1")
rf_rest1_cm
rf_rest1_cmp <- round(prop.table(confusionMatrix(rf_rest1_prob, pretbirth_train$premature_birth, positive = "1")$table),4)*100
rf_rest1_cmp


#### 4.2 Test the random forest model ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance
# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_rest1 <- predict(rf_rest1, data = pretbirth_test)
saveRDS(pred_rf_rest1, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_rest1.rds")
# look at the predicted probabilities
summary(pred_rf_rest1$predictions[,2])
hist(pred_rf_rest1$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_rest1_class <- ifelse(pred_rf_rest1$predictions[,2] > 0.0638, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest1_pred_cm <- confusionMatrix(pred_rf_rest1_class, pretbirth_test$premature_birth, positive = "1")
rf_rest1_pred_cm
rf_rest1_pred_cmp <- round(prop.table(confusionMatrix(pred_rf_rest1_class, pretbirth_test$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_cmp

# try different thresholds, e.g. > 0.01974 10% highest risks
pred_rf_rest1_class2 <- ifelse(pred_rf_rest1$predictions[,2] > 0.01974, 1, 0) %>% as.factor()
rf_rest1_pred_cm2 <- confusionMatrix(pred_rf_rest1_class2, pretbirth_test$premature_birth, positive = "1")
rf_rest1_pred_cm2
rf_rest1_pred_cmp2 <- round(prop.table(confusionMatrix(pred_rf_rest1_class2, pretbirth_test$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_cmp2

# try different thresholds, e.g. > 0.0144 approx 20% highest risks
pred_rf_rest1_class15 <- ifelse(pred_rf_rest1$predictions[,2] > 0.0144, 1, 0) %>% as.factor()
rf_rest1_pred_cm15 <- confusionMatrix(pred_rf_rest1_class15, pretbirth_test$premature_birth, positive = "1")
rf_rest1_pred_cm15
rf_rest1_pred_cmp15 <- round(prop.table(confusionMatrix(pred_rf_rest1_class15, pretbirth_test$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_cmp15

# look at the area under the ROC curve to assess overall model performance
rf_rest1_roc <- roc(pretbirth_test$premature_birth, pred_rf_rest1$predictions[,2])
auc(rf_rest1_roc)
plot(rf_rest1_roc, print.auc = TRUE)
saveRDS(rf_rest1_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_rest1_32_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_cdf <- pretbirth_test
PTB_test_cdf$pred_prob <- pred_rf_rest1$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_cdf_1 <- PTB_test_cdf %>% filter(premature_birth == 1)
PTB_test_cdf_0 <- PTB_test_cdf %>% filter(premature_birth == 0)
summary(PTB_test_cdf_1$pred_prob)
summary(PTB_test_cdf_0$pred_prob)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_cdf_1_list <- PTB_test_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_cdf_0_list <- PTB_test_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_cdf_1_list.csv")
write_excel_csv2(PTB_test_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
pred_rf_rest1_multi <- predict(rf_rest1, data = pretbirth_test_multi)
saveRDS(pred_rf_rest1_multi, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_rest1_32_multi.rds")
# look at the predicted probabilities
summary(pred_rf_rest1_multi$predictions[,2])
hist(pred_rf_rest1_multi$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_rest1_multi_class <- ifelse(pred_rf_rest1_multi$predictions[,2] > 0.068, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest1_pred_multi_cm <- confusionMatrix(pred_rf_rest1_multi_class, pretbirth_test_multi$premature_birth, positive = "1")
rf_rest1_pred_multi_cm
rf_rest1_pred_multi_cmp <- round(prop.table(confusionMatrix(pred_rf_rest1_multi_class, pretbirth_test_multi$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_multi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_rest1_multi_class2 <- ifelse(pred_rf_rest1_multi$predictions[,2] >= 0.025, 1, 0) %>% as.factor()
rf_rest1_pred_multi_cm2 <- confusionMatrix(pred_rf_rest1_multi_class2, pretbirth_test_multi$premature_birth, positive = "1")
rf_rest1_pred_multi_cm2
rf_rest1_pred_multi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_rest1_multi_class2, pretbirth_test_multi$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_multi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_rest1_multi_roc <- roc(pretbirth_test_multi$premature_birth, pred_rf_rest1_multi$predictions[,2])
auc(rf_rest1_multi_roc)
plot(rf_rest1_multi_roc, print.auc = TRUE)
saveRDS(rf_rest1_multi_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_rest1_32_multi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_multi_cdf <- pretbirth_test_multi
PTB_test_multi_cdf$pred_prob <- pred_rf_rest1_multi$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_multi_cdf_1 <- PTB_test_multi_cdf %>% filter(premature_birth == 1)
PTB_test_multi_cdf_0 <- PTB_test_multi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_multi_cdf_1_list <- PTB_test_multi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_multi_cdf_0_list <- PTB_test_multi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_multi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_multi_cdf_1_list.csv")
write_excel_csv2(PTB_test_multi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_multi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.4 Test the random forest model: sample of primipara   ####
# we use the previously trained RF model and test it on a sample of primipara
# create a sample of only primipara (not the first pregnancy)
pred_rf_rest1_primi <- predict(rf_rest1, data = pretbirth_test_primi)
saveRDS(pred_rf_rest1_primi, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_rest1_32_primi.rds")
# look at the predicted probabilities
summary(pred_rf_rest1_primi$predictions[,2])
hist(pred_rf_rest1_primi$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_rest1_primi_class <- ifelse(pred_rf_rest1_primi$predictions[,2] > 0.068, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest1_pred_primi_cm <- confusionMatrix(pred_rf_rest1_primi_class, pretbirth_test_primi$premature_birth, positive = "1")
rf_rest1_pred_primi_cm
rf_rest1_pred_primi_cmp <- round(prop.table(confusionMatrix(pred_rf_rest1_primi_class, pretbirth_test_primi$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_primi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_rest1_primi_class2 <- ifelse(pred_rf_rest1_primi$predictions[,2] > 0.025, 1, 0) %>% as.factor()
rf_rest1_pred_primi_cm2 <- confusionMatrix(pred_rf_rest1_primi_class2, pretbirth_test_primi$premature_birth, positive = "1")
rf_rest1_pred_primi_cm2
rf_rest1_pred_primi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_rest1_primi_class2, pretbirth_test_primi$premature_birth, positive = "1")$table),4)*100
rf_rest1_pred_primi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_rest1_primi_roc <- roc(pretbirth_test_primi$premature_birth, pred_rf_rest1_primi$predictions[,2])
auc(rf_rest1_primi_roc)
plot(rf_rest1_primi_roc, print.auc = TRUE)
saveRDS(rf_rest1_primi_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_rest1_32_primi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_primi_cdf <- pretbirth_test_primi
PTB_test_primi_cdf$pred_prob <- pred_rf_rest1_primi$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_primi_cdf_1 <- PTB_test_primi_cdf %>% filter(premature_birth == 1)
PTB_test_primi_cdf_0 <- PTB_test_primi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_primi_cdf_1_list <- PTB_test_primi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_primi_cdf_0_list <- PTB_test_primi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_primi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_primi_cdf_1_list.csv")
write_excel_csv2(PTB_test_primi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_rest1_primi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability




#### Medical birth records model  ####
# this model includes medical records available in birth care clinical practice such as birth history, age of the pregnant individual and care history
# The underlying intention is to compare the restricted model with medical birth records and socio-economic factors, 
# to this prediction model that includes only the medical birth records, in order to say something about the added value of 
# socio-economic factors. 

#### 3. Create the data set for the medical birth records model ####
# select only the medical birth records not the socio-economic factors 
pretbirth_train_medical <- pretbirth_train %>% 
  select(-c(educationlevel_mo, educationlevel_fa, COROP2020_mo, STED_mo, 
            house_ownership_mo, plhh_partner_child, income_hh_mo_cat, age_at_concp_fa_cat))

pretbirth_test_medical <- pretbirth_test %>% 
  select(-c(educationlevel_mo, educationlevel_fa, COROP2020_mo, STED_mo, 
            house_ownership_mo, plhh_partner_child, income_hh_mo_cat, age_at_concp_fa_cat))

pretbirth_test_multi_medical <- pretbirth_test_multi %>% 
  select(-c(educationlevel_mo, educationlevel_fa, COROP2020_mo, STED_mo, 
            house_ownership_mo, plhh_partner_child, income_hh_mo_cat, age_at_concp_fa_cat))

pretbirth_test_primi_medical <- pretbirth_test_primi %>% 
  select(-c(educationlevel_mo, educationlevel_fa, COROP2020_mo, STED_mo, 
            house_ownership_mo, plhh_partner_child, income_hh_mo_cat, age_at_concp_fa_cat))

#### 4. Random Forest model ####

#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# Train the random forest model
rf_medical1 <- ranger(premature_birth ~ . , data = pretbirth_train_medical, num.trees = 1000, #mtry = 5, min.node.size = 15,
                   importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object. 
# saveRDS(rf_medical1, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_medical1_32.rds")

# look at the trained model
rf_medical1
rf_medical1$variable.importance
rf_medical1$prediction.error

# variable importance scores show how 'important' a variable is in predicting the PTB outcome
# look at variable importance score, ordered and standardized (% out of 100%)  
var_imp_norm <- data.frame(var = names(rf_medical1$variable.importance), # make sure to not include the outcome variable here 
                           imp = rf_medical1$variable.importance/sum(rf_medical1$variable.importance)*100) %>% 
  arrange(desc(imp))
# create a plot of the Nth most important variables
var_imp_norm[1:14,] %>% # specify the N 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF model variable importance plot PTB <32w rf_medical1 model") # change title if you use PTB of <37w as an outcome

write_excel_csv2(var_imp_norm, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/RF_VarImp_32rf_medical1.csv")

# looking at predictions in the trained model 
head(rf_medical1$predictions)
summary(rf_medical1$predictions)
hist(rf_medical1$predictions[,2], xlim = c(0, 1))
prop.table(table(predictors$premature_birth))*100

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of PTB), otherwise negative =0 (low risk of PTB)
# try different thresholds 0.07 (threshold at prevalence = 0.82), 0.025 (threshold for 9% highest risks), (threshold for 15% highest risks)
rf_medical1_prob <- ifelse(rf_medical1$predictions[,2] > 0.0645, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_medical1_cm <- confusionMatrix(rf_medical1_prob, pretbirth_train_medical$premature_birth, positive = "1")
rf_medical1_cm
rf_medical1_cmp <- round(prop.table(confusionMatrix(rf_medical1_prob, pretbirth_train_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_cmp


#### 4.2 Test the random forest model ####
# Use the trained model to predict individual outcomes in the test data & evaluate predictive performance
# predict preterm_birth outcomes in test data using probability estimation 
pred_rf_medical1_medical <- predict(rf_medical1, data = pretbirth_test_medical)
saveRDS(pred_rf_medical1_medical, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_medical1_medical.rds")
# look at the predicted probabilities
summary(pred_rf_medical1_medical$predictions[,2])
hist(pred_rf_medical1_medical$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_medical1_class <- ifelse(pred_rf_medical1_medical$predictions[,2] > 0.0638, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_medical1_pred_cm <- confusionMatrix(pred_rf_medical1_class, pretbirth_test_medical$premature_birth, positive = "1")
rf_medical1_pred_cm
rf_medical1_pred_cmp <- round(prop.table(confusionMatrix(pred_rf_medical1_class, pretbirth_test_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_cmp

# try different thresholds, e.g. > 0.01974 10% highest risks
pred_rf_medical1_class2 <- ifelse(pred_rf_medical1_medical$predictions[,2] > 0.01974, 1, 0) %>% as.factor()
rf_medical1_pred_cm2 <- confusionMatrix(pred_rf_medical1_class2, pretbirth_test_medical$premature_birth, positive = "1")
rf_medical1_pred_cm2
rf_medical1_pred_cmp2 <- round(prop.table(confusionMatrix(pred_rf_medical1_class2, pretbirth_test_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_cmp2

# try different thresholds, e.g. > 0.0144 approx 20% highest risks
pred_rf_medical1_class15 <- ifelse(pred_rf_medical1_medical$predictions[,2] > 0.0144, 1, 0) %>% as.factor()
rf_medical1_pred_cm15 <- confusionMatrix(pred_rf_medical1_class15, pretbirth_test_medical$premature_birth, positive = "1")
rf_medical1_pred_cm15
rf_medical1_pred_cmp15 <- round(prop.table(confusionMatrix(pred_rf_medical1_class15, pretbirth_test_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_cmp15

# look at the area under the ROC curve to assess overall model performance
rf_medical1_roc <- roc(pretbirth_test_medical$premature_birth, pred_rf_medical1_medical$predictions[,2])
auc(rf_medical1_roc)
plot(rf_medical1_roc, print.auc = TRUE)
saveRDS(rf_medical1_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_medical1_32_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_cdf <- pretbirth_test_medical
PTB_test_cdf$pred_prob <- pred_rf_medical1_medical$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_cdf_1 <- PTB_test_cdf %>% filter(premature_birth == 1)
PTB_test_cdf_0 <- PTB_test_cdf %>% filter(premature_birth == 0)
summary(PTB_test_cdf_1$pred_prob)
summary(PTB_test_cdf_0$pred_prob)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_cdf_1_list <- PTB_test_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_cdf_0_list <- PTB_test_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_cdf_1_list.csv")
write_excel_csv2(PTB_test_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.3 Test the random forest model: sample of multipara  ####
# we use the previously trained RF model and test it on a sample of multipara
pred_rf_medical1_multi_medical <- predict(rf_medical1, data = pretbirth_test_multi_medical)
saveRDS(pred_rf_medical1_multi_medical, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_medical1_32_multi.rds")
# look at the predicted probabilities
summary(pred_rf_medical1_multi_medical$predictions[,2])
hist(pred_rf_medical1_multi_medical$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_medical1_multi_class <- ifelse(pred_rf_medical1_multi_medical$predictions[,2] > 0.068, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_medical1_pred_multi_cm <- confusionMatrix(pred_rf_medical1_multi_class, pretbirth_test_multi_medical$premature_birth, positive = "1")
rf_medical1_pred_multi_cm
rf_medical1_pred_multi_cmp <- round(prop.table(confusionMatrix(pred_rf_medical1_multi_class, pretbirth_test_multi_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_multi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_medical1_multi_class2 <- ifelse(pred_rf_medical1_multi_medical$predictions[,2] >= 0.025, 1, 0) %>% as.factor()
rf_medical1_pred_multi_cm2 <- confusionMatrix(pred_rf_medical1_multi_class2, pretbirth_test_multi_medical$premature_birth, positive = "1")
rf_medical1_pred_multi_cm2
rf_medical1_pred_multi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_medical1_multi_class2, pretbirth_test_multi_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_multi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_medical1_multi_roc <- roc(pretbirth_test_multi_medical$premature_birth, pred_rf_medical1_multi_medical$predictions[,2])
auc(rf_medical1_multi_roc)
plot(rf_medical1_multi_roc, print.auc = TRUE)
saveRDS(rf_medical1_multi_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_medical1_32_multi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_multi_cdf <- pretbirth_test_multi_medical
PTB_test_multi_cdf$pred_prob <- pred_rf_medical1_multi_medical$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_multi_cdf_1 <- PTB_test_multi_cdf %>% filter(premature_birth == 1)
PTB_test_multi_cdf_0 <- PTB_test_multi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_multi_cdf_1_list <- PTB_test_multi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_multi_cdf_0_list <- PTB_test_multi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_multi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_multi_cdf_1_list.csv")
write_excel_csv2(PTB_test_multi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_multi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


#### 3.4 Test the random forest model: sample of primipara   ####
# we use the previously trained RF model and test it on a sample of primipara
# create a sample of only primipara (not the first pregnancy)
pred_rf_medical1_primi_medical <- predict(rf_medical1, data = pretbirth_test_primi_medical)
saveRDS(pred_rf_medical1_primi_medical, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/pred_rf_medical1_32_primi.rds")
# look at the predicted probabilities
summary(pred_rf_medical1_primi_medical$predictions[,2])
hist(pred_rf_medical1_primi_medical$predictions[,2])

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of PTB for approx. 0.82% of pregnancies
pred_rf_medical1_primi_class <- ifelse(pred_rf_medical1_primi_medical$predictions[,2] > 0.068, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_medical1_pred_primi_cm <- confusionMatrix(pred_rf_medical1_primi_class, pretbirth_test_primi_medical$premature_birth, positive = "1")
rf_medical1_pred_primi_cm
rf_medical1_pred_primi_cmp <- round(prop.table(confusionMatrix(pred_rf_medical1_primi_class, pretbirth_test_primi_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_primi_cmp

# try different thresholds, e.g. 0.025 approx. 9% highest risks
pred_rf_medical1_primi_class2 <- ifelse(pred_rf_medical1_primi_medical$predictions[,2] > 0.025, 1, 0) %>% as.factor()
rf_medical1_pred_primi_cm2 <- confusionMatrix(pred_rf_medical1_primi_class2, pretbirth_test_primi_medical$premature_birth, positive = "1")
rf_medical1_pred_primi_cm2
rf_medical1_pred_primi_cmp2 <- round(prop.table(confusionMatrix(pred_rf_medical1_primi_class2, pretbirth_test_primi_medical$premature_birth, positive = "1")$table),4)*100
rf_medical1_pred_primi_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_medical1_primi_roc <- roc(pretbirth_test_primi_medical$premature_birth, pred_rf_medical1_primi_medical$predictions[,2])
auc(rf_medical1_primi_roc)
plot(rf_medical1_primi_roc, print.auc = TRUE)
saveRDS(rf_medical1_primi_roc, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/rf_medical1_32_primi_roc.rds") 

### save the predicted probabilities by outcome
PTB_test_primi_cdf <- pretbirth_test_primi_medical
PTB_test_primi_cdf$pred_prob <- pred_rf_medical1_primi_medical$predictions[,2]

# look at predicted probabilities by PTB outcome
PTB_test_primi_cdf_1 <- PTB_test_primi_cdf %>% filter(premature_birth == 1)
PTB_test_primi_cdf_0 <- PTB_test_primi_cdf %>% filter(premature_birth == 0)

# arrange the predicted probabilities per PTB outcome and save as csv
PTB_test_primi_cdf_1_list <- PTB_test_primi_cdf_1 %>% select(c(pred_prob)) %>% arrange(pred_prob)
PTB_test_primi_cdf_0_list <- PTB_test_primi_cdf_0 %>% select(c(pred_prob)) %>% arrange(pred_prob)

write_excel_csv2(PTB_test_primi_cdf_1_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_primi_cdf_1_list.csv")
write_excel_csv2(PTB_test_primi_cdf_0_list, "case1_preterm_birth/repo/PTB_output/temp_NAdummy/PTB32_medical1_primi_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability













