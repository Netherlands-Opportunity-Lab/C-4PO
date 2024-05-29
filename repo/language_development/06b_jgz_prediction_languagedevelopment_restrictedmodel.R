#### JGZ DATA ANALYSIS ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 06_jgz_prediction_languagedevelopment.R ####
# THE GOAL OF THIS SCRIPT IS TO PREDICT LANUAGE DEVELOPMENT

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
# 06_ANALYSIS: PREDICT LANGUAGE DEVELOPMENT
#################################################################################
#### RESTRICTED MODEL ####
# this model includes a restricted set of variables; the restricted model includes only variables that are available 
# in a clinical, health care setting (EHR, and are currently registered by JGZ) or can reasonably be collected in this setting
# Additionally, the full model should indicate that these variables are the most relevant in predicting the outcome

#### 1. Look at the set of predictors (before multiple imputation) ####
predictors <- readRDS("case_jgz/data_outcome/wiechen/wiechen_predictors_p.rds")

# look at the outcome variable: taal_ont3y9m
table(predictors$taal_ont3y9m)
prop.table(table(predictors$taal_ont3y9m)) * 100

predictors %>% 
  group_by(taal_ont3y9m, geslacht) %>% 
  summarise(n = n())

#### 2. Dealing with missing data - multiple imputation with MICE ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# To deal with missing data, we used the MICE algorithm for multiple imputation: see 05_jgz_preprocess_MI

# read the multiple imputed train data for the case language development created in 05_jgz_preprocess_MI
mi_complete <- readRDS("case_jgz/repo/language_development/data/taal_spraak_MI_complete.rds")
# look at the data set
glimpse(mi_complete)

# read the multiple imputed test data 
mi_test_comp <- readRDS("case_jgz/repo/language_development/data/taal_spraak_MI_test_comp.rds")

# set seed for reproducibility
set.seed(8151)

#### 3. Create the restricted data set for analysis ####
# select the variables that you want to include in the restricted model 
# 3.1 For train data 
mi_rest <- mi_complete %>% 
  lapply(select, c(taal_ont3y9m,
                   # select the variables from the JGZ data
                   # that are used in current practice 
                   geslacht,
                   bst_nl, zin2w, pop6, taalomg,
                   educc_mo, educc_fa
                   # if you want to include more variables in the model
                   # you can add them here
                   )) 

# look at the education level data, if there are too little observations in 1_geen
# collapse categories 1_geen and 2_basis
table(mi_rest$`1`$educc_fa)
func_educc_collapse <- function(mi_list){
  mi_list %>% 
    mutate(educc_fa = fct_collapse(educc_fa, "1_2basis" = c("1_geen", "2_basis")),
           educc_mo = fct_collapse(educc_mo, "1_2basis" = c("1_geen", "2_basis")))
}
# apply the function to the m = 5 data sets and check to see the result
mi_rest <- lapply(mi_rest, func_educc_collapse)
table(mi_rest$`3`$educc_fa)

# 3.2 For test data 
mi_test_rest <- mi_test_comp %>% 
  lapply(select, c(taal_ont3y9m,
                   # select the variables from the JGZ data
                   # that are used in current practice 
                   geslacht,
                   bst_nl, zin2w, pop6, taalomg,
                   educc_mo, educc_fa
                   # if you want to include more variables in the model
                   # you can add them here
  )) 

# collapse education level categories
table(mi_test_rest$`1`$educc_fa)
mi_test_rest <- lapply(mi_test_rest, func_educc_collapse)



#### 4. Random Forest model ####


#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 
# train random forest model on restricted data
rf_rest <- mi_rest %>% 
  lapply(ranger, formula = taal_ont3y9m ~ .,
         num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)

# you can save the ranger object but this is a very big object.
saveRDS(rf_rest, "case_jgz/repo/language_development/output/rest/rf_rest.rds")

# look at the trained models. Remember you have m = 5 trained models
rf_rest$`1`
rf_rest$`3`
rf_rest$`5`
rf_rest$`1`$variable.importance
rf_rest$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the outcome delay in language development
# look at variable importance score, ordered and standardized (% out of 100%) 
var_imp_norm1 <- data.frame(var = names(rf_rest$`1`$variable.importance),
                            imp = rf_rest$`1`$variable.importance/sum(rf_rest$`1`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm2 <- data.frame(var = names(rf_rest$`2`$variable.importance),
                            imp = rf_rest$`2`$variable.importance/sum(rf_rest$`2`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm3 <- data.frame(var = names(rf_rest$`3`$variable.importance),
                            imp = rf_rest$`3`$variable.importance/sum(rf_rest$`3`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm4 <- data.frame(var = names(rf_rest$`4`$variable.importance),
                            imp = rf_rest$`4`$variable.importance/sum(rf_rest$`4`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm5 <- data.frame(var = names(rf_rest$`5`$variable.importance),
                            imp = rf_rest$`5`$variable.importance/sum(rf_rest$`5`$variable.importance)*100) %>% 
  arrange(desc(imp))

# create a plot of the Nth most important variables
# change var_imp_norm1 to 2, 3, 4 or 5 if you want to look at the plots for the other m's
var_imp_norm1[1:7,] %>% 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF restricted model variable importance plot language development")

# QUESTION TO POLINA; HOW TO SAVE THIS, MULTIPLE SHEETS IN EXCEL FILE? OR MULTIPLE CSV. FILES?
## Polina: Excel file seems a good solution for me. 
varimp_sheet_names <- list('var_imp_norm1' = var_imp_norm1, 'var_imp_norm2' = var_imp_norm2, 'var_imp_norm3' = var_imp_norm3, 
                           'var_imp_norm4' = var_imp_norm4, 'var_imp_norm5' = var_imp_norm5)
openxlsx::write.xlsx(varimp_sheet_names, file = 'case_jgz/repo/language_development/output/rest/var_imp_norm.xlsx')

# looking at predictions in the trained model 
summary(rf_rest$`1`$predictions[,2])
hist(rf_rest$`1`$predictions[,2], xlim = c(0, 1))

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of delay in language development), 
# otherwise negative =0 (low risk of delay in language development)
# try different thresholds; threshold at prevalence = 13.16%, threshold at 0.2, potential other thresholds
# you can do this for all m = 5 rf_rest models by changing rf_rest$`1` to rf_rest$`2` or rf_rest$`3` or rf_rest$`4` or rf_rest$`5`
rf_rest_prob <- ifelse(rf_rest$`1`$predictions[,2] > 0.311, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest_cm <- confusionMatrix(rf_rest_prob, mi_rest$`1`$taal_ont3y9m, positive = "1")
rf_rest_cm
rf_rest_cmp <- round(prop.table(confusionMatrix(rf_rest_prob, mi_rest$`1`$taal_ont3y9m, positive = "1")$table),4)*100
rf_rest_cmp


#### 4.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# predict delay in language development in test data using probability estimation
pred_rf_rest <- lapply(rf_rest, function(model){
  predict(model, data = mi_test_rest$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use any of the other m's, it should not make much of a difference

# for each individual in the test set you obtain m = 5 predicted probabilities because you have trained 5 RF models on 5 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_rest_pool <- data.frame(
  pred_rf_rest1 = pred_rf_rest[[1]],
  pred_rf_rest2 = pred_rf_rest[[2]],
  pred_rf_rest3 = pred_rf_rest[[3]],
  pred_rf_rest4 = pred_rf_rest[[4]],
  pred_rf_rest5 = pred_rf_rest[[5]]
)

# pool the m = 5 predicted probabilities per individual by taking the mean
pred_rf_rest_pool <- pred_rf_rest_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_rest1, pred_rf_rest2, pred_rf_rest3, pred_rf_rest4, pred_rf_rest5), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_rest_pool, "case_jgz/repo/language_development/output/rest/pred_rf_rest_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of delay in language development for approx. 13.16% of children
pred_rf_rest_pool_class1 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.3015, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm1 <- confusionMatrix(pred_rf_rest_pool_class1, mi_test_rest$`1`$taal_ont3y9m, positive = "1")
pred_rf_rest_pool_class_cm1
pred_rf_rest_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class1, mi_test_rest$`1`$taal_ont3y9m, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp1

# try different thresholds: > 0.2 approx. 23.5% highest risks
pred_rf_rest_pool_class2 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.2, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm2 <- confusionMatrix(pred_rf_rest_pool_class2, mi_test_rest$`1`$taal_ont3y9m, positive = "1")
pred_rf_rest_pool_class_cm2
pred_rf_rest_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class2, mi_test_rest$`1`$taal_ont3y9m, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp2

# look at the area under the ROC curve to assess overall model performance
rf_rest_roc <- roc(mi_test_rest$'1'$taal_ont3y9m, pred_rf_rest_pool$pred_outcome_prob)
auc(rf_rest_roc)
plot(rf_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_rest_roc, "case_jgz/repo/language_development/output/rest/pred_rf_rest_pool.csv")
saveRDS(rf_rest_roc, "case_jgz/repo/language_development/output/rest/pred_rf_rest_pool.rds")


#### 4.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities
pred_rf_rest_pool$taal_ont3y9m <- mi_test_rest$'1'$taal_ont3y9m
table(pred_rf_rest_pool$taal_ont3y9m)
prop.table(table(pred_rf_rest_pool$taal_ont3y9m)) * 100

# look at predicted probabilities by PTB outcome
taal_rf_rest_pool_cdf_1 <- pred_rf_rest_pool %>% filter(taal_ont3y9m == 1)
taal_rf_rest_pool_cdf_0 <- pred_rf_rest_pool %>% filter(taal_ont3y9m == 0)

summary(taal_rf_rest_pool_cdf_1$pred_outcome_prob)
summary(taal_rf_rest_pool_cdf_0$pred_outcome_prob)

# plot the cdf
plot(ecdf(taal_rf_rest_pool_cdf_1$pred_outcome_prob), xlab = "predicted probability", do.points = FALSE)+
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(taal_rf_rest_pool_cdf_1$pred_prob), col = "red")+
  abline(v = median(taal_rf_rest_pool_cdf_1$pred_prob), col = "orange")

plot(ecdf(taal_rf_rest_pool_cdf_0$pred_outcome_prob), xlab = "predicted probability") +
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(taal_rf_rest_pool_cdf_0$pred_prob), col = "red")+
  abline(v = median(taal_rf_rest_pool_cdf_0$pred_prob), col = "orange")

# arrange the predicted probabilities per PTB outcome and save as csv
taal_rf_rest_pool_cdf_1_list <- taal_rf_rest_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
taal_rf_rest_pool_cdf_0_list <- taal_rf_rest_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(taal_rf_rest_pool_cdf_1_list, "case_jgz/repo/language_development/output/rest/taal_rf_rest_pool_cdf_1_list.csv")
write_excel_csv2(taal_rf_rest_pool_cdf_0_list, "case_jgz/repo/language_development/output/rest/taal_rf_rest_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


# Look at specificity and sensitivity of the prediction model for different thresholds of predicted probability 
# create 100 bins, take the mean of predicted probability and look at the percentage of children with delayed language development for each bin
predprob_bins <- data.frame(predprob = rf_rest_roc$predictor, taal_ont3y9m = rf_rest_roc$response) 
predprob_bins$bin100 <- ntile(rf_rest_roc$predictor, n = 100)

predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          outcome_1 = sum(taal_ont3y9m == 1),
          outcome_0 = sum(taal_ont3y9m == 0),
          outcome1_perc = sum(taal_ont3y9m == 1) / (sum(taal_ont3y9m == 1)+sum(taal_ont3y9m == 0))*100) %>% 
  mutate(outcome1_perc = ifelse((outcome1_perc/100)*n < 10, NA, outcome1_perc))

view(predprob_bins100)
# look at the distribution of predicted probabilities 
plot(x = predprob_bins100$bin100, y = predprob_bins100$predprob_mean)
# look at the percentage of children with delayed language development per bin (most are NA because N<10)
plot(x = predprob_bins100$bin100, y = predprob_bins100$outcome1_perc)

# For the 100 different predicted probability thresholds, calculate specificity and sensitivity
predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest_roc$response, positive = "1")
  pred_rf_class_cmi
  
  predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i,
                                     sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                     specificity = pred_rf_class_cmi[[4]][[2]])
  
  predprob_bins100_df <- rbind(predprob_bins100_df, predprob_bins100_dfi)
  #print(predprob_bins100_df)
}

# add the sensitivity and specificy for the different thresholds to the predprob_bins100 table
predprob_bins100_df <- cbind(predprob_bins100, predprob_bins100_df)
sum(predprob_bins100_df$predprob_mean == predprob_bins100_df$predprob_mean_i)
predprob_bins100_df <- predprob_bins100_df %>% select(-c(outcome_1, outcome_0, predprob_mean_i))
# save the table
write_excel_csv2(predprob_bins100_df, "case_jgz/repo/language_development/output/rest/taal_rf_rest_predprob_bins100_df.csv")
# look at the table
view(predprob_bins100_df)

# plot the predicted probability for the 100 bins on x-axis and sensitivity or specificity on y-axis
# save the plot as png
png(file = "output/rest/taal_rf_rest_predprob_bins100_sens_spec.png",
    width= 600, height = 450)
ggplot(predprob_bins100_df, show.legend = TRUE)+
  geom_point(aes(x = predprob_mean, y = specificity, colour = 'specificity'))+
  geom_point(aes(x = predprob_mean, y = sensitivity, colour = 'sensitivity'))+
  ggtitle("taal_rf_rest_predprob_bins100_ysens_spec")+
  labs(y = 'specificity and sensitivity')



#### 5. Logistic regression model ####
# we also train and test a logistic regression model and consider its predictive performance in the case of language development
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

#### 5.1 Train the Logistic regression model using glm ###
glm_rest <- mi_rest %>% #! USE FACTORS NOT ORDERED 
  lapply(glm, formula = taal_ont3y9m ~ ., family = binomial)
# look at the summary of the trained model by pooling the m =5 logistic regression models
glm_rest_pool <- summary(pool(glm_rest))
glm_rest_pool

# you can save the trained model but this is a very big object
#saveRDS(glm_rest, "case_jgz/repo/language_development/output/rest/glm_rest.rds")
# maybe save the pooled summary instead
write_excel_csv2(glm_rest_pool, "case_jgz/repo/language_development/output/rest/glm_rest_pool.csv")


#### 5.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# read the multiple imputed test data
pred_glm_rest <- predict(glm_rest, mi_test_rest$`1`)
# we use test set m = 1, you van also use m = 2 3 4 OR 5, it should not make much of a difference

# for each individual in the test set you obtain m = 5 predicted probabilities because you have trained 5 models on 5 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_glm_rest_pool <- data.frame(
  pred_glm_rest1 = pred_glm_rest[[1]],
  pred_glm_rest2 = pred_glm_rest[[2]],
  pred_glm_rest3 = pred_glm_rest[[3]],
  pred_glm_rest4 = pred_glm_rest[[4]],
  pred_glm_rest5 = pred_glm_rest[[5]]
)

# pool the m = 5 predicted probabilities per individual by taking the mean
pred_glm_rest_pool <- pred_glm_rest_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_glm_rest1, pred_glm_rest2, pred_glm_rest3, pred_glm_rest4, pred_glm_rest5), na.rm = T))

# for the logistic regression, compute the exponential function of the predicted probabilities to get a scale of 0-1
pred_glm_rest_pool <- pred_glm_rest_pool %>%
  mutate(pred_outcome_prob_exp = exp(pred_outcome_prob) / (1+exp(pred_outcome_prob)))

# save the predicted probabilities for future use
write_excel_csv2(pred_glm_rest_pool, "case_jgz/repo/language_development/output/rest/pred_glm_rest_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of delayed language development for approx. 13.16% of children
pred_glm_rest_pool_class1 <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.31, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm1 <- confusionMatrix(pred_glm_rest_pool_class1, mi_test_rest$`1`$taal_ont3y9m, positive = "1")
pred_glm_rest_pool_class_cm1
pred_glm_rest_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class1, mi_test_rest$`1`$taal_ont3y9m, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp1

# try different thresholds: > 0.2 approx. 22% highest risks 
pred_glm_rest_pool_class2 <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.2, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm2 <- confusionMatrix(pred_glm_rest_pool_class2, mi_test_rest$`1`$taal_ont3y9m, positive = "1")
pred_glm_rest_pool_class_cm2
pred_glm_rest_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class2, mi_test_rest$`1`$taal_ont3y9m, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp2

# look at the area under the ROC curve to assess overall model performance
glm_rest_roc <- roc(mi_test_rest$`1`$taal_ont3y9m, pred_glm_rest_pool$pred_outcome_prob_exp)
auc(glm_rest_roc)
plot(glm_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(glm_rest_roc, "case_jgz/repo/language_development/output/rest/pred_glm_rest_pool.csv")
saveRDS(glm_rest_roc, "case_jgz/repo/language_development/output/rest/pred_glm_rest_pool.rds")


#### 5.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities 
pred_glm_rest_pool$taal_ont3y9m <- mi_test_rest$`1`$taal_ont3y9m
table(pred_glm_rest_pool$taal_ont3y9m)
prop.table(table(pred_glm_rest_pool$taal_ont3y9m)) * 100

taal_glm_rest_pool_cdf_1 <- pred_glm_rest_pool %>% filter(taal_ont3y9m == 1)
taal_glm_rest_pool_cdf_0 <- pred_glm_rest_pool %>% filter(taal_ont3y9m == 0)

taal_glm_rest_pool_cdf_1_list <- taal_glm_rest_pool_cdf_1 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)
taal_glm_rest_pool_cdf_0_list <- taal_glm_rest_pool_cdf_0 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)

write_excel_csv2(taal_glm_rest_pool_cdf_1_list, "case_jgz/repo/language_development/output/rest/taal_glm_rest_pool_cdf_1_list.csv")
write_excel_csv2(taal_glm_rest_pool_cdf_0_list, "case_jgz/repo/language_development/output/rest/taal_glm_rest_pool_cdf_0_list.csv")


# Look at specificity and sensitivity of the prediction model for different thresholds of predicted probability. 
# create 100 bins, take the mean of predicted probability and look at the percentage of children with delayed language development for each bin
predprob_bins_glm <- data.frame(predprob = glm_rest_roc$predictor, taal_ont3y9m = glm_rest_roc$response) 
predprob_bins_glm$bin100 <- ntile(glm_rest_roc$predictor, n = 100)

predprob_bins_glm100 <- predprob_bins_glm %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          outcome_1 = sum(taal_ont3y9m == 1),
          outcome_0 = sum(taal_ont3y9m == 0),
          outcome1_perc = sum(taal_ont3y9m == 1) / (sum(taal_ont3y9m == 1)+sum(taal_ont3y9m == 0))*100) %>% 
  mutate(outcome1_perc = ifelse((outcome1_perc/100)*n < 10, NA, outcome1_perc))

view(predprob_bins_glm100)
# look at the distribution of predicted probabilities 
plot(x = predprob_bins_glm100$bin100, y = predprob_bins_glm100$predprob_mean)
# look at the percentage of children with delayed language development per bin (most are NA because N<10)
plot(x = predprob_bins_glm100$bin100, y = predprob_bins_glm100$outcome1_perc)

# For the 100 different predicted probability thresholds, calculate specificity and sensitivity
predprob_bins_glm100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(predprob_bins_glm100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in predprob_bins_glm100$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(glm_rest_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, glm_rest_roc$response, positive = "1")
  pred_rf_class_cmi
  
  predprob_bins_glm100_dfi <- data.frame(predprob_mean_i = predprob_i,
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  predprob_bins_glm100_df <- rbind(predprob_bins_glm100_df, predprob_bins_glm100_dfi)
  #print(predprob_bins_glm100_df)
}

# add the sensitivity and specificy for the different thresholds to the predprob_bins_glm100 table
predprob_bins_glm100_df <- cbind(predprob_bins_glm100, predprob_bins_glm100_df)
sum(predprob_bins_glm100_df$predprob_mean == predprob_bins_glm100_df$predprob_mean_i)
predprob_bins_glm100_df <- predprob_bins_glm100_df %>% select(-c(outcome_1, outcome_0, predprob_mean_i))
# save the table
write_excel_csv2(predprob_bins_glm100_df, "output/rest/taal_glm_rest_predprob_bins100_df.csv")
# look at the table
predprob_bins_glm100_df


#Plot the predicted probability for the 100 bins on x-axis and sensitivity or specificity on y-axis
# save the plot as png
png(file = "output/rest/taal_glm_rest_predprob_bins100_sens_spec.png",
    width= 600, height = 450)
ggplot(predprob_bins_glm100_df, show.legend = TRUE)+
  geom_point(aes(x = predprob_mean, y = specificity, colour = 'specificity'))+
  geom_point(aes(x = predprob_mean, y = sensitivity, colour = 'sensitivity'))+
  ggtitle("taal_glm_rest_predprob_bins100_ysens_spec")+
  labs(y = 'specificity and sensitivity')
dev.off()






# ! QUESTION TO POLINA: do we want to include the code that I wrote to compare the current practice
# with our restricted model? The part where I look at the overlap and the distinctions in predictions?
# that we included in the powerpoint

#Polina: Yes, would be great!
 ## models for current practice 1 and 2 + roc curves


################################################################################
#### Additional analysis ####

#### Comparing the restricted model to current VVE guidelines ####
# VVE = voor- en vroegschoolse educatie or preschool and early childhood education 
# We compare the trained restricted prediction model ('new care') with current care guidelines ('usual care') on language development in PYHC (preventive youth health care)

mi_test_rest_m1 <- mi_test_rest$'1'
mi_test_rest_m1$pred_outcome_prob <- pred_rf_rest_pool$pred_outcome_prob

# the current VVE guidelines offer three different scenario's
mi_test_rest_m1 <- mi_test_rest_m1 %>% 
         # scenario 1: VVE based on judgement of the professional (bst_nl)
  mutate(guideline_vve_prof = ifelse(bst_nl == "langzaam" | taalomg == "onvoldoende" | 
                                       ((educc_mo == "1_2basis" | educc_mo == "3_vmbopraktijk" | educc_mo == "4_vmbomavo") & 
                                          (educc_fa == "1_2basis" | educc_fa == "3_vmbopraktijk" | educc_fa == "4_vmbomavo")), 
                                     1, 0),
         guideline_vve_prof = as.factor(guideline_vve_prof),
         # scenario 2: VVE based on performance on van Wiechen items (zin2w, pop6)
         guideline_vve_wiechen = ifelse(zin2w == "-" | pop6 == "-" | taalomg == "onvoldoende" |
                                          ((educc_mo == "1_2basis" | educc_mo == "3_vmbopraktijk" | educc_mo == "4_vmbomavo") & 
                                             (educc_fa == "1_2basis" | educc_fa == "3_vmbopraktijk" | educc_fa == "4_vmbomavo")), 
                                        1, 0),
         guideline_vve_wiechen = as.factor(guideline_vve_wiechen),
         # scenario 3: a combination of judgement of professional and the van Wiechen items (scenario 1 and 2)
         guideline_vve_combi = ifelse(zin2w == "-" | pop6 == "-" | bst_nl == "langzaam" | taalomg == "onvoldoende" | 
                                        ((educc_mo == "1_2basis" | educc_mo == "3_vmbopraktijk" | educc_mo == "4_vmbomavo") & 
                                           (educc_fa == "1_2basis" | educc_fa == "3_vmbopraktijk" | educc_fa == "4_vmbomavo")), 
                                      1, 0),
         guideline_vve_combi = as.factor(guideline_vve_combi)) 

# threshold of predicted probability based on prevalence per scenario
perc_vve_prof <- sum(mi_test_rest_m1$guideline_vve_prof == "1")/ nrow(mi_test_rest_m1)*100
perc_vve_prof
threshold_predprob_prof <- quantile(mi_test_rest_m1$pred_outcome_prob, probs = (100-perc_vve_prof)/100)
threshold_predprob_prof
mi_test_rest_m1_prof <- ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_prof, 1, 0) %>% as.factor() 
mi_test_rest_m1_cm_prof <- confusionMatrix(mi_test_rest_m1_prof, mi_test_rest_m1$taal_ont3y9m, positive = "1")
mi_test_rest_m1_cm_prof
mi_test_rest_m1_cmp_prof <- round(prop.table(confusionMatrix(mi_test_rest_m1_prof, mi_test_rest_m1$taal_ont3y9m, positive = "1")$table),4)*100
mi_test_rest_m1_cmp_prof

perc_vve_wiechen <- sum(mi_test_rest_m1$guideline_vve_wiechen == "1")/ nrow(mi_test_rest_m1)*100
perc_vve_wiechen
threshold_predprob_wiechen <- quantile(mi_test_rest_m1$pred_outcome_prob, probs = (100-perc_vve_wiechen)/100)
threshold_predprob_wiechen
mi_test_rest_m1_wiechen <- ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_wiechen, 1, 0) %>% as.factor() 
mi_test_rest_m1_cm_wiechen <- confusionMatrix(mi_test_rest_m1_wiechen, mi_test_rest_m1$taal_ont3y9m, positive = "1")
mi_test_rest_m1_cm_wiechen
mi_test_rest_m1_cmp_wiechen <- round(prop.table(confusionMatrix(mi_test_rest_m1_wiechen, mi_test_rest_m1$taal_ont3y9m, positive = "1")$table),4)*100
mi_test_rest_m1_cmp_wiechen

perc_vve_combi <- sum(mi_test_rest_m1$guideline_vve_combi == "1")/ nrow(mi_test_rest_m1)*100
perc_vve_combi
threshold_predprob_combi <- quantile(mi_test_rest_m1$pred_outcome_prob, probs = (100-perc_vve_combi)/100)
threshold_predprob_combi
mi_test_rest_m1_combi <- ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_combi, 1, 0) %>% as.factor() 
mi_test_rest_m1_cm_combi <- confusionMatrix(mi_test_rest_m1_combi, mi_test_rest_m1$taal_ont3y9m, positive = "1")
mi_test_rest_m1_cm_combi
mi_test_rest_m1_cmp_combi <- round(prop.table(confusionMatrix(mi_test_rest_m1_combi, mi_test_rest_m1$taal_ont3y9m, positive = "1")$table),4)*100
mi_test_rest_m1_cmp_combi

mi_test_rest_m1 <- mi_test_rest_m1 %>% 
  mutate(guideline_vve_predp = ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_prof, 1, 0) %>% as.factor(),
         guideline_vve_predw = ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_wiechen, 1, 0) %>% as.factor(),
         guideline_vve_predc = ifelse(mi_test_rest_m1$pred_outcome_prob >= threshold_predprob_combi, 1, 0) %>% as.factor())

### new care vs usual care 1 judgement professional
sum(mi_test_rest_m1$guideline_vve_prof == "1" & mi_test_rest_m1$guideline_vve_predp == "1")
sum(mi_test_rest_m1$guideline_vve_prof == "0" & mi_test_rest_m1$guideline_vve_predp == "1")
sum(mi_test_rest_m1$guideline_vve_prof == "1" & mi_test_rest_m1$guideline_vve_predp == "0")

# kids indicated by both methods
overlap_new_usual_prof <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_prof == "1" & mi_test_rest_m1$guideline_vve_predp == "1")
table(overlap_new_usual_prof$taal_ont3y9m)
prop.table(table(overlap_new_usual_prof$taal_ont3y9m)) * 100
# kids indicated by 'new care' prediction model 
diff_new_prof <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_prof == "0" & mi_test_rest_m1$guideline_vve_predp == "1")
table(diff_new_prof$taal_ont3y9m)
prop.table(table(diff_new_prof$taal_ont3y9m)) * 100
# kids indicated by usual care VVE guidelines
diff_usual_prof <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_prof == "1" & mi_test_rest_m1$guideline_vve_predp == "0")
table(diff_usual_prof$taal_ont3y9m)
prop.table(table(diff_usual_prof$taal_ont3y9m)) * 100


### new care vs usual care 2 performance on van Wiechen items
sum(mi_test_rest_m1$guideline_vve_wiechen == "1" & mi_test_rest_m1$guideline_vve_predw == "1")
sum(mi_test_rest_m1$guideline_vve_wiechen == "0" & mi_test_rest_m1$guideline_vve_predw == "1")
sum(mi_test_rest_m1$guideline_vve_wiechen == "1" & mi_test_rest_m1$guideline_vve_predw == "0")

# kids indicated by both methods
overlap_new_usual_wiechen <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_wiechen == "1" & mi_test_rest_m1$guideline_vve_predw == "1")
table(overlap_new_usual_wiechen$taal_ont3y9m)
prop.table(table(overlap_new_usual_wiechen$taal_ont3y9m)) * 100
# kids indicated by 'new care' prediction model 
diff_new_wiechen <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_wiechen == "0" & mi_test_rest_m1$guideline_vve_predw == "1")
table(diff_new_wiechen$taal_ont3y9m)
prop.table(table(diff_new_wiechen$taal_ont3y9m)) * 100
# kids indicated by usual care VVE guidelines
diff_usual_wiechen <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_wiechen == "1" & mi_test_rest_m1$guideline_vve_predw == "0")
table(diff_usual_wiechen$taal_ont3y9m)
prop.table(table(diff_usual_wiechen$taal_ont3y9m)) * 100

### new care vs usual care 3 combi 
sum(mi_test_rest_m1$guideline_vve_combi == "1" & mi_test_rest_m1$guideline_vve_predc == "1")
sum(mi_test_rest_m1$guideline_vve_combi == "0" & mi_test_rest_m1$guideline_vve_predc == "1")
sum(mi_test_rest_m1$guideline_vve_combi == "1" & mi_test_rest_m1$guideline_vve_predc == "0")

overlap_new_usual_combi <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_combi == "1" & mi_test_rest_m1$guideline_vve_predc == "1")
table(overlap_new_usual_combi$taal_ont3y9m)
prop.table(table(overlap_new_usual_combi$taal_ont3y9m)) * 100

diff_new_combi <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_combi == "0" & mi_test_rest_m1$guideline_vve_predc == "1")
table(diff_new_combi$taal_ont3y9m)
prop.table(table(diff_new_combi$taal_ont3y9m)) * 100

diff_usual_combi <- mi_test_rest_m1 %>% filter(mi_test_rest_m1$guideline_vve_combi == "1" & mi_test_rest_m1$guideline_vve_predc == "0")
table(diff_usual_combi$taal_ont3y9m)
prop.table(table(diff_usual_combi$taal_ont3y9m)) * 100

