#### JGZ DATA ANALYSIS ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 06_jgz_prediction_overweight.R ####
# THE GOAL OF THIS SCRIPT IS TO PREDICT OVERWEIGHT

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
setwd("H:/Mirthe/")


#################################################################################
# 06_ANALYSIS: PREDICT OVERWEIGHT
#################################################################################
#### RESTRICTED MODEL ####
# this model includes a restricted set of variables; the restricted model includes only variables that are available 
# in a clinical, health care setting (EHR, and are currently registered by JGZ) or can reasonably be collected in this setting
# Additionally, the full model should indicate that these variables are the most relevant in predicting the outcome

#### 1. Look at the set of predictors (before multiple imputation) ####
predictors <- readRDS("case_jgz/data_outcome/overweight/overweight_jgz_dat_4m_comp_predictors_706_p.rds")

# look at the outcome variable: overweight
table(predictors$overweight)
prop.table(table(predictors$overweight)) * 100


#### 2. Dealing with missing data - multiple imputation with MICE ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# To deal with missing data, we used the MICE algorithm for multiple imputation: see 05_jgz_preprocess_MI

# read the multiple imputed train data for the case overweight created in 05_jgz_preprocess_MI
mi_complete <- readRDS("case_jgz/repo/overweight/data/overweight_MI_complete.rds")
# look at the data set
glimpse(mi_complete)

table(mi_complete$'1'$overweight)
prop.table(table(mi_complete$'1'$overweight)) * 100

mi_complete$'1' %>% 
  group_by(overweight, geslacht) %>% 
  summarise(n = n(),
            perc = n()/nrow(mi_complete$'1')*100)

# read the multiple imputed test data 
mi_test_comp <- readRDS("case_jgz/repo/overweight/data/overweight_MI_test.rds")

table(mi_test_comp$'1'$overweight)
prop.table(table(mi_test_comp$'1'$overweight)) * 100

mi_test_comp$'1' %>% 
  group_by(overweight, geslacht) %>% 
  summarise(n = n(),
            perc = n()/nrow(mi_test_comp$'1')*100)

# set seed for reproducibility
set.seed(8151)

#### 3. Create the restricted data set for analysis ####
# select the variables that you want to include in the restricted model 
# 3.1 For train data 
mi_rest <- mi_complete %>% 
  lapply(select, c(overweight,
                   # select the variables from the JGZ data
                   bmi_4wks_znl, bmi_8wks_znl, bmi_3mnd_znl, bmi_4mnd_znl,
                   geslacht, lft_concept_mo,
                   # select the education variables based on records from JGZ and CBS
                   educc_mo, educc_fa, 
                   
                   # add additional variables from CBS and perined
                   geboortegew, ED_woz, lft_concept_fa,
                   amww, par_cat, STED
                   ))

# 3.2 For test data 
mi_test_rest <- mi_test_comp %>% 
  lapply(select, c(overweight,
                   # select the variables from the JGZ data
                   bmi_4wks_znl, bmi_8wks_znl, bmi_3mnd_znl, bmi_4mnd_znl,
                   geslacht, lft_concept_mo,
                   # select the education variables based on records from JGZ and CBS
                   educc_mo, educc_fa, 
                   
                   # add additional variables from CBS and perined
                   geboortegew, ED_woz, lft_concept_fa,
                   amww, par_cat, STED
                   ))



#### 4. Random Forest model ####


#### 4.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 
# train random forest model on restricted data
rf_rest <- mi_rest %>% 
  lapply(ranger, formula = overweight ~ .,
         num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)

# you can save the ranger object but this is a very big object.
#saveRDS(rf_rest, "case_jgz/repo/overweight/output/rest/rf_rest.rds")

# look at the trained models. Remember you have m = 5 trained models
rf_rest$`1`
rf_rest$`3`
rf_rest$`5`
rf_rest$`1`$variable.importance
rf_rest$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the outcome overweight 
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
var_imp_norm1[1:14,] %>% 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF restricted model variable importance plot overweight")

# QUESTION TO POLINA; HOW TO SAVE THIS, MULTIPLE SHEETS IN EXCEL FILE? OR MULTIPLE CSV. FILES?
varimp_sheet_names <- list('var_imp_norm1' = var_imp_norm1, 'var_imp_norm2' = var_imp_norm2, 'var_imp_norm3' = var_imp_norm3, 
                           'var_imp_norm4' = var_imp_norm4, 'var_imp_norm5' = var_imp_norm5)
openxlsx::write.xlsx(varimp_sheet_names, file = "case_jgz/repo/overweight/output/rest/var_imp_norm_rest.xlsx")

# looking at predictions in the trained model 
summary(rf_rest$`1`$predictions[,2])
hist(rf_rest$`1`$predictions[,2], xlim = c(0, 1))

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of overweight), 
# otherwise negative =0 (low risk of overweight)
# try different thresholds; threshold at prevalence = 9.24%, threshold at 0.2, potential other thresholds
# you can do this for all m = 5 rf_rest models by changing rf_rest$`1` to rf_rest$`2` or rf_rest$`3` or rf_rest$`4` or rf_rest$`5`
rf_rest_prob <- ifelse(rf_rest$`1`$predictions[,2] > 0.2535, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_rest_cm <- confusionMatrix(rf_rest_prob, mi_rest$`1`$overweight, positive = "1")
rf_rest_cm
rf_rest_cmp <- round(prop.table(confusionMatrix(rf_rest_prob, mi_rest$`1`$overweight, positive = "1")$table),4)*100
rf_rest_cmp


#### 4.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# predict overweight in test data using probability estimation
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
write_excel_csv2(pred_rf_rest_pool, "case_jgz/repo/overweight/output/rest/pred_rf_rest_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of overweight for approx. 9.24% of children
pred_rf_rest_pool_class1 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.253, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm1 <- confusionMatrix(pred_rf_rest_pool_class1, mi_test_rest$`1`$overweight, positive = "1")
pred_rf_rest_pool_class_cm1
pred_rf_rest_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class1, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp1

# try different thresholds: > 0.2 approximately 15% highest risks
pred_rf_rest_pool_class2 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.2, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm2 <- confusionMatrix(pred_rf_rest_pool_class2, mi_test_rest$`1`$overweight, positive = "1")
pred_rf_rest_pool_class_cm2
pred_rf_rest_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class2, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp2

# try different thresholds: > 0.3 approximately 23% highest risks
pred_rf_rest_pool_class3 <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.15, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm3 <- confusionMatrix(pred_rf_rest_pool_class3, mi_test_rest$`1`$overweight, positive = "1")
pred_rf_rest_pool_class_cm3
pred_rf_rest_pool_class_cmp3 <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class3, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp3

# try different thresholds: > 0.424 at 1% highest risks
pred_rf_rest_pool_class1p <- ifelse(pred_rf_rest_pool$pred_outcome_prob > 0.424, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_rest_pool_class_cm1p <- confusionMatrix(pred_rf_rest_pool_class1p, mi_test_rest$`1`$overweight, positive = "1")
pred_rf_rest_pool_class_cm1p
pred_rf_rest_pool_class_cmp1p <- round(prop.table(confusionMatrix(pred_rf_rest_pool_class1p, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_rf_rest_pool_class_cmp1p

# look at the area under the ROC curve to assess overall model performance
rf_rest_roc <- roc(mi_test_rest$'1'$overweight, pred_rf_rest_pool$pred_outcome_prob)
auc(rf_rest_roc)
plot(rf_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_rest_roc, "case_jgz/repo/overweight/output/rest/rf_rest_roc.rds")


#### 4.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities
pred_rf_rest_pool$overweight <- mi_test_rest$'1'$overweight
table(pred_rf_rest_pool$overweight)
prop.table(table(pred_rf_rest_pool$overweight)) * 100

# look at predicted probabilities by PTB outcome
bmi_rf_rest_pool_cdf_1 <- pred_rf_rest_pool %>% filter(overweight == 1)
bmi_rf_rest_pool_cdf_0 <- pred_rf_rest_pool %>% filter(overweight == 0)

summary(bmi_rf_rest_pool_cdf_1$pred_outcome_prob)
summary(bmi_rf_rest_pool_cdf_0$pred_outcome_prob)

# plot the cdf
plot(ecdf(bmi_rf_rest_pool_cdf_1$pred_outcome_prob), xlab = "predicted probability", do.points = FALSE)+
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(bmi_rf_rest_pool_cdf_1$pred_prob), col = "red")+
  abline(v = median(bmi_rf_rest_pool_cdf_1$pred_prob), col = "orange")

plot(ecdf(bmi_rf_rest_pool_cdf_0$pred_outcome_prob), xlab = "predicted probability") +
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)+
  abline(v = mean(bmi_rf_rest_pool_cdf_0$pred_prob), col = "red")+
  abline(v = median(bmi_rf_rest_pool_cdf_0$pred_prob), col = "orange")

# arrange the predicted probabilities per PTB outcome and save as csv
bmi_rf_rest_pool_cdf_1_list <- bmi_rf_rest_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
bmi_rf_rest_pool_cdf_0_list <- bmi_rf_rest_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(bmi_rf_rest_pool_cdf_1_list, "case_jgz/repo/overweight/output/rest/bmi_rf_rest_pool_cdf_1_list.csv")
write_excel_csv2(bmi_rf_rest_pool_cdf_0_list, "case_jgz/repo/overweight/output/rest/bmi_rf_rest_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability



# Look at specificity and sensitivity of the prediction model for different thresholds of predicted probability 
# create 100 bins, take the mean of predicted probability and look at the percentage of children with overweight for each bin
predprob_bins <- data.frame(predprob = rf_rest_roc$predictor, overweight = rf_rest_roc$response) 
predprob_bins$bin100 <- ntile(rf_rest_roc$predictor, n = 100)

predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          outcome_1 = sum(overweight == 1),
          outcome_0 = sum(overweight == 0),
          outcome1_perc = sum(overweight == 1) / (sum(overweight == 1)+sum(overweight == 0))*100) %>% 
  mutate(outcome1_perc = ifelse((outcome1_perc/100)*n < 10, NA, outcome1_perc))

view(predprob_bins100)
# look at the distribution of predicted probabilities 
plot(x = predprob_bins100$bin100, y = predprob_bins100$predprob_mean)
# look at the percentage of children with overweight per bin (most are NA because N<10)
plot(x = predprob_bins100$bin100, y = predprob_bins100$outcome1_perc)

# For the 100 different predicted probability thresholds, calculate specificity and sensitivity
predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in predprob_bins100$predprob_mean){
  
  #print(predprob_i)
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
write_excel_csv2(predprob_bins100_df, "output/rest/overweight_rf_rest_predprob_bins100_df.csv")
# look at the table
predprob_bins100_df

# Plot the predicted probability for the 100 bins on x-axis and sensitivity or specificity on y-axis
# save the plot as png
png(file = "output/rest/overweight_rf_rest_predprob_bins100_sens_spec.png",
    width= 600, height = 450)
ggplot(predprob_bins100_df, show.legend = TRUE)+
  geom_point(aes(x = predprob_mean, y = specificity, colour = 'specificity'))+
  geom_point(aes(x = predprob_mean, y = sensitivity, colour = 'sensitivity'))+
  ggtitle("overweight_rf_rest_predprob_bins100_ysens_spec")+
  labs(y = 'specificity and sensitivity')
dev.off()




#### 5. Logistic regression model ####
# we also train and test a logistic regression model and consider its predictive performance in the case of overweight
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

#### 5.1 Train the Logistic regression model using glm ####
glm_rest <- mi_rest %>% 
  lapply(glm, formula = overweight ~ ., family = binomial)
# look at the summary of the trained model by pooling the m =5 logistic regression models
glm_rest_pool <- summary(pool(glm_rest))
glm_rest_pool

# you can save the trained model but this is a very big object
#saveRDS(glm_rest, "case_jgz/repo/overweight/output/rest/glm_rest.rds")
# maybe save the pooled summary instead
write_excel_csv2(glm_rest_pool, "case_jgz/repo/overweight/output/rest/glm_rest_pool.csv")


#### 5.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# predict on test data
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
write_excel_csv2(pred_glm_rest_pool, "case_jgz/repo/overweight/output/rest/pred_glm_rest_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of overweight for approx. 9.24% of children
pred_glm_rest_pool_class1 <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.2167, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm1 <- confusionMatrix(pred_glm_rest_pool_class1, mi_test_rest$`1`$overweight, positive = "1")
pred_glm_rest_pool_class_cm1
pred_glm_rest_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class1, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp1

# try different thresholds: > 0.15 approx. 17% highest risks 
pred_glm_rest_pool_class2 <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.15, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm2 <- confusionMatrix(pred_glm_rest_pool_class2, mi_test_rest$`1`$overweight, positive = "1")
pred_glm_rest_pool_class_cm2
pred_glm_rest_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class2, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp2

# try different thresholds: > 0.449 approx. 1% highest risks 
pred_glm_rest_pool_class3 <- ifelse(pred_glm_rest_pool$pred_outcome_prob_exp > 0.449, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_rest_pool_class_cm3 <- confusionMatrix(pred_glm_rest_pool_class3, mi_test_rest$`1`$overweight, positive = "1")
pred_glm_rest_pool_class_cm3
pred_glm_rest_pool_class_cmp3 <- round(prop.table(confusionMatrix(pred_glm_rest_pool_class3, mi_test_rest$`1`$overweight, positive = "1")$table),4)*100
pred_glm_rest_pool_class_cmp3

# look at the area under the ROC curve to assess overall model performance
glm_rest_roc <- roc(mi_test_rest$`1`$overweight, pred_glm_rest_pool$pred_outcome_prob_exp)
auc(glm_rest_roc)
plot(glm_rest_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(glm_rest_roc, "case_jgz/repo/overweight/output/rest/glm_rest_roc.rds")


#### 5.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities 
pred_glm_rest_pool$overweight <- mi_test_rest$`1`$overweight
table(pred_glm_rest_pool$overweight)
prop.table(table(pred_glm_rest_pool$overweight)) * 100

bmi_glm_rest_pool_cdf_1 <- pred_glm_rest_pool %>% filter(overweight == 1)
bmi_glm_rest_pool_cdf_0 <- pred_glm_rest_pool %>% filter(overweight == 0)

bmi_glm_rest_pool_cdf_1_list <- bmi_glm_rest_pool_cdf_1 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)
bmi_glm_rest_pool_cdf_0_list <- bmi_glm_rest_pool_cdf_0 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)

write_excel_csv2(bmi_glm_rest_pool_cdf_1_list, "case_jgz/repo/overweight/output/rest/bmi_glm_rest_pool_cdf_1_list.csv")
write_excel_csv2(bmi_glm_rest_pool_cdf_0_list, "case_jgz/repo/overweight/output/rest/bmi_glm_rest_pool_cdf_0_list.csv")


#Look at specificity and sensitivity of the prediction model for different thresholds of predicted probability. 
# create 100 bins, take the mean of predicted probability and look at the percentage of children with overweight for each bin
predprob_bins_glm <- data.frame(predprob = glm_rest_roc$predictor, overweight = glm_rest_roc$response) 
predprob_bins_glm$bin100 <- ntile(glm_rest_roc$predictor, n = 100)

predprob_bins_glm100 <- predprob_bins_glm %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          outcome_1 = sum(overweight == 1),
          outcome_0 = sum(overweight == 0),
          outcome1_perc = sum(overweight == 1) / (sum(overweight == 1)+sum(overweight == 0))*100) %>% 
  mutate(outcome1_perc = ifelse((outcome1_perc/100)*n < 10, NA, outcome1_perc))

view(predprob_bins_glm100)
# look at the distribution of predicted probabilities 
plot(x = predprob_bins_glm100$bin100, y = predprob_bins_glm100$predprob_mean)
# look at the percentage of children with overweight per bin (most are NA because N<10)
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
write_excel_csv2(predprob_bins_glm100_df, "output/rest/overweight_glm_rest_predprob_bins100_df.csv")
# look at the table
predprob_bins_glm100_df


# Plot the predicted probability for the 100 bins on x-axis and sensitivity or specificity on y-axis
# save the plot as png
png(file = "output/rest/overweight_glm_rest_predprob_bins100_sens_spec.png",
    width= 600, height = 450)
ggplot(predprob_bins_glm100_df, show.legend = TRUE)+
  geom_point(aes(x = predprob_mean, y = specificity, colour = 'specificity'))+
  geom_point(aes(x = predprob_mean, y = sensitivity, colour = 'sensitivity'))+
  ggtitle("overweight_glm_rest_predprob_bins100_ysens_spec")+
  labs(y = 'specificity and sensitivity')
dev.off()

