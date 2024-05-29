#### JGZ DATA ANALYSIS ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 06_jgz_prediction_sdq_risk.R ####
# THE GOAL OF THIS SCRIPT IS TO PREDICT SOCIAL-EMOTIONAL DEVELOPMENT

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
# 06_ANALYSIS: PREDICT SOCIAL-EMOTIONAL DEVELOPMENT with SDQ
#################################################################################
# 'doe-moment': consultation moment during which the child is approx. 16 weeks of age
# 'outcome-age': consultation moment during which the child is approx. 5 years of age

#### FULL MODEL ####
# this model includes all the available data from JGZ, CBS and Perined

#### 1. Look at the set of predictors (before multiple imputation) ####
predictors <- readRDS("case_jgz/data_outcome/sdq_16w/sdq_predictors_p.rds")

# look at the outcome variable: sdq_risk, SDQ total score of >= 11
table(predictors$sdq_risk)
prop.table(table(predictors$sdq_risk)) * 100

predictors %>% 
  group_by(sdq_risk, geslacht) %>% 
  summarise(n = n())

#### 2. Dealing with missing data - multiple imputation with MICE ####
# There are missing values in the data set, we cannot train a model when there are missing values
# we don't want to throw away all records with missing values, this might bias the results and it is a waste 
# To deal with missing data, we used the MICE algorithm for multiple imputation: see 05_jgz_preprocess_MI

# read the multiple imputed train data for the case sdq_risk created in 05_jgz_preprocess_MI
mi_complete <- readRDS("case_jgz/repo/socialemotional_development/16w/data/socialemotional_16w_MI_complete.rda")
# look at the data set
glimpse(mi_complete)

table(mi_complete$'1'$sdq_risk)
prop.table(table(mi_complete$'1'$sdq_risk)) * 100

# read the multiple imputed test data 
mi_test_comp <- readRDS("case_jgz/repo/socialemotional_development/16w/data/socialemotional_16w_MI_test_comp.rda")

table(mi_test_comp$'1'$sdq_risk)
prop.table(table(mi_test_comp$'1'$sdq_risk)) * 100

# set seed for reproducibility
set.seed(8151)


#### 3. Random Forest model ####

#### 3.1 Train the random forest model using ranger ####
# Ranger is a fast implementation of random forest (Breiman 2001) or recursive partitioning, particularly suited for high dimensional data. 

# Train the random forest model; remember we have m = 5 multiple imputed data sets, we train 5 RF models, for each of the imputed data sets 
rf_full <- mi_complete %>% 
  # use lapply to apply the function to the list of m = 5
  lapply(ranger, formula = sdq_risk ~ ., num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)
# specify probability is TRUE to obtain predicted probability of outcome instead of binary prediction
# ! try different hyperparameter specifications for mtry and min.node.size

# you can save the ranger object but this is a very big object.
#saveRDS(rf_full, "case_jgz/repo/socialemotional_development/16w/output/rf_full.rds")

# look at the trained models. Remember you have m = 5 trained models
rf_full$`1`
rf_full$`3`
rf_full$`5`
rf_full$`1`$variable.importance
rf_full$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the outcome sdq_risk
# look at variable importance score, ordered and standardized (% out of 100%) 
var_imp_norm1 <- data.frame(var = names(rf_full$`1`$variable.importance),
                            imp = rf_full$`1`$variable.importance/sum(rf_full$`1`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm2 <- data.frame(var = names(rf_full$`2`$variable.importance),
                            imp = rf_full$`2`$variable.importance/sum(rf_full$`2`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm3 <- data.frame(var = names(rf_full$`3`$variable.importance),
                            imp = rf_full$`3`$variable.importance/sum(rf_full$`3`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm4 <- data.frame(var = names(rf_full$`4`$variable.importance),
                            imp = rf_full$`4`$variable.importance/sum(rf_full$`4`$variable.importance)*100) %>% 
  arrange(desc(imp))
var_imp_norm5 <- data.frame(var = names(rf_full$`5`$variable.importance),
                            imp = rf_full$`5`$variable.importance/sum(rf_full$`5`$variable.importance)*100) %>% 
  arrange(desc(imp))

# create a plot of the Nth most important variables
# change var_imp_norm1 to 2 or 3 if you want to look at the plots for the other m's
var_imp_norm1[1:30,] %>% # change this number in case you want to look at more independent variables in the model
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF full model variable importance plot sdq risk")

varimp_sheet_names <- list('var_imp_norm1' = var_imp_norm1, 'var_imp_norm2' = var_imp_norm2, 'var_imp_norm3' = var_imp_norm3, 
                           'var_imp_norm4' = var_imp_norm4, 'var_imp_norm5' = var_imp_norm5)
openxlsx::write.xlsx(varimp_sheet_names, file = 'case_jgz/repo/socialemotional_development/16w/output/var_imp_norm.xlsx')

# looking at predictions in the trained model 
summary(rf_full$`1`$predictions[,2])
hist(rf_full$`1`$predictions[,2], xlim = c(0, 1))
prop.table(table(mi_complete$'1'$sdq_risk))*100

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
# if the predictive probability is higher than the threshold we predict a positive outcome =1 (high risk of sdq >= 11), 
# otherwise negative =0 (low risk of sdq >= 11)
# try different thresholds; threshold at prevalence = 13.8%, threshold at 0.248, potential other thresholds
# you can do this for all m = 5 rf_full models by changing rf_full$`1` to rf_full$`2` or rf_full$`3` or rf_full$`4` or rf_full$`5`
rf_full_prob <- ifelse(rf_full$`1`$predictions[,2] > 0.248, 1, 0) %>% as.factor()  
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
rf_full_cm <- confusionMatrix(rf_full_prob, mi_complete$`1`$sdq_risk, positive = "1")
rf_full_cm
rf_full_cmp <- round(prop.table(confusionMatrix(rf_full_prob, mi_complete$`1`$sdq_risk, positive = "1")$table),4)*100
rf_full_cmp


#### 3.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# predict sdq_risk in test data using probability estimation
pred_rf_full <- lapply(rf_full, function(model){
  predict(model, data = mi_test_comp$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use any of the other m's, it should not make much of a difference

# for each individual in the test set you obtain m = 5 predicted probabilities because you have trained 5 RF models on 5 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_full_pool <- data.frame(
  pred_rf_full1 = pred_rf_full[[1]],
  pred_rf_full2 = pred_rf_full[[2]],
  pred_rf_full3 = pred_rf_full[[3]],
  pred_rf_full4 = pred_rf_full[[4]],
  pred_rf_full5 = pred_rf_full[[5]]
)

# pool the m = 5 predicted probabilities per individual by taking the mean
pred_rf_full_pool <- pred_rf_full_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_full1, pred_rf_full2, pred_rf_full3, pred_rf_full4, pred_rf_full5), na.rm = T))

# save the predicted probabilities for future use
write_excel_csv2(pred_rf_full_pool, "case_jgz/repo/socialemotional_development/16w/output/pred_rf_full_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of sdq >= 11 for approx. 13.8% of children 
pred_rf_full_pool_class1 <- ifelse(pred_rf_full_pool$pred_outcome_prob > 0.2535, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_full_pool_class_cm1 <- confusionMatrix(pred_rf_full_pool_class1, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_rf_full_pool_class_cm1
pred_rf_full_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_rf_full_pool_class1, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_rf_full_pool_class_cmp1

# try different thresholds: > 0.23 approx. 20% highest risks
pred_rf_full_pool_class2 <- ifelse(pred_rf_full_pool$pred_outcome_prob > 0.23, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_full_pool_class_cm2 <- confusionMatrix(pred_rf_full_pool_class2, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_rf_full_pool_class_cm2
pred_rf_full_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_rf_full_pool_class2, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_rf_full_pool_class_cmp2

# try different thresholds: > 0.248 approx. 15% highest risks
pred_rf_full_pool_class15 <- ifelse(pred_rf_full_pool$pred_outcome_prob > 0.248, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_full_pool_class_cm15 <- confusionMatrix(pred_rf_full_pool_class15, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_rf_full_pool_class_cm15
pred_rf_full_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_rf_full_pool_class15, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_rf_full_pool_class_cmp15

# look at the area under the ROC curve to assess overall model performance
rf_full_roc <- roc(mi_test_comp$'1'$sdq_risk, pred_rf_full_pool$pred_outcome_prob)
auc(rf_full_roc)
plot(rf_full_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_full_roc, "case_jgz/repo/socialemotional_development/16w/output/pred_rf_full_pool.csv")


#### 3.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities
pred_rf_full_pool$sdq_risk <- mi_test_comp$'1'$sdq_risk
table(pred_rf_full_pool$sdq_risk)
prop.table(table(pred_rf_full_pool$sdq_risk)) * 100

# look at predicted probabilities by sdq_risk outcome
sdq_rf_full_pool_cdf_1 <- pred_rf_full_pool %>% filter(sdq_risk == 1)
sdq_rf_full_pool_cdf_0 <- pred_rf_full_pool %>% filter(sdq_risk == 0)

summary(sdq_rf_full_pool_cdf_1$pred_outcome_prob)
summary(sdq_rf_full_pool_cdf_0$pred_outcome_prob)

# plot the cdf
plot(ecdf(sdq_rf_full_pool_cdf_1$pred_outcome_prob), xlab = "predicted probability")+
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)

plot(ecdf(sdq_rf_full_pool_cdf_0$pred_outcome_prob), xlab = "predicted probability") +
  abline(h = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2) +
  abline(v = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), col = "grey", lty = 2)

# arrange the predicted probabilities per sdq_risk outcome and save as csv
sdq_rf_full_pool_cdf_1_list <- sdq_rf_full_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
sdq_rf_full_pool_cdf_0_list <- sdq_rf_full_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(sdq_rf_full_pool_cdf_1_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_rf_full_pool_cdf_1_list.csv")
write_excel_csv2(sdq_rf_full_pool_cdf_0_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_rf_full_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


# Look at specificity and sensitivity of the prediction model for different thresholds of predicted probability.
# create 100 bins, take the mean of predicted probability and look at the percentage of children with delayed social emotional development for each bin
predprob_bins <- data.frame(predprob = rf_full_roc$predictor, sdq_risk = rf_full_roc$response) 
predprob_bins$bin100 <- ntile(rf_full_roc$predictor, n = 100)

predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          outcome_1 = sum(sdq_risk == 1),
          outcome_0 = sum(sdq_risk == 0),
          outcome1_perc = sum(sdq_risk == 1) / (sum(sdq_risk == 1)+sum(sdq_risk == 0))*100) %>% 
  mutate(outcome1_perc = ifelse((outcome1_perc/100)*n < 10, NA, outcome1_perc))

view(predprob_bins100)
# look at the distribution of predicted probabilities 
plot(x = predprob_bins100$bin100, y = predprob_bins100$predprob_mean)
# look at the percentage of children with delayed social-emotional development per bin (most are NA because N<10)
plot(x = predprob_bins100$bin100, y = predprob_bins100$outcome1_perc)

# For the 100 different predicted probability thresholds, calculate specificity and sensitivity
predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in predprob_bins100$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_full_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full_roc$response, positive = "1")
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
write_excel_csv2(predprob_bins100_df, "case_jgz/repo/socialemotional_development/16w/output/sdq_risk_rf_full_predprob_bins100_df.csv")
# look at the table
predprob_bins100_df


# Plot the predicted probability for the 100 bins on x-axis and sensitivity or specificity on y-axis
# save the plot as png
png(file = "case_jgz/repo/socialemotional_development/16w/output/sdq_risk_rf_full_predprob_bins100_sens_spec.png",
    width= 600, height = 450)
ggplot(predprob_bins100_df, show.legend = TRUE)+
  geom_point(aes(x = predprob_mean, y = specificity, colour = 'specificity'))+
  geom_point(aes(x = predprob_mean, y = sensitivity, colour = 'sensitivity'))+
  ggtitle("sdq_risk_rf_full_predprob_bins100_ysens_spec")+
  labs(y = 'specificity and sensitivity')
dev.off()



#### 4. Logistic regression model ####
# we also train and test a logistic regression model and consider its predictive performance in the case of sdq_risk 
# Compared to the RF model, the logistic regression model is easier to interpret, but it is also less flexible.
# The logistic regression model assumes linearity. Be aware that its assumptions might be violated. 

#### 4.1 Train the Logistic regression model using glm ####
glm_full <- mi_complete %>% 
  lapply(glm, formula = sdq_risk ~ ., family = binomial)
# look at the summary of the trained model by pooling the m =5 logistic regression models
glm_full_pool <- summary(pool(glm_full))
glm_full_pool

# you can save the trained model but this is a very big object
#saveRDS(glm_full, "case_jgz/repo/socialemotional_development/16w/output/glm_full.rds")
# maybe save the pooled summary instead
write_excel_csv2(glm_full_pool, "case_jgz/repo/socialemotional_development/16w/output/glm_full_pool.csv")


#### 4.2 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# read the multiple imputed test data
pred_glm_full <- predict(glm_full, mi_test_comp$`1`)
# we use test set m = 1, you van also use m = 2 3 4 OR 5, it should not make much of a difference

# for each individual in the test set you obtain m = 5 predicted probabilities because you have trained 5 models on 5 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_glm_full_pool <- data.frame(
  pred_glm_full1 = pred_glm_full[[1]],
  pred_glm_full2 = pred_glm_full[[2]],
  pred_glm_full3 = pred_glm_full[[3]],
  pred_glm_full4 = pred_glm_full[[4]],
  pred_glm_full5 = pred_glm_full[[5]]
)

# pool the m = 5 predicted probabilities per individual by taking the mean
pred_glm_full_pool <- pred_glm_full_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_glm_full1, pred_glm_full2, pred_glm_full3, pred_glm_full4, pred_glm_full5), na.rm = T))

# for the logistic regression, compute the exponential function of the predicted probabilities to get a scale of 0-1
pred_glm_full_pool <- pred_glm_full_pool %>%
  mutate(pred_outcome_prob_exp = exp(pred_outcome_prob) / (1+exp(pred_outcome_prob)))

# save the predicted probabilities for future use
write_excel_csv2(pred_glm_full_pool, "case_jgz/repo/socialemotional_development/16w/output/pred_glm_full_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
# this threshold is set a prevalence, we predict a high risk of sdq >= 11 for approx. 13.8% of children
pred_glm_full_pool_class1 <- ifelse(pred_glm_full_pool$pred_outcome_prob_exp > 0.227, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_full_pool_class_cm1 <- confusionMatrix(pred_glm_full_pool_class1, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_glm_full_pool_class_cm1
pred_glm_full_pool_class_cmp1 <- round(prop.table(confusionMatrix(pred_glm_full_pool_class1, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_glm_full_pool_class_cmp1

# try different thresholds: > 0.2 approx. 20% highest risks 
pred_glm_full_pool_class2 <- ifelse(pred_glm_full_pool$pred_outcome_prob_exp > 0.2, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_full_pool_class_cm2 <- confusionMatrix(pred_glm_full_pool_class2, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_glm_full_pool_class_cm2
pred_glm_full_pool_class_cmp2 <- round(prop.table(confusionMatrix(pred_glm_full_pool_class2, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_glm_full_pool_class_cmp2

# try different thresholds: > 0.222 approx. 15% highest risks 
pred_glm_full_pool_class15 <- ifelse(pred_glm_full_pool$pred_outcome_prob_exp > 0.222, 1, 0) %>% as.factor() 
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_glm_full_pool_class_cm15 <- confusionMatrix(pred_glm_full_pool_class15, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_glm_full_pool_class_cm15
pred_glm_full_pool_class_cmp15 <- round(prop.table(confusionMatrix(pred_glm_full_pool_class15, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_glm_full_pool_class_cmp15

# look at the area under the ROC curve to assess overall model performance
glm_full_roc <- roc(mi_test_comp$`1`$sdq_risk, pred_glm_full_pool$pred_outcome_prob_exp)
auc(glm_full_roc)
plot(glm_full_roc, print.auc = TRUE, col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(glm_full_roc, "case_jgz/repo/socialemotional_development/16w/output/pred_glm_full_pool.csv")


#### 4.3 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities 
pred_glm_full_pool$sdq_risk <- mi_test_comp$`1`$sdq_risk
table(pred_glm_full_pool$sdq_risk)
prop.table(table(pred_glm_full_pool$sdq_risk)) * 100

sdq_glm_full_pool_cdf_1 <- pred_glm_full_pool %>% filter(sdq_risk == 1)
sdq_glm_full_pool_cdf_0 <- pred_glm_full_pool %>% filter(sdq_risk == 0)

sdq_glm_full_pool_cdf_1_list <- sdq_glm_full_pool_cdf_1 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)
sdq_glm_full_pool_cdf_0_list <- sdq_glm_full_pool_cdf_0 %>% select(c(pred_outcome_prob_exp)) %>% arrange(pred_outcome_prob_exp)

write_excel_csv2(sdq_glm_full_pool_cdf_1_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_glm_full_pool_cdf_1_list.csv")
write_excel_csv2(sdq_glm_full_pool_cdf_0_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_glm_full_pool_cdf_0_list.csv")



######################################################################################
#### Additional analysis ####


#### 5 Balanced models ####
# A challenge working with this data set and with the outcome sdq_risk is class imbalance
# Class imbalance is a common issue in classification tasks where the number of instances in each class of the outcome is not equal.
# In this data set, a significant proportion of the observed instances of sdq_risk belong to one class: no sdq_risk (the majority class).
# This biases the model towards the majority class at the expense of the minority class; sdq_risk, our class of interest.
# The class imbalance problem is usually addressed by oversampling and under-sampling approaches.
# We used WERCS to balance the data set. Subsequently, we trained the RF model on the balanced data set. 
# ! Warning: This resulted in a trained model that was overfit; predictive performance of the trained balanced model was better compared to unbalanced, 
# but performance in test data did not improve.
library(UBL)

#### 5.1 Balance the data set ####
# use WEighted Relevance-based Combination Strategy (WERCS) algorithm to balance the data set 
mi_complete_balanced <- lapply(mi_complete, function(mi_set){
  # C.perc is set to over-sample the minority class by *3, the minority class remains unchanged
  WERCSClassif(sdq_risk ~ ., data.frame(mi_set), C.perc = list("0" = 1, "1" = 3)) # other option: C.perc = "balance")
})

# look at the new balanced outcome variable
table(mi_complete_balanced$`1`$sdq_risk)
prop.table(table(mi_complete_balanced$`1`$sdq_risk)) * 100

#### 5.2 Train the random forest model with ranger using the balanced data set ####
rf_full_balanced <- mi_complete_balanced %>% 
  lapply(ranger, formula = sdq_risk ~ ., num.trees = 1000, importance = 'impurity', probability = TRUE, seed = 8151)

# look at the trained models. Remember you have m = 5 trained models
rf_full_balanced$`1`
rf_full_balanced$`1`$variable.importance
rf_full_balanced$`1`$prediction.error

# variable importance scores show how 'important' a variable is in predicting the outcome sdq_risk
# look at variable importance score, ordered and standardized (% out of 100%) 
# change rf_full_balanced$`1` to 2, 3, 4 or 5 if you want to look at the plots for the other m's
var_imp_norm_balanced1 <- data.frame(var = names(rf_full_balanced$`1`$variable.importance),
                                     imp = rf_full_balanced$`1`$variable.importance/sum(rf_full_balanced$`1`$variable.importance)*100) %>% 
  arrange(desc(imp))

# create a plot of the Nth most important variables
# change var_imp_norm_balanced1 to 2, 3, 4 or 5 if you want to look at the plots for the other m's
var_imp_norm_balanced1[1:30,] %>% 
  ggplot(aes(imp, x = reorder(var, imp))) +
  geom_point(size = 3, colour = "black") +
  coord_flip() +
  labs(x = "Predictors", y = "Importance scores")+
  ggtitle("RF full model variable importance plot sdq_risk balanced")

varimp_sheet_names_balanced <- list('var_imp_norm_balanced1' = var_imp_norm_balanced1)
openxlsx::write.xlsx(varimp_sheet_names_balanced, file = 'case_jgz/repo/socialemotional_development/16w/output/var_imp_norm_balanced.xlsx')

# looking at predictions in the trained model
summary(rf_full_balanced$`1`$predictions[,2])
hist(rf_full_balanced$`1`$predictions[,2], xlim = c(0, 1))

# Classify the predictions of training data; predict binary outcome using different predicted probability thresholds
rf_full_prob_balanced <- ifelse(rf_full_balanced$`1`$predictions[,2] > 0.45, 1, 0) %>% as.factor()  
rf_full_prob_balanced_cm <- confusionMatrix(rf_full_prob_balanced, mi_complete_balanced$`1`$sdq_risk, positive = "1")
rf_full_prob_balanced_cm
rf_full_prob_balanced_cmp <- round(prop.table(confusionMatrix(rf_full_prob_balanced, mi_complete_balanced$`1`$sdq_risk, positive = "1")$table),4)*100
rf_full_prob_balanced_cmp

#### 5.3 Use the trained model to predict individual outcomes in the test data & evaluate predictive performance ####
# the test data set is not balanced; only the train data is balanced to see if it will improve prediction
# predict sdq_risk in test data using probability estimation
pred_rf_full_balanced <- lapply(rf_full_balanced, function(model){
  predict(model, data = mi_test_comp$`1`)$predictions[,2]
})
# we use test set m = 1, you van also use any of the other m's, it should not make much of a difference

# for each individual in the test set you obtain m = 5 predicted probabilities because you have trained 5 RF models on 5 imputed train data sets
# make a data from with a column for each predicted probability per individual
pred_rf_full_balanced_pool <- data.frame(
  pred_rf_full_balanced1 = pred_rf_full_balanced[[1]],
  pred_rf_full_balanced2 = pred_rf_full_balanced[[2]],
  pred_rf_full_balanced3 = pred_rf_full_balanced[[3]],
  pred_rf_full_balanced4 = pred_rf_full_balanced[[4]],
  pred_rf_full_balanced5 = pred_rf_full_balanced[[5]]
)

# pool the m = 5 predicted probabilities per individual by taking the mean
pred_rf_full_balanced_pool <- pred_rf_full_balanced_pool %>% 
  mutate(pred_outcome_prob = rowMeans(select(., pred_rf_full_balanced1, pred_rf_full_balanced2, pred_rf_full_balanced3, pred_rf_full_balanced4, pred_rf_full_balanced5), na.rm = T))
# save the predicted probabilities for future use
write_excel_csv2(pred_rf_full_balanced_pool, "case_jgz/repo/socialemotional_development/16w/output/pred_rf_full_balanced_pool.csv")

# classify predictions of test data; predict binary outcome using a predicted probability threshold
pred_rf_full_balanced_pool_classb <- ifelse(pred_rf_full_balanced_pool$pred_outcome_prob > 0.3, 1, 0) %>% as.factor()
# look at the confusionMatrix: accuracy, sensitivity, specificity and the Cohen's Kappa coefficient (Kappa accounts for the possibility of a correct prediction by chance alone (in contrast to Accuracy))
pred_rf_full_balanced_pool_classb_cm <- confusionMatrix(pred_rf_full_balanced_pool_classb, mi_test_comp$`1`$sdq_risk, positive = "1")
pred_rf_full_balanced_pool_classb_cm
pred_rf_full_balanced_pool_classb_cmp <- round(prop.table(confusionMatrix(pred_rf_full_balanced_pool_classb, mi_test_comp$`1`$sdq_risk, positive = "1")$table),4)*100
pred_rf_full_balanced_pool_classb_cmp
# ! warning the model is overfit

# look at the area under the ROC curve to assess overall model performance
rf_full_roc_balanced <- roc(mi_test_comp$`1`$sdq_risk, pred_rf_full_balanced_pool$pred_outcome_prob)
auc(rf_full_roc_balanced)
plot(rf_full_roc_balanced, print.auc = TRUE,  col = "black", xlim = c(1, 0), ylim=c(0, 1))
saveRDS(rf_full_roc_balanced, "case_jgz/repo/socialemotional_development/16w/output/rf_full_roc_balanced.csv")

#### 5.4 save the predicted probabilities by outcome ####
# add the observed outcome to the data frame with predicted probabilities
pred_rf_full_balanced_pool$sdq_risk <- mi_test_comp$`1`$sdq_risk
table(pred_rf_full_balanced_pool$sdq_risk)
prop.table(table(pred_rf_full_balanced_pool$sdq_risk)) * 100

# look at predicted probabilities by PTB outcome
sdq_rf_full_balanced_pool_cdf_1 <- pred_rf_full_balanced_pool %>% filter(sdq_risk == 1)
sdq_rf_full_balanced_pool_cdf_0 <- pred_rf_full_balanced_pool %>% filter(sdq_risk == 0)

summary(sdq_rf_full_balanced_pool_cdf_1$pred_outcome_prob)
summary(sdq_rf_full_balanced_pool_cdf_0$pred_outcome_prob)

# arrange the predicted probabilities per PTB outcome and save as csv
sdq_rf_full_balanced_pool_cdf_1_list <- sdq_rf_full_balanced_pool_cdf_1 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)
sdq_rf_full_balanced_pool_cdf_0_list <- sdq_rf_full_balanced_pool_cdf_0 %>% select(c(pred_outcome_prob)) %>% arrange(pred_outcome_prob)

write_excel_csv2(sdq_rf_full_balanced_pool_cdf_1_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_rf_full_balanced_pool_cdf_1_list.csv")
write_excel_csv2(sdq_rf_full_balanced_pool_cdf_0_list, "case_jgz/repo/socialemotional_development/16w/output/sdq_rf_full_balanced_pool_cdf_0_list.csv")
# with these saved, you can later assess classification performance at various thresholds of predicted probability


