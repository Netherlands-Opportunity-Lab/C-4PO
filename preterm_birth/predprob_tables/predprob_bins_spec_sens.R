#### BINS BINS BINS ####
library(tidyverse)
library(pROC)
library(caret)
setwd("H:/Mirthe/")

rf_full32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_full32_roc.rds")
rf_full32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_full32_multi_roc.rds")
rf_full32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_full32_primi_roc.rds")

rf_rest32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_rest32_roc.rds")
rf_rest32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_rest32_multi_roc.rds")
rf_rest32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_rest32_primi_roc.rds")

rf_medical32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_medical32_roc.rds")
rf_medical32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_medical32_multi_roc.rds")
rf_medical32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_medical32_primi_roc.rds")

rf_ptbhist32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_ptbhist32_roc.rds")
rf_ptbhist32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/repo/PTB_output/202403_v1/rf_ptbhist32_multi_roc.rds")


#### rf_full32_roc ####

predprob_bins <- data.frame(predprob = rf_full32_roc$predictor, outcome_ptb = rf_full32_roc$response) 

predprob_bins$bin100 <- ntile(rf_full32_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_full32_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins1000_df.csv")



#### rf_full32_multi_roc ####

predprob_bins <- data.frame(predprob = rf_full32_multi_roc$predictor, outcome_ptb = rf_full32_multi_roc$response) 

predprob_bins$bin100 <- ntile(rf_full32_multi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_full32_multi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins1000_df.csv")





#### rf_full32_primi_roc ####

predprob_bins <- data.frame(predprob = rf_full32_primi_roc$predictor, outcome_ptb = rf_full32_primi_roc$response) 

predprob_bins$bin100 <- ntile(rf_full32_primi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_full32_primi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_full32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_full32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins1000_df.csv")





#### rf_rest32_roc ####

predprob_bins <- data.frame(predprob = rf_rest32_roc$predictor, outcome_ptb = rf_rest32_roc$response) 

predprob_bins$bin100 <- ntile(rf_rest32_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_rest32_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins1000_df.csv")



#### rf_rest32_multi_roc ####

predprob_bins <- data.frame(predprob = rf_rest32_multi_roc$predictor, outcome_ptb = rf_rest32_multi_roc$response) 

predprob_bins$bin100 <- ntile(rf_rest32_multi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_rest32_multi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
test_ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins1000_df.csv")





#### rf_rest32_primi_roc ####

predprob_bins <- data.frame(predprob = rf_rest32_primi_roc$predictor, outcome_ptb = rf_rest32_primi_roc$response) 

predprob_bins$bin100 <- ntile(rf_rest32_primi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_rest32_primi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_rest32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_rest32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins1000_df.csv")




#### rf_medical32_roc ####
predprob_bins <- data.frame(predprob = rf_medical32_roc$predictor, outcome_ptb = rf_medical32_roc$response) 

predprob_bins$bin100 <- ntile(rf_medical32_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_medical32_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins1000_df.csv")


#### rf_medical32_multi_roc ####
predprob_bins <- data.frame(predprob = rf_medical32_multi_roc$predictor, outcome_ptb = rf_medical32_multi_roc$response) 

predprob_bins$bin100 <- ntile(rf_medical32_multi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_medical32_multi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins1000_df.csv")


#### rf_medical32_primi_roc ####

predprob_bins <- data.frame(predprob = rf_medical32_primi_roc$predictor, outcome_ptb = rf_medical32_primi_roc$response) 

predprob_bins$bin100 <- ntile(rf_medical32_primi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_medical32_primi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_medical32_primi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_medical32_primi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins1000_df.csv")





#### rf_ptbhist32_roc ####

predprob_bins <- data.frame(predprob = rf_ptbhist32_roc$predictor, outcome_ptb = rf_ptbhist32_roc$response) 

predprob_bins$bin100 <- ntile(rf_ptbhist32_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_ptbhist32_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_ptbhist32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_ptbhist32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_ptbhist32_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_ptbhist32_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins1000_df.csv")



#### rf_ptbhist32_multi_roc ####

predprob_bins <- data.frame(predprob = rf_ptbhist32_multi_roc$predictor, outcome_ptb = rf_ptbhist32_multi_roc$response) 

predprob_bins$bin100 <- ntile(rf_ptbhist32_multi_roc$predictor, n = 100)
table(predprob_bins$bin100)
predprob_bins$bin1000 <- ntile(rf_ptbhist32_multi_roc$predictor, n = 1000)
table(predprob_bins$bin1000)


ptb_predprob_bins100 <- predprob_bins %>% group_by(bin100) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))

view(ptb_predprob_bins100)
plot(x = ptb_predprob_bins100$bin100, y = ptb_predprob_bins100$ptb1_perc)


ptb_predprob_bins100_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins100_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins100$predprob_mean){
  
  print(predprob_i)
  pred_rf_classi <- ifelse(rf_ptbhist32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_ptbhist32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins100_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                         sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                         specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins100_df <- rbind(ptb_predprob_bins100_df, ptb_predprob_bins100_dfi)
  #print(ptb_predprob_bins100_df)
}
view(ptb_predprob_bins100_df)

ptb_predprob_bins100_df <- cbind(ptb_predprob_bins100, ptb_predprob_bins100_df)
sum(ptb_predprob_bins100_df$predprob_mean == ptb_predprob_bins100_df$predprob_mean_i)
ptb_predprob_bins100_df <- ptb_predprob_bins100_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

##
ptb_predprob_bins1000 <- predprob_bins %>% group_by(bin1000) %>% 
  reframe(predprob_mean = mean(predprob),
          n = n(),
          ptb_1 = sum(outcome_ptb == 1),
          ptb_0 = sum(outcome_ptb == 0),
          ptb1_perc = sum(outcome_ptb == 1) / (sum(outcome_ptb == 1)+sum(outcome_ptb == 0))*100) %>% 
  mutate(ptb1_perc = ifelse((ptb1_perc/100)*n < 10, NA, ptb1_perc))
view(ptb_predprob_bins1000)
sum(is.na(ptb_predprob_bins1000$ptb1_perc))
plot(x = ptb_predprob_bins1000$bin1000, y = ptb_predprob_bins1000$ptb1_perc)


ptb_predprob_bins1000_df <- data.frame((matrix(nrow = 0, ncol = 3)))
colnames(ptb_predprob_bins1000_df) <- c("predprob_mean_i", "sensitivity", "specificity")

for (predprob_i in ptb_predprob_bins1000$predprob_mean){
  
  #print(predprob_i)
  pred_rf_classi <- ifelse(rf_ptbhist32_multi_roc$predictor >= predprob_i, 1, 0) %>% as.factor() 
  pred_rf_class_cmi <- confusionMatrix(pred_rf_classi, rf_ptbhist32_multi_roc$response, positive = "1")
  pred_rf_class_cmi
  
  ptb_predprob_bins1000_dfi <- data.frame(predprob_mean_i = predprob_i, 
                                          sensitivity = pred_rf_class_cmi[[4]][[1]], 
                                          specificity = pred_rf_class_cmi[[4]][[2]])
  
  ptb_predprob_bins1000_df <- rbind(ptb_predprob_bins1000_df, ptb_predprob_bins1000_dfi)
  #print(ptb_predprob_bins1000_df)
}
view(ptb_predprob_bins1000_df)
ptb_predprob_bins1000_df <- cbind(ptb_predprob_bins1000, ptb_predprob_bins1000_df)
sum(ptb_predprob_bins1000_df$predprob_mean == ptb_predprob_bins1000_df$predprob_mean_i)
ptb_predprob_bins1000_df <- ptb_predprob_bins1000_df %>% select(-c(ptb_1, ptb_0, predprob_mean_i))

write.csv2(ptb_predprob_bins100_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins100_df.csv")
write.csv2(ptb_predprob_bins1000_df, "case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins1000_df.csv")




#### PLOTS ####
library(ggplot2)

full32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins100_df.csv")
full32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins100_df.csv")
full32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_multi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_multi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_primi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/full32_primi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins100_yspec")
dev.off()

#####

rest32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins100_df.csv")
rest32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins100_df.csv")
rest32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_multi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_multi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_primi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/rest32_primi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins100_yspec")
dev.off()

####
medical32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins100_df.csv")
medical32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins100_df.csv")
medical32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_multi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_multi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_primi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/medical32_primi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins100_yspec")
dev.off()


####
ptbhist32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins100_df.csv")
ptbhist32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins100_df.csv")

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/ptbhist32_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/ptbhist32_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins100_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/ptbhist32_multi_predprob_bins100_ysens.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins100_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins100_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/ptbhist32_multi_predprob_bins100_yspec.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins100_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins100_yspec")
dev.off()



#### 1000 ####


full32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins1000_df.csv")
full32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins1000_df.csv")
full32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_multi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_multi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_primi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/full32_primi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins1000_yspec")
dev.off()

#####

rest32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins1000_df.csv")
rest32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins1000_df.csv")
rest32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_multi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_multi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_primi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/rest32_primi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins1000_yspec")
dev.off()

####
medical32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins1000_df.csv")
medical32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins1000_df.csv")
medical32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_multi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_multi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_primi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/medical32_primi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins1000_yspec")
dev.off()


####
ptbhist32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins1000_df.csv")
ptbhist32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins1000_df.csv")

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/ptbhist32_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/ptbhist32_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins1000_yspec")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/ptbhist32_multi_predprob_bins1000_ysens.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = sensitivity))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins1000_ysens")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/bins1000/ptbhist32_multi_predprob_bins1000_yspec.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = specificity))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins1000_yspec")
dev.off()


#### outcome on y-axis ####


full32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins100_df.csv")
full32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins100_df.csv")
full32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/full32_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/full32_multi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/full32_primi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins100_yptb1_perc")
dev.off()

#####

rest32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins100_df.csv")
rest32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins100_df.csv")
rest32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/rest32_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/rest32_multi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/rest32_primi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins100_yptb1_perc")
dev.off()


####
medical32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins100_df.csv")
medical32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins100_df.csv")
medical32_primi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins100_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/medical32_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/medical32_multi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/medical32_primi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins100_yptb1_perc")
dev.off()


####
ptbhist32_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins100_df.csv")
ptbhist32_multi_predprob_bins100_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins100_df.csv")

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/ptbhist32_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins100_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/ptbhist32_multi_predprob_bins100_yptb1_perc.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins100_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins100_yptb1_perc")
dev.off()

#### outcome on y-axis: 1000 bins ####

full32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_predprob_bins1000_df.csv")
full32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_multi_predprob_bins1000_df.csv")
full32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_full32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/full32_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/full32_multi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_multi_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/full32_primi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(full32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("full32_primi_predprob_bins1000_yptb1_perc")
dev.off()

#####

rest32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_predprob_bins1000_df.csv")
rest32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_multi_predprob_bins1000_df.csv")
rest32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_rest32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/rest32_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/rest32_multi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_multi_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/rest32_primi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(rest32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("rest32_primi_predprob_bins1000_yptb1_perc")
dev.off()


####
medical32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_predprob_bins1000_df.csv")
medical32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_multi_predprob_bins1000_df.csv")
medical32_primi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_medical32_primi_predprob_bins1000_df.csv")


png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/medical32_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/medical32_multi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_multi_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/medical32_primi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(medical32_primi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("medical32_primi_predprob_bins1000_yptb1_perc")
dev.off()


####
ptbhist32_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_predprob_bins1000_df.csv")
ptbhist32_multi_predprob_bins1000_df <- read.csv2("case1_preterm_birth/output/predprob_bins_spec_sens_tables/ptb_rf_ptbhist32_multi_predprob_bins1000_df.csv")

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/ptbhist32_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(ptbhist32_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("ptbhist32_predprob_bins1000_yptb1_perc")
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/binscatter_plot/yptb1_perc/bins1000/ptbhist32_multi_predprob_bins1000_yptb1_perc.png",
    width= 600, height = 450)
ggplot(ptbhist32_multi_predprob_bins1000_df, aes(x = predprob_mean, y = ptb1_perc))+
  geom_point()+
  ggtitle("ptbhist32_multi_predprob_bins1000_yptb1_perc")
dev.off()



