#### BINS BINS BINS ####
library(tidyverse)
library(pROC)
library(caret)
setwd("H:/Mirthe/")

rf_full32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_full32_roc.rds")
rf_full32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_full32_multi_roc.rds")
rf_full32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_full32_primi_roc.rds")

rf_rest32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_rest32_roc.rds")
rf_rest32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_rest32_multi_roc.rds")
rf_rest32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_rest32_primi_roc.rds")

rf_medical32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_medical32_roc.rds")
rf_medical32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_medical32_multi_roc.rds")
rf_medical32_primi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_medical32_primi_roc.rds")

rf_ptbhist32_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_ptbhist32_roc.rds")
rf_ptbhist32_multi_roc <- readRDS("H:/Mirthe/case1_preterm_birth/PTB_output/rf_ptbhist32_multi_roc.rds")

#### plot - predprob x ####

#### full ####
png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_roc_sens.png",
    width= 600, height = 450)
plot(rf_full32_roc$thresholds, rf_full32_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_roc_spec.png",
    width= 600, height = 450)
plot(rf_full32_roc$thresholds, rf_full32_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_multi_roc_sens.png",
    width= 600, height = 450)
plot(rf_full32_multi_roc$thresholds, rf_full32_multi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_multi_roc_spec.png",
    width= 600, height = 450)
plot(rf_full32_multi_roc$thresholds, rf_full32_multi_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_primi_roc_sens.png",
    width= 600, height = 450)
plot(rf_full32_primi_roc$thresholds, rf_full32_primi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_full32_primi_roc_spec.png",
    width= 600, height = 450)
plot(rf_full32_primi_roc$thresholds, rf_full32_primi_roc$specificities)
dev.off()


#### rest ####
png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_roc_sens.png",
    width= 600, height = 450)
plot(rf_rest32_roc$thresholds, rf_rest32_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_roc_spec.png",
    width= 600, height = 450)
plot(rf_rest32_roc$thresholds, rf_rest32_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_multi_roc_sens.png",
    width= 600, height = 450)
plot(rf_rest32_multi_roc$thresholds, rf_rest32_multi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_multi_roc_spec.png",
    width= 600, height = 450)
plot(rf_rest32_multi_roc$thresholds, rf_rest32_multi_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_primi_roc_sens.png",
    width= 600, height = 450)
plot(rf_rest32_primi_roc$thresholds, rf_rest32_primi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_rest32_primi_roc_spec.png",
    width= 600, height = 450)
plot(rf_rest32_primi_roc$thresholds, rf_rest32_primi_roc$specificities)
dev.off()


#### medical ####
png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_roc_sens.png",
    width= 600, height = 450)
plot(rf_medical32_roc$thresholds, rf_medical32_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_roc_spec.png",
    width= 600, height = 450)
plot(rf_medical32_roc$thresholds, rf_medical32_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_multi_roc_sens.png",
    width= 600, height = 450)
plot(rf_medical32_multi_roc$thresholds, rf_medical32_multi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_multi_roc_spec.png",
    width= 600, height = 450)
plot(rf_medical32_multi_roc$thresholds, rf_medical32_multi_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_primi_roc_sens.png",
    width= 600, height = 450)
plot(rf_medical32_primi_roc$thresholds, rf_medical32_primi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_medical32_primi_roc_spec.png",
    width= 600, height = 450)
plot(rf_medical32_primi_roc$thresholds, rf_medical32_primi_roc$specificities)
dev.off()



#### ptbhist ####
png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_ptbhist32_roc_sens.png",
    width= 600, height = 450)
plot(rf_ptbhist32_roc$thresholds, rf_ptbhist32_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_ptbhist32_roc_spec.png",
    width= 600, height = 450)
plot(rf_ptbhist32_roc$thresholds, rf_ptbhist32_roc$specificities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_ptbhist32_multi_roc_sens.png",
    width= 600, height = 450)
plot(rf_ptbhist32_multi_roc$thresholds, rf_ptbhist32_multi_roc$sensitivities)
dev.off()

png(file = "case1_preterm_birth/output/predprob_bins_spec_sens_tables/plot_xpredprob_ysens_spec/rf_ptbhist32_multi_roc_spec.png",
    width= 600, height = 450)
plot(rf_ptbhist32_multi_roc$thresholds, rf_ptbhist32_multi_roc$specificities)
dev.off()

