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
predictors <- readRDS("case1_preterm_birth/repo/PTB_data/predictors_rest_model.rds")
glimpse(predictors)

# If multiple imputations have been generated before, you can simply load the mids object, skip to line 150

#### 1. Prepare predictors data for multiple imputation ####

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
                          '7' = '5+'),
    interpreg_nvt = ifelse(interpreg==-88, TRUE, FALSE), #generate indicator of "nvt", after imputation replate imputed values in interpreg where this indicator is TRUE with "nvt"
    interpreg = ifelse(interpreg<0, NA, interpreg), #replace 'geen data' and 'nvt'  with NA; in some cases, interpreg is negative because of mistake in Perined data; this variable will be imputed as continious
    age_at_concp_fa_missing = is.na(age_at_concp_fa)) #generate indicator of missing father; age of father var will be imputed as continous


#Base R solution for 
predictors$house_ownership_mo[predictors$house_ownership_mo=="Onbekend"] <- NA

#predictors$N_vroeg_24_37[predictors$N_vroeg_24_37=="geen data"] <- NA
predictors$N_vooraf_sga[predictors$N_vooraf_sga=="geen data"] <- NA
#predictors$vooraf_zw_vroeg_24_37[predictors$vooraf_zw_vroeg_24_37=="geen data"] <- NA
predictors$vooraf_sga[predictors$vooraf_sga=="geen data"] <- NA
predictors$vooraf_zw_vroeg_24_28[predictors$vooraf_zw_vroeg_24_28=="geen data"] <- NA
predictors$vooraf_zw_vroeg_28_34[predictors$vooraf_zw_vroeg_28_34=="geen data"] <- NA
predictors$vooraf_zw_vroeg_34_37[predictors$vooraf_zw_vroeg_34_37=="geen data"] <- NA
predictors$N_vroeg_24_28[predictors$N_vroeg_24_28=="geen data"] <- NA
predictors$N_vroeg_28_34[predictors$N_vroeg_28_34=="geen data"] <- NA
predictors$N_vroeg_34_37[predictors$N_vroeg_34_37=="geen data"] <- NA

predictors <- predictors%>%
  droplevels()

predictors<-predictors%>%
  slice_sample(prop=0.05)


#predictors<-predictors%>%
#   slice_sample(prop=0.50)
dim(predictors)
#Explore missing patterns
#miss_case_smr<-miss_case_summary(predictors)
#cases_low_miss<-miss_case_smr%>%filter(pct_miss<50)%>%pull(case)
#exclude  cases wtih >50% missings
#predictors<-predictors[cases_low_miss,] # excluded cases with >50% missings
dim(predictors)


### 2. Create train and test split for predictors
# In ML, when imputing missing observations, a train/test split should be made before imputation
# In training the prediction model, the algorithm should not be able to learn from the test data. Test data remains separate to evaluate predictive performance of the trained model.
set.seed(8151)
# assign split value per row, with probability of 0.7 for train and 0.3 for test
split <- sample(c(TRUE, FALSE), nrow(predictors), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
vroeg_geb_train <- predictors[split, ]
pretbirth_test <- predictors[!split, ]


#### 3. Multiple imputation with the R package MICE ####
### 3.1 Train data 

#Matrix of predictors
#generate matrix where variables are chosen as predictors if correlation is at least 0.2


vars_to_exclude <- c("year_conception", "interpreg_nvt","age_at_concp_fa_missing", "Rinpersoons_Kind", 
                     "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", "rins_fa", "rin_fa", "age_at_concp_fa_missing", "interpreg_nvt")

for (var in vars_to_exclude) {
  vroeg_geb_train[,var] <-complete(vroeg_geb_train[,var])
}
predmatrix = quickpred(vroeg_geb_train, mincor = 0.2)

#Prediction matrix can be further edited manually (to add or remove some variables as predictors)
predmatrix[,c("year_conception", "interpreg_nvt","age_at_concp_fa_missing", "Rinpersoons_Kind", "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", "rins_fa", "rin_fa", "age_at_concp_fa_missing", "interpreg_nvt")]<- 0  #leave these vars from the set of predictors
predmatrix[c("year_conception", "interpreg_nvt","age_at_concp_fa_missing", "Rinpersoons_Kind", "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", "rins_fa", "rin_fa", "age_at_concp_fa_missing", "interpreg_nvt"), ]<- 0  #leave these vars from vars to be predicted


predmatrix[c("N_vroeg_24_28", "N_vroeg_28_34", "N_vroeg_34_37", "vooraf_zw_vroeg_24_28", "vooraf_zw_vroeg_28_34", "vooraf_zw_vroeg_34_37"), ] <-0 # set all predictors as 0 
predmatrix[c("N_vroeg_24_28", "N_vroeg_28_34", "N_vroeg_34_37", "vooraf_zw_vroeg_24_28", "vooraf_zw_vroeg_28_34", "vooraf_zw_vroeg_34_37"), c("lft_concep", "grav", "par", "age_at_concp_fa")]<-1

# use mice to generate multiple imputations
# be aware: this takes a long time to run (approx 8 days)
# this syntax imputes 3 chains (if time allows, 5 is usually recommended)
start_time<-Sys.time()
mi <- mice(vroeg_geb_train, m=3, pred = predmatrix, seed = 815, maxit=40, defaultMethod = c("pmm", "logreg", "pmm", "polr"),
           print = F) 
#record time needed to impute, can be handy for planning
end_time<-Sys.time()
time.taken<-round(end_time - start_time, 2)
time.taken



# save the multiple imputed data; a large mids object with m = 3 imputations
save(mi, file="H:/Mirthe/case1_preterm_birth/repo/PTB_data/MI_vroeggeb_with_year_v7mini.rda")

load("H:/Mirthe/case1_preterm_birth/repo/PTB_data/MI_vroeggeb_with_year_v3.rda")

# look at the multiple imputed data and check the quality of imputations
#Check logged events 
logged<-data.frame(mi$loggedEvents)

#check convergence
plot(mi, layout=c(4,4))

#for continuous var look at density plots
densityplot(mi, layout=c(2,2))

#Strip plots to see whether imputed variables are in plausible range
a<-stripplot(mi, educationlevel_fa~.imp, pch=c(21,20), cex=c(1,1.5))
b<-stripplot(mi, grav~.imp, pch=c(21,20), cex=c(1,1.5))
c<-stripplot(mi, plhh_partner_child~.imp, pch=c(21,20), cex=c(1,1.5))
d<-stripplot(mi, educationlevel_mo~.imp, pch=c(21,20), cex=c(1,1.5))
e<-stripplot(mi, N_vooraf_sga~.imp, pch=c(21,20), cex=c(1,1.5))
f<-stripplot(mi, interpreg~.imp, pch=c(21,20), cex=c(1,1.5))
g<-stripplot(mi, amddd1ond~.imp, pch=c(21,20), cex=c(1,1.5))
h<-stripplot(mi, N_vroeg_24_28~.imp, pch=c(21,20), cex=c(1,1.5))
i<-stripplot(mi, vooraf_sga~.imp, pch=c(21,20), cex=c(1,1.5))

ggarrange(a,b,c,d,e,f,g,h,i, ncol=3, nrow=3)

#Look at summary of imputed variables 
sapply(Filter(function(x) nrow(x) >0, mi$imp), 
       function(x) summary(unlist(x)))


### 3.2 Test data

# to deal with a bug in mice.mids when there are (complete) character variables in the data set 
pretbirth_test <- pretbirth_test %>% 
  mutate(Rinpersoon_Kind = as.numeric(Rinpersoon_Kind),
         Rinpersoon_Moeder = as.numeric(Rinpersoon_Moeder),
         Rinpersoon_Moeder = ifelse(is.na(Rinpersoon_Moeder), 99, Rinpersoon_Moeder),
         rin_fa = as.numeric(rin_fa),
         rins_fa = if_else(is.na(rins_fa), "N", rins_fa),
         rins_fa = as.factor(rins_fa))
# use mice.mids to generate multiple imputations for the test data using the MI model generated on the train data
# be aware: this takes a long time to run
mi_test <- mice.mids(mi, maxit = 1, newdata = pretbirth_test, seed = 8151)
save(mi_test, file="H:/Mirthe/case1_preterm_birth/repo/PTB_data/MItest_vroeggeb_with_year_test_v2.rda")

#################################################################################
#### 4. Prepare MI data for analysis ####

# If multiple imputations have been generated previously, you can simply load the mids object
# if not, run the code above
load("H:/Mirthe/case1_preterm_birth/repo/PTB_data/MI_vroeggeb_with_year.rda")
load("H:/Mirthe/case1_preterm_birth/repo/PTB_data/MItest_vroeggeb_with_year_test.rda")

### 4.1 Train data
# MI is a mids object: get the complete data set by replacing missing values by imputations 
mi_complete <- mi %>% 
  mice::complete("all") 

# look at summary statistics of the m = 1 imputed data
sapply(mi_complete$'1', summary)

# look at the outcome variable preterm birth
table(mi_complete$'1'$premature_birth)
prop.table(table(mi_complete$'1'$premature_birth)) * 100

# we restructure variables in the MI dataset: we kept the continuous variables in the MI data to retain as much info as possible
# however we use categorical variables in the analysis to incorporate the 'nvt'(first pregnancy) category and the N+ category
# we create a function to restructure the variables because we need to do this for all (m =)3 imputed datasets
table(predictors$age_at_concp_fa)
func_postprocess_cat <- function(mi_list) {
  mi_list %>% 
    mutate(
      # in consultation with birth care professionals we decided to include age of father as a categorical variable, 
      # and create a category for cases in which the father is missing
      age_at_concp_fa_cat = cut(age_at_concp_fa, 
                                breaks = seq(11, 89, by = 5),
                                include.lowest = TRUE,
                                # use these two lines for train data 
                                labels = paste(seq(11, 89, by = 5)[-length(seq(11, 89, by = 5))],
                                               "-", seq(11, 89, by = 5)[-1])),
      
      age_at_concp_fa_cat = if_else(age_at_concp_fa_missing == 1, "vader_NA", age_at_concp_fa_cat),
      age_at_concp_fa_cat = as.factor(age_at_concp_fa_cat),
      age_at_concp_fa_cat = fct_collapse(age_at_concp_fa_cat, "71+" = c("71 - 76",  "76 - 81", "81 - 86")),
      age_at_concp_fa_cat = if_else(is.na(age_at_concp_fa_cat), "71+", age_at_concp_fa_cat), # temp solution because cut deletes highest observation, age 89, we do want to include this individual in the 71+ group
      age_at_concp_fa_cat = as.factor(age_at_concp_fa_cat),
      
      # we incorporate interpregnancy interval as a categorical variable and include a 'nvt' category for first pregnancies 
      interpreg_cat = case_when(
        interpreg < (30.436*6) ~ "<6 months",
        interpreg >= (30.436*6) & interpreg < (30.436*12) ~ "6-12 months", 
        interpreg >= (30.436*12) & interpreg < (30.436*18) ~ "12-18 months", 
        interpreg >= (30.436*18) & interpreg < (30.436*24) ~ "18-24 months",
        interpreg >= (30.436*24) & interpreg < (30.436*30) ~ "24-30 months",
        interpreg >= (30.436*30) ~ ">30 months"),
      interpreg_cat = ifelse(interpreg_nvt == 1, "nvt", interpreg_cat),
      interpreg_cat = as_factor(interpreg_cat),
      
      # we incorporate the term of the pregnancy at the moment of the first pregnancy consultation as a categorical variable to createa category  for 16+ weeks
      amddd1ond_cat = ifelse(amddd1ond>=0 & amddd1ond<=70, '0-70 dagen', 
                             ifelse(amddd1ond > 70 & amddd1ond<=112, '71 - 112 dagen',
                                    ifelse(amddd1ond > 112, '>112 dagen', NA))), # check there is no NA
      amddd1ond_cat = as.factor(amddd1ond_cat), 
      
      # we incorporate gravidity as a categorical variable and create a 10+ category
      grav_cat = ifelse(grav>9, '10+', grav), 
      grav_cat = as.factor(grav_cat),
      par_cat = ifelse(par>9, '10+', par),
      par_cat = as.factor(par_cat)) %>% 
    
    select(-c(age_at_concp_fa, age_at_concp_fa_missing, interpreg, interpreg_nvt, amddd1ond, grav, par))
}

# function to set ordered factors to factor for m = 3
func_postprocess_ordered <- function(mi_list) {
  mi_list %>% 
    mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))
}

# use lapply to apply the function to the m = 3 elements (laplly returns a list of THE same length as C, each element of which is the result of applying FUN)
mi_complete <- lapply(mi_complete, func_postprocess_cat)
mi_complete <- lapply(mi_complete, func_postprocess_ordered)

# check the restructured variabels
table(mi_complete$'3'$age_at_concp_fa_cat)
class(mi_complete$'1'$age_at_concp_fa_cat)
sum(is.na(mi_complete$'1'$age_at_concp_fa_cat))
table(mi_complete$'1'$interpreg_cat)
class(mi_complete$'1'$interpreg_cat)
table(mi_complete$'1'$amddd1ond_cat)
class(mi_complete$'1'$amddd1ond_cat)
table(mi_complete$'1'$par_cat)
class(mi_complete$'1'$par_cat)


# function to create the N_vroeg_24_37 variable based on the 24_28, 28_34 and 34_37 variables after multiple imputation 
func_postprocess_N_vroeg <- function(mi_list) {
  mi_list %>% 
    mutate(
      N_vroeg_24_37 = case_when(
        N_vroeg_24_28 == "nvt" | N_vroeg_28_34 == "nvt" | N_vroeg_34_37 == "nvt" ~ "nvt",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "0" ~ "0",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "1" ~ "1",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "0" ~ "1",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "0" ~ "1",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "1" ~ "2",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "0" ~ "2",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "1" ~ "2",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "1" ~ "3",
        
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "0" ~ "2",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "2" ~ "2",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "0" ~ "2",
        
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "0" ~ "3",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "1" ~ "3",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "1" ~ "4",
        
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "0" ~ "3",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "1" ~ "3",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "1" ~ "4",
        
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "2" ~ "3",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "2" ~ "3",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "2" ~ "4",
        
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "0" ~ "4",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "2" ~ "4",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "2" ~ "4",
        
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "1" ~ "5",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "2" ~ "5",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "2" ~ "5",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "2" ~ "6",
        
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "0" ~ "3+",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "3+" ~ "3+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "0" ~ "4+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "3+" ~ "4+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "0" ~ "5+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "0" & N_vroeg_34_37 == "3+" ~ "5+",
        
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "1" ~ "4+",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "3+" ~ "4+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "1" ~ "5+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "3+" ~ "5+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "1" ~ "6+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "1" & N_vroeg_34_37 == "3+" ~ "6+",
        
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "2" ~ "5+",
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "3+" ~ "5+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "2" ~ "6+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "3+" ~ "6+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "2" ~ "7+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "2" & N_vroeg_34_37 == "3+" ~ "7+",
        
        N_vroeg_24_28 == "0" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "3+" ~ "6+",
        N_vroeg_24_28 == "1" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "3+" ~ "7+",
        N_vroeg_24_28 == "2" & N_vroeg_28_34 == "3+" & N_vroeg_34_37 == "3+" ~ "8+",
        TRUE ~ NA_character_
        
      ), 
      # we group the higher categories to avoid categories with very little observations
      N_vroeg_24_37 = fct_collapse(N_vroeg_24_37, "3+" = c("3+", "4", "4+", "5", "5+", "6", "6+", "7+", "8+")),
      N_vroeg_24_37 = as.factor(N_vroeg_24_37))
}


# function to create the vooraf_zw_vroeg_24_37 variable based on the 24_28, 28_34 and 34_37 variables
func_postprocess_vooraf_vroeg <- function(mi_list) {
  mi_list %>% 
    mutate(
      vooraf_zw_vroeg_24_37 = case_when(
        vooraf_zw_vroeg_24_28 == "nvt" | vooraf_zw_vroeg_28_34 == "nvt" | vooraf_zw_vroeg_34_37 == "nvt" ~ "nvt",
        vooraf_zw_vroeg_24_28 == "ja" | vooraf_zw_vroeg_28_34 == "ja" | vooraf_zw_vroeg_34_37 == "ja" ~ "ja",
        vooraf_zw_vroeg_24_28 == "nee" | vooraf_zw_vroeg_28_34 == "nee" | vooraf_zw_vroeg_34_37 == "nee" ~ "nee"),
      vooraf_zw_vroeg_24_37 = as.factor(vooraf_zw_vroeg_24_37))
}

# apply the functions to the m = 3 imputed datasets
mi_complete <- lapply(mi_complete, func_postprocess_N_vroeg)
mi_complete <- lapply(mi_complete, func_postprocess_vooraf_vroeg)

# look at the generated variables
class(mi_complete$'1'$N_vroeg_24_37)
table(mi_complete$'1'$N_vroeg_24_37)
sum(is.na(mi_complete$'1'$N_vroeg_24_37))
table(mi_complete$'1'$vooraf_zw_vroeg_24_37)
class(mi_complete$'1'$vooraf_zw_vroeg_24_37)


# consider including household income of the mother as a categorical variable
# to create the categories we use year-dependent cutoffs at the 10th, 40th and 80th percentile
hh_income_perc_year_cbs <- readRDS("case1_preterm_birth/data/hh_income_perc_year_cbs.rds")
hh_income_perc_year_cbs <- hh_income_perc_year_cbs %>% 
  select(c(year, perc_10, perc_40, perc_80)) %>% 
  mutate(year = as.factor(year))

func_postprocess_income <- function(mi_list) {
  mi_list %>% 
    left_join(hh_income_perc_year_cbs, by = c("year_conception" = "year")) %>% 
    mutate(income_hh_mo_cat = case_when(
      income_hh_mo < perc_10 ~ "bestaansmin",
      income_hh_mo < perc_40 ~ "laag",
      income_hh_mo < perc_80 ~ "midden",
      income_hh_mo >= perc_80 ~ "hoog",
      TRUE ~ NA_character_),
      income_hh_mo_cat = as.factor(income_hh_mo_cat)) %>% 
    select(-c(perc_10, perc_40, perc_80, year_conception, income_hh_mo))
} 

# look at the generated variables
mi_complete <- lapply(mi_complete, func_postprocess_income)
table(mi_complete$'1'$income_hh_mo_cat)
class(mi_complete$'1'$income_hh_mo_cat)
sum(is.na(mi_complete$'1'$income_hh_mo_cat))

# exclude geslacht because it is not always known at 16 weeks of gestational age
func_postprocess_deselectgesl <- function(mi_list) {
  mi_list %>% 
    select(-c(gesl))
}
mi_complete <- lapply(mi_complete, func_postprocess_deselectgesl)

# exclude ID variables and the secondary outcome variable for analysis
func_postprocess_deselectid <- function(mi_list) {
  mi_list %>% 
    select(-c(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, rins_fa, rin_fa, premature_birth37))
}
mi_complete <- lapply(mi_complete, func_postprocess_deselectid)

sapply(mi_complete$'1', summary)

# save the transformed, multiple imputed predictors data
saveRDS(mi_complete, "case1_preterm_birth/repo/PTB_data/predictors_rest_model32_mi_complete.rds")


### 4.2 Test data
# MI is a mids object: get the complete data set by replacing missing values by imputations 
mi_test_comp <- mi_test %>% mice::complete("all")

# look at summary statistics of the m = 1 imputed data
sapply(mi_test_comp$'1', summary)

# apply all of the above functions to the m = 3 imputed data sets
mi_test_comp <- lapply(mi_test_comp, func_postprocess_cat)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_ordered)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_N_vroeg)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_vooraf_vroeg)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_income)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_deselectgesl)
mi_test_comp <- lapply(mi_test_comp, func_postprocess_deselectid)

# look at the generated variables
sapply(mi_test_comp$'2', summary)
class(mi_test_comp$'1'$amddd1ond_cat)
table(mi_test_comp$'1'$N_vroeg_24_37)
sum(is.na(mi_test_comp$'1'$N_vroeg_24_37))
class(mi_test_comp$'1'$N_vroeg_24_28)


# save the transformed, multiple imputed predictors data
saveRDS(mi_test_comp, "case1_preterm_birth/repo/PTB_data/predictors_rest_model32_mi_test_comp.rds")
