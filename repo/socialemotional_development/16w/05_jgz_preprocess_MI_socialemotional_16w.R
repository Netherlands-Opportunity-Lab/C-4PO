#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 05_jgz_preprocess_MI ####
# THE GOAL OF THIS SCRIPT IS TO DEAL WITH MISSING DATA USING THE R PACKAGE MICE FOR MULTIPLE IMPUTATION 


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


#### 1. Multiple imputation using MICE ####

# this is the predictor dataset for the case overweight 
predictors <- readRDS("case_jgz/data_outcome/sdq_16w/sdq_predictors_p.rds")

#### 1.1 Prepare the predictors data set for imputation ####

predictors<-predictors%>%
  replace_with_na_at(.vars = "geslacht", condition = ~.x == "onbekend")%>%
  replace_with_na_at(.vars = "house_ownership_mo", condition = ~.x == "Onbekend")%>%
  replace_with_na_at(.vars = "income_hh_source_mo", condition = ~.x == "Huishoudensinkomen onbekend")%>%
  mutate(
    income_hh_source_mo=droplevels(income_hh_source_mo), 
    house_ownership_mo = droplevels(house_ownership_mo),
    geslacht = droplevels(geslacht), 
    GBA_generation_mo = droplevels(GBA_generation_mo), 
    ED_rentown_mo = droplevels(ED_rentown_mo),
    ED_rentown_fa = droplevels(ED_rentown_fa),
    SPOLIS_contract_mo = droplevels(SPOLIS_contract_mo), 
    SPOLIS_contract_fa = droplevels(SPOLIS_contract_fa), 
    PLHH_mo = droplevels(PLHH_mo), 
    AANTALPERSHHf_mo = recode(AANTALPERSHHf_mo, '1' = '1 of 2', # 1 person in HH is probably a mistake, merged with 2
                              '2' = '1 of 2'), 
    TYPHH_mo = droplevels(TYPHH_mo), 
    TYPHH_mo = recode(TYPHH_mo, 'Niet-gehuwd paar zonder kinderen' = 'Overig huishouden',
                      'Gehuwd paar zonder kinderen' = 'Overig huishouden', 
                      'Institutioneel huishouden' = 'Overig huishouden'), 
    LAND_ETNG_mo = droplevels(LAND_ETNG_mo),
    LAND_ACHTS_mo = droplevels(LAND_ACHTS_mo), 
    income_hh_source_mo = recode(income_hh_source_mo, 
                       'Inkomen uit vermogen' = 'Inkomen uit vermogen of loon', # 3 cat merged
                       'Loon' = 'Inkomen uit vermogen of loon', # 3 cat merged                                
                       'Werkloosheidsuitkering' ='Werkloosheidsuitkering',                 
                       'Loon directeur-grootaandeelhouder'  = 'Inkomen uit vermogen of loon', #  # 3 cat merged  
                       'Bijstandsuitkering' = 'Bijstandsuitkering of Uitkering sociale voorziening overig',      # 2 cat merged               
                       'Winst zelfstandig ondernemer' = 'Winst zelfstandig ondernemer',           
                       'Uitkering sociale voorziening overig'  = 'Bijstandsuitkering of Uitkering sociale voorziening overig',  # 2 cat merged     
                       'Inkomen overige zelfstandige' = 'Overige inkomen, mix', # mix van 3 cat met zeer kleine N "Pensioenuitkering",   "Studiefinanciering",  "Inkomen overige zelfstandige"     
                       'Uitkering ziekte/arbeidsongeschiktheid' ='Uitkering ziekte/arbeidsongeschiktheid' ,
                       'Pensioenuitkering'   = 'Overige inkomen, mix', # mix van 3 cat met zeer kleine N "Pensioenuitkering",   "Studiefinanciering",  "Inkomen overige zelfstandige"                         
                       'Studiefinanciering'  = 'Overige inkomen, mix'), # mix van 3 cat met zeer kleine N "Pensioenuitkering",   "Studiefinanciering",  "Inkomen overige zelfstandige"                         
    SECM_mo = recode(SECM_mo, 
                       '11'= 'Werknemer',
                       '12'= 'Directeur-grootaandeelhouder',
                       '13'= 'Zelfstandig ondernemer',
                       '14'= 'Overige zelfstandige',
                       '21'= 'Ontvanger werkloosheidsuitkering',
                       '22'= 'Ontvanger bijstandsuitkering',
                       '23'= 'Ontvanger uitkering sociale voorz.overig',
                       '24'= 'Ontvanger uitkering ziekte/AO/pension',  # merged 2 catergories
                       '25'= 'Ontvanger uitkering ziekte/AO/pension',  # merged 2 catergories
                       '26'= 'Nog niet schoolg./schol./stud. met or zonder ink', # merged 2 catergories
                       '31'= 'Nog niet schoolg./schol./stud. met or zonder ink', # merged 2 catergories
                       '32'= 'Overig', # merged 2 categories
                       '15'= 'Overig'), # merged 2 categories 
    SECM_fa = recode(SECM_fa, 
                       '11'= 'Werknemer',
                       '12'= 'Directeur-grootaandeelhouder',
                       '13'= 'Zelfstandig ondernemer',
                       '14'= 'Overige zelfstandige',
                       '21'= 'Ontvanger werkloosheidsuitkering',
                       '22'= 'Ontvanger bijstandsuitkering',
                       '23'= 'Ontvanger uitkering sociale voorz.overig',
                       '24'= 'Ontvanger uitkering ziekte/AO/pension',  # merged 2 catergories
                       '25'= 'Ontvanger uitkering ziekte/AO/pension',  # merged 2 catergories
                       '26'= 'Nog niet schoolg./schol./stud. met of zonder ink', # merged 2 catergories
                       '31'= 'Nog niet schoolg./schol./stud. met of zonder ink', # merged 2 catergories
                       '32'= 'Overig', # merged 2 categories
                       '15'= 'Overig'), # merged 2 categories 
    SPOLIS_wages_mo = ifelse(is.na(SPOLIS_wages_mo) & SECM_mo=="Ontvanger werkloosheidsuitkering" | 
                        SECM_mo=="Ontvanger bijstandsuitkering" | SECM_mo=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_mo=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_mo=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_mo=="Overig zonder inkomen", 0, SPOLIS_wages_mo), 
    SPOLIS_paidhours_mo = ifelse(is.na(SPOLIS_paidhours_mo) & SECM_mo=="Ontvanger werkloosheidsuitkering" | 
                        SECM_mo=="Ontvanger bijstandsuitkering" | SECM_mo=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_mo=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_mo=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_mo=="Overig zonder inkomen", 0, SPOLIS_paidhours_mo),
    SPOLIS_contract_mo = ifelse(is.na(SPOLIS_contract_mo) & SECM_mo=="Ontvanger werkloosheidsuitkering" | 
                        SECM_mo=="Ontvanger bijstandsuitkering" | SECM_mo=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_mo=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_mo=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_mo=="Overig zonder inkomen", 4, SPOLIS_contract_mo), 
    SPOLIS_contract_mo = as_factor(SPOLIS_contract_mo),
    SPOLIS_contract_mo = recode(SPOLIS_contract_mo, '1' = '1', 
                        '2' = '2', 
                        '3' = '3', 
                        '4' = 'niet werkend volgens SECM'),
    SPOLIS_wages_fa = ifelse(is.na(SPOLIS_wages_fa) & SECM_fa=="Ontvanger werkloosheidsuitkering" | 
                        SECM_fa=="Ontvanger bijstandsuitkering" | SECM_fa=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_fa=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_fa=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_fa=="Overig zonder inkomen", 0, SPOLIS_wages_fa), 
    SPOLIS_paidhours_fa = ifelse(is.na(SPOLIS_paidhours_fa) & SECM_fa=="Ontvanger werkloosheidsuitkering" | 
                        SECM_fa=="Ontvanger bijstandsuitkering" | SECM_fa=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_fa=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_fa=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_fa=="Overig zonder inkomen", 0, SPOLIS_paidhours_fa),
    SPOLIS_contract_fa = ifelse(is.na(SPOLIS_contract_fa) & SECM_fa=="Ontvanger werkloosheidsuitkering" | 
                        SECM_fa=="Ontvanger bijstandsuitkering" | SECM_fa=="Ontvanger uitkering sociale voorz.overig" | 
                        SECM_fa=="Ontvanger uitkering ziekte/AO/pension" | 
                        SECM_fa=="Nog niet schoolg./schol./stud. met of zonder ink" | 
                        SECM_fa=="Overig zonder inkomen", 4, SPOLIS_contract_fa), 
    SPOLIS_contract_fa = as_factor(SPOLIS_contract_fa),
    SPOLIS_contract_fa = recode(SPOLIS_contract_fa, '1' = '1', 
                        '2' = '2', 
                        '3' = '3', 
                        '4' = 'niet werkend volgens SECM'),
    N_vroeg_24_37 = recode(N_vroeg_24_37, 
                         '2' = '1+',
                         '1' = '1+'),
    N_vooraf_sga = recode(N_vooraf_sga, '1' = '1+',
                         '2' = '1+', 
                         '3' = '1+', 
                         '4' = '1+', 
                         '5' = '1+'), 
    STED_mo = recode(STED_mo, '1' = '1 or 2', 
                         '2' = '1 or 2')
  )%>%
  replace_with_na_at(.vars = c("N_vroeg_24_37", "N_vooraf_sga", "vooraf_zw_vroeg_24_37", "vooraf_sga"), condition = ~.x == "geen data")%>%
  mutate(N_vroeg_24_37 =  droplevels(N_vroeg_24_37),
         N_vooraf_sga = droplevels(N_vooraf_sga), 
         vooraf_zw_vroeg_24_37 = droplevels(vooraf_zw_vroeg_24_37),
         vooraf_sga = droplevels(vooraf_sga))%>%
  mutate(N_vroeg_24_37 = factor(N_vroeg_24_37, ordered = FALSE))%>%
  select(-jaar, -LAND_ACHTS_gebl1, -LAND_ACHTS_gebl2, -LAND_ETNG_gebl1, -LAND_ETNG_gebl2, -ZVWK_birth_obstetrician_fa, - ZVWK_birth_maternitycare_fa) #birth costs father are 0, excluded


#### 1.2 Explore missingness patterns ####
miss_var_smr<-miss_var_summary(predictors)
miss_case_smr<-miss_case_summary(predictors)
cases_low_miss<-miss_case_smr%>%filter(pct_miss<50)%>%pull(case)
#exclude  cases wtih >50% missings
predictors<-predictors[cases_low_miss,] # excluded cases with >50% missings



#### 1.3 Create train and test datasets ####
# In ML, when imputing missing observations, a train/test split should be made before imputation
# In training the prediction model, the algorithm should not be able to learn from the test data. Test data remains separate to evaluate predictive performance of the trained model.

# set seed for reproducibility
set.seed(8151)

# assign split value per row, with probability of 0.7 for train and 0.3 for test
split <- sample(c(TRUE, FALSE), nrow(predictors), replace = TRUE,
                prob = c(0.7, 0.3))
# create a train and a test dataset
sdq_16w_train <- predictors[split, ]
sdq_16w_test <- predictors[!split, ]


#### 1.4 Look at the matrix of predictors ####
#Initialize prediction matrix that selects as predictors all variables that have at least 0.2 correlation with the target variable
predmatrix = quickpred(sdq_16w_train, mincor = 0.2)

#Manually edit the prediction matrix, to specify which variables are used to predict the others (the choices are made in iterative process using the diagnostic tools of MICE)
predmatrix[c("SECM_employee_mo","SECM_director_mo", "SECM_selfemployed_mo", "SECM_otherwork_mo", "SECM_unemployed_mo", "SECM_socialassistance_mo",
             "SECM_otherassistance_mo", "SECM_disability_mo", "SECM_retirement_mo", "SECM_student_mo", "SECM_familywork_mo",
             "SECM_employee_fa", "SECM_director_fa", "SECM_selfemployed_fa", "SECM_otherwork_fa", "SECM_unemployed_fa", "SECM_socialassistance_fa",
             "SECM_otherassistance_fa", "SECM_disability_fa", "SECM_retirement_fa", "SECM_student_fa", "SECM_familywork_fa", "hh_married_mo", 
             "hh_parents_different", "hh_single_parent_mo"), c("hh_single_parent_mo", "TYPHH_mo")]<-0

predmatrix["SPOLIS_wages_fa",  "SECM_fa"]<-0
predmatrix["SPOLIS_paidhours_fa",  "SECM_fa"]<-0
predmatrix[, "TYPHH_mo"]<-0
predmatrix[, c("vooraf_zw_vroeg_24_37", "vooraf_sga", "N_vroeg_24_37", "N_vooraf_sga")]<-0
predmatrix[c("LAND_ETNG_fa", "LAND_ETNG_mo", "LAND_ACHTS_fa", "LAND_ACHTS_mo"), c("hh_single_parent_mo", "GBA_generation_fa", "GBA_generation_mo")]<-0
predmatrix[,c("LAND_ETNG_fa", "LAND_ETNG_mo", "LAND_ACHTS_fa", "LAND_ACHTS_mo")]<-0

predmatrix[c("SPOLIS_paidhours_fa", "SPOLIS_paidhours_mo", "SPOLIS_wages_fa", "SPOLIS_wages_mo"), ]<-0 #first set all predictors to 0 

predmatrix["SPOLIS_paidhours_fa", c("SPOLIS_paidhours_mo", "SPOLIS_wages_fa", "SPOLIS_wages_mo", "educc_mo", "educc_fa", "ED_rentown_mo", "ED_rentown_fa", "SECM_mo", 
                                    "SECM_fa", "income_mo", "income_fa")]<-1
predmatrix["SPOLIS_paidhours_mo", c("SPOLIS_paidhours_fa", "SPOLIS_wages_fa", "SPOLIS_wages_mo", 
                                    "educc_mo", "educc_fa", "ED_rentown_mo", "ED_rentown_fa", "SECM_mo", "SECM_fa", "income_mo", "income_fa")]<-1
predmatrix["SPOLIS_wages_fa", c("SPOLIS_paidhours_fa", "SPOLIS_paidhours_mo", "SPOLIS_wages_mo", "educc_mo", "educc_fa", "ED_rentown_mo", "ED_rentown_fa", "SECM_mo", 
                                "SECM_fa", "income_mo", "income_fa")]<-1
predmatrix["SPOLIS_wages_mo", c("SPOLIS_paidhours_fa", "SPOLIS_paidhours_mo", "SPOLIS_wages_fa","educc_mo", "educc_fa", "ED_rentown_mo", "ED_rentown_fa", "SECM_mo", 
                                "SECM_fa", "income_mo", "income_fa")]<-1

predmatrix[c("ZVWK_mentalhealth_bas_fa", "ZVWK_mentalhealth_spec_fa", "ZVWK_mentalh_spec_nostay_inst_fa", "ZVWK_mentalh_spec_nostay_ind_fa", 
             "ZVWK_mentalh_spec_other_fa", "ZVWK_mentalhealth_bas_mo", "ZVWK_mentalhealth_spec_mo", 
             "ZVWK_mentalh_spec_nostay_inst_mo", "ZVWK_mentalh_spec_nostay_ind_mo", "ZVWK_mentalh_spec_other_mo"), ]<-0 #first set all predictors to 0 
predmatrix["ZVWK_mentalhealth_bas_fa", c("ZVWK_sumtotal_fa", "ZVWK_mentalhealth_spec_fa", "ZVWK_mentalhealth_spec_stay_fa", "ZVWK_mentalh_spec_nostay_inst_fa", 
                                         "ZVWK_mentalh_spec_nostay_ind_fa", "ZVWK_mentalh_spec_other_fa")]<-1              

predmatrix["ZVWK_mentalhealth_spec_fa", c("ZVWK_sumtotal_fa", "ZVWK_mentalhealth_bas_fa", "ZVWK_mentalhealth_spec_stay_fa", "ZVWK_mentalh_spec_nostay_inst_fa", 
                                          "ZVWK_mentalh_spec_nostay_ind_fa", "ZVWK_mentalh_spec_other_fa")]<-1 

predmatrix["ZVWK_mentalh_spec_nostay_inst_fa", c("ZVWK_sumtotal_fa", "ZVWK_mentalhealth_spec_fa",  "ZVWK_mentalhealth_spec_stay_fa", "ZVWK_mentalhealth_bas_fa", 
                                                 "ZVWK_mentalh_spec_nostay_ind_fa", "ZVWK_mentalh_spec_other_fa")]<-1 

predmatrix["ZVWK_mentalh_spec_nostay_ind_fa", c("ZVWK_sumtotal_fa", "ZVWK_mentalhealth_spec_fa", 
                                                "ZVWK_mentalhealth_spec_stay_fa", "ZVWK_mentalh_spec_nostay_inst_fa", 
                                                "ZVWK_mentalhealth_bas_fa", "ZVWK_mentalh_spec_other_fa")]<-1 

predmatrix["ZVWK_mentalh_spec_other_fa", c("ZVWK_sumtotal_fa", "ZVWK_mentalhealth_spec_fa", 
                                           "ZVWK_mentalhealth_spec_stay_fa", "ZVWK_mentalh_spec_nostay_inst_fa", 
                                           "ZVWK_mentalh_spec_nostay_ind_fa", "ZVWK_mentalhealth_bas_fa")]<-1 


predmatrix["ZVWK_mentalhealth_bas_mo", c("ZVWK_sumtotal_mo", "ZVWK_mentalhealth_spec_mo", 
                                         "ZVWK_mentalhealth_spec_stay_mo", "ZVWK_mentalh_spec_nostay_inst_mo", 
                                         "ZVWK_mentalh_spec_nostay_ind_mo", "ZVWK_mentalh_spec_other_mo")]<-1              

predmatrix["ZVWK_mentalhealth_spec_mo", c("ZVWK_sumtotal_mo", "ZVWK_mentalhealth_bas_mo", 
                                          "ZVWK_mentalhealth_spec_stay_mo", "ZVWK_mentalh_spec_nostay_inst_mo", 
                                          "ZVWK_mentalh_spec_nostay_ind_mo", "ZVWK_mentalh_spec_other_mo")]<-1 

predmatrix["ZVWK_mentalh_spec_nostay_inst_mo", c("ZVWK_sumtotal_mo", "ZVWK_mentalhealth_spec_mo", 
                                                 "ZVWK_mentalhealth_spec_stay_mo", "ZVWK_mentalhealth_bas_mo", 
                                                 "ZVWK_mentalh_spec_nostay_ind_mo", "ZVWK_mentalh_spec_other_mo")]<-1 

predmatrix["ZVWK_mentalh_spec_nostay_ind_mo", c("ZVWK_sumtotal_mo", "ZVWK_mentalhealth_spec_mo", 
                                                "ZVWK_mentalhealth_spec_stay_mo", "ZVWK_mentalh_spec_nostay_inst_mo", 
                                                "ZVWK_mentalhealth_bas_mo", "ZVWK_mentalh_spec_other_mo")]<-1 

predmatrix["ZVWK_mentalh_spec_other_mo", c("ZVWK_sumtotal_mo", "ZVWK_mentalhealth_spec_mo", 
                                           "ZVWK_mentalhealth_spec_stay_mo", "ZVWK_mentalh_spec_nostay_inst_mo", 
                                           "ZVWK_mentalh_spec_nostay_ind_mo", "ZVWK_mentalhealth_bas_mo")]<-1 



#### 1.5 Multiple imputation with MICE ####
### Train data
mi <- mice(sdq_16w_train, m=5, pred = predmatrix, seed = 815, maxit=100, defaultMethod = c("cart", "cart", "cart", "cart"),
           print = F) 

#Check logged events 
logged<-data.frame(mi$loggedEvents)

#check convergence
plot(mi, layout=c(4,4))

#for continious var look at density plots
densityplot(mi, layout=c(5,5))

#Strip plots to see whether imputed variables are in plausible range
a<-stripplot(mi, educc_mo~.imp, pch=c(21,20), cex=c(1,1.5))
b<-stripplot(mi, SPOLIS_wages_mo~.imp, pch=c(21,20), cex=c(1,1.5))
c<-stripplot(mi, lft_concept_mo~.imp, pch=c(21,20), cex=c(1,1.5))
d<-stripplot(mi, educc_fa~.imp, pch=c(21,20), cex=c(1,1.5))
e<-stripplot(mi, N_vooraf_sga~.imp, pch=c(21,20), cex=c(1,1.5))
f<-stripplot(mi, ZVWK_mentalhealth_bas_fa~.imp, pch=c(21,20), cex=c(1,1.5))
g<-stripplot(mi, ZVWK_patient_transport_sit_mo~.imp, pch=c(21,20), cex=c(1,1.5))
e<-stripplot(mi, interpreg_cat~.imp, pch=c(21,20), cex=c(1,1.5))
h<-stripplot(mi, N_vroeg_24_37~.imp, pch=c(21,20), cex=c(1,1.5))
i<-stripplot(mi, vooraf_sga~.imp, pch=c(21,20), cex=c(1,1.5))

ggarrange(a,b,c,d,e,f,g,h,i, ncol=3, nrow=3)

a<-stripplot(mi, LAND_ETNG_mo~.imp, pch=c(21,20), cex=c(1,1.5))
b<-stripplot(mi, LAND_ACHTS_mo~.imp, pch=c(21,20), cex=c(1,1.5))
c<-stripplot(mi, GBA_generation_mo~.imp, pch=c(21,20), cex=c(1,1.5))
d<-stripplot(mi, SECM_mo~.imp, pch=c(21,20), cex=c(1,1.5))
e<-stripplot(mi, TYPHH_mo~.imp, pch=c(21,20), cex=c(1,1.5))
f<-stripplot(mi, PLHH_mo~.imp, pch=c(21,20), cex=c(1,1.5))
ggarrange(a,b,c,d,e,f, ncol=2, nrow=3)


#LOok at summary of imputed variables 
sapply(Filter(function(x) nrow(x) >0, mi$imp), 
       function(x) summary(unlist(x)))

#same mi object 
save(mi, file="H:/Mirthe/case_jgz/MI_datasets_and_mice_scripts/sdq_16w_MI.rda")

#loading for future use
load("H:/Mirthe/case_jgz/MI_datasets_and_mice_scripts/sdq_16w_MI.rda")


# MI is a mids object: get the complete data set by replacing missing values by imputations 
mi_complete <- mi %>% 
  mice::complete("all")

# set ordered factors to factors
func_convert_ordered <- function(mi_list) {
  mi_list %>% 
    mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))
}

# look at the education level data, if there are too little observations in 1_geen
# collapse categories 1_geen and 2_basis
table(mi_complete$`1`$educc_mo)
table(mi_complete$`1`$educc_fa)
func_educc_collapse <- function(mi_list){
  mi_list %>% 
    mutate(educc_fa = fct_collapse(educc_fa, "1_2basis" = c("1_geen", "2_basis")),
           educc_mo = fct_collapse(educc_mo, "1_2basis" = c("1_geen", "2_basis")))
}

# apply the function to the m = 5 data sets and check to see the result
mi_complete <- lapply(mi_complete, func_convert_ordered)
mi_complete <- lapply(mi_complete, func_educc_collapse)

saveRDS(mi_complete, "case_jgz/repo/socialemotional_development/16w/data/socialemotional_16w_MI_complete.rda")

### Test data
mi_test <- mice.mids(mi, maxit = 1, newdata = sdq_16w_test, seed = 8151)
mi_test_comp <- mi_test %>% 
  mice::complete("all")
mi_test_comp <- lapply(mi_test_comp, func_convert_ordered)
mi_test_comp <- lapply(mi_test_comp, func_educc_collapse)

saveRDS(mi_test_comp, "case_jgz/repo/socialemotional_development/16w/data/socialemotional_16w_MI_test_comp.rda")

# now that we have used MICE for multiple imputation to deal with the missing data
# we can move to the analysis: 06_jgz_prediction_socialemotional_16w.R

