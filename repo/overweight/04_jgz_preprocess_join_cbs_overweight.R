#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 04_jgz_preprocess_join_cbs.R ####
# THE GOAL OF THIS SCRIPT IS TO ADD THE CBS DATA TO THE JGZ DATA 

# load the necessary libraries 
library(tidyverse)
library(haven)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(caret) # for confusionMatrix
library(ranger)
library(readxl)
library(xlsx)
library(pROC)
library(tuneRanger)
library(mlr)
setwd("H:/Mirthe/")


#### THE CASE: OVERWEIGHT (NL: OVERGEWICHT) ####


#### 1. Function to join the JGZ and CBS data #### 
# make a dataset to identify children and their parents, which also records birthdate and doe-moment of the child
join_jgz <- function(jgz_data_outcome){
  jgz_data_outcome %>% 
    select(c(RINPERSOONS, RINPERSOON, rins_mo, rin_mo, rins_fa, rin_fa, 
             birthdate, birthday_kid, doe_moment_ind)) %>% 
    unique()
}
# use this dataset to join datasets on various subjects and to select observations based on the doe-moment of the child

# function to join a CBS dataset with join_jgz, to link data from the parents to child 
# for the mother 
join_jgz_mo <- function(join_data, subject_data) {
  join_data %>% 
    left_join(y = subject_data,
              by = c("rins_mo" = "rins", "rin_mo" = "rin"),
              relationship = "many-to-many") %>% 
    rename_at(vars(-c(RINPERSOONS, RINPERSOON, rins_mo, rin_mo, rins_fa, rin_fa, 
                      birthdate, birthday_kid, doe_moment_ind)), 
              function(x) paste0(x, "_mo"))
}

# for the father
join_jgz_fa <- function(join_data, subject_data) {
  join_data %>% 
    left_join(y = subject_data,
              by = c("rins_fa" = "rins", "rin_fa" = "rin"), 
              relationship = "many-to-many") %>% 
    rename_at(vars(-c(RINPERSOONS, RINPERSOON, rins_mo, rin_mo, rins_fa, rin_fa, 
                      birthdate, birthday_kid, doe_moment_ind)),  
              function(x) paste0(x, "_fa"))
}

#### 2. Filter the CBS data at the doe-moment 4 months ####

# read the case-specific data created in 03_jgz_preprocess_filter
jr3maand9_BMI <- readRDS("case_jgz/data_outcome/overweight/jr3maand9_BMI_p_711.rds")

# specify the doe-moment
doe_moment_ind <- "doe_moment_4m"


join_data <- join_jgz(jr3maand9_BMI)

# function for the selection with doe-moment 4 months 
selection_4m_mo <- function(dat) {
  dat %>% 
    filter(doe_moment_4m %within% interval(start_date_mo, end_date_mo))
}

selection_4m_fa <- function(dat) {
  dat %>% 
    filter(doe_moment_4m %within% interval(start_date_fa, end_date_fa))
}

selection_4m <- function(selection_4m_mo, selection_4m_fa) {
  full_join(selection_4m_mo, selection_4m_fa,
            by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", 
                   "rins_mo" = "rins_mo", "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", 
                   "birthdate" = "birthdate", "birthday_kid" = "birthday_kid", "doe_moment_4m" = "doe_moment_4m"))
}


# CBS microdata files:
### 2.1 residence data
residence_dat <- readRDS("data/parents_jgz/residence_dat_jgz.rds")
residence_dat_kids <- readRDS("data/parents_jgz/residence_dat_kids_jgz.rds")

join_residence_mo <- join_jgz_mo(join_data = join_data, subject_data = residence_dat)
join_residence_fa <- join_jgz_fa(join_data, residence_dat)
join_residence_kids <- join_data %>% left_join(residence_dat_kids, by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"))

selection_4m_residence_mo <- selection_4m_mo(join_residence_mo)
selection_4m_residence_fa <- selection_4m_fa(join_residence_fa)
selection_4m_residence_kids <- join_residence_kids %>% 
  filter(doe_moment_4m %within% interval(start_date, end_date))

selection_4m_residence_kids <- selection_4m_residence_kids %>% 
  left_join(readRDS("data/eigendomwoz_dat.rds"), 
            by = c("residence_type" = "SOORTOBJECTNUMMER", "residence" = "RINOBJECTNUMMER")) %>% 
  filter(year(doe_moment_4m) == year) %>% 
  select(-c(year))

overweight_residence_dat <- selection_4m(selection_4m_residence_mo, selection_4m_residence_fa)
#saveRDS(overweight_residence_dat, "case_jgz/selection/overweight/overweight_residence_dat.rds")

overweight_residence_woz_dat <- full_join(overweight_residence_dat, selection_4m_residence_kids,
                                          by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", 
                                                 "rins_mo" = "rins_mo", "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", 
                                                 "birthdate" = "birthdate", "birthday_kid" = "birthday_kid", "doe_moment_4m" = "doe_moment_4m"))

saveRDS(overweight_residence_woz_dat, "case_jgz/selection/overweight/overweight_residence_woz_dat.rds")
rm(residence_dat, residence_dat_kids, overweight_residence_dat)


### 2.2 education level data
educ_dat <- readRDS("data/parents_jgz/educ_dat_ref_jgz.rds")

join_educ_mo <- join_jgz_mo(join_data, educ_dat)
join_educ_fa <- join_jgz_fa(join_data, educ_dat)

selection_4m_educ_mo <- selection_4m_mo(join_educ_mo)
selection_4m_educ_fa <- selection_4m_fa(join_educ_fa)

overweight_educ_dat <- selection_4m(selection_4m_educ_mo, selection_4m_educ_fa)

saveRDS(overweight_educ_dat, "case_jgz/selection/overweight/overweight_educ_dat.rds")
rm(educ_dat)


### 2.3 socio-economic data
secm_dat <- readRDS("data/parents_jgz/secm_dat_NA0_jgz.rds")
# load the secm dataset with NA set to 0, for the secm categories NA (as opposed to 1) implies that the category does not apply

join_secm_mo <- join_jgz_mo(join_data, secm_dat)
join_secm_fa <- join_jgz_fa(join_data, secm_dat)

selection_4m_secm_mo <- selection_4m_mo(join_secm_mo)
selection_4m_secm_fa <- selection_4m_fa(join_secm_fa)

overweight_secm_dat <- selection_4m(selection_4m_secm_mo, selection_4m_secm_fa)

saveRDS(overweight_secm_dat, "case_jgz/selection/overweight/overweight_secm_dat.rds")
rm(secm_dat)


### 2.4 (S)POLIS DATA
polis_dat <- readRDS("data/parents_jgz/polis_dat_jgz.rds")

join_polis_mo <- join_jgz_mo(join_data, polis_dat)
join_polis_fa <- join_jgz_fa(join_data, polis_dat)

selection_4m_polis_mo <- selection_4m_mo(join_polis_mo)
selection_4m_polis_mo <- selection_4m_polis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_4m_polis_fa <- selection_4m_fa(join_polis_fa)
selection_4m_polis_fa <- selection_4m_polis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

overweight_polis_dat <- selection_4m(selection_4m_polis_mo, selection_4m_polis_fa)
#saveRDS(overweight_polis_dat, "case_jgz/selection/overweight_polis_dat.rds")

spolis_dat <- readRDS("data/parents_jgz/spolis_dat_jgz.rds")

join_spolis_mo <- join_jgz_mo(join_data, spolis_dat)
join_spolis_fa <- join_jgz_fa(join_data, spolis_dat)

selection_4m_spolis_mo <- selection_4m_mo(join_spolis_mo)
selection_4m_spolis_mo <- selection_4m_spolis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_4m_spolis_fa <- selection_4m_fa(join_spolis_fa)
selection_4m_spolis_fa <- selection_4m_spolis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

overweight_spolis_dat <- selection_4m(selection_4m_spolis_mo, selection_4m_spolis_fa)
#saveRDS(overweight_spolis_dat, "case_jgz/selection/overweight_spolis_dat.rds")

overweight_spolis_complete <- rbind(overweight_polis_dat, overweight_spolis_dat)
saveRDS(overweight_spolis_complete, "case_jgz/selection/overweight/overweight_spolis_complete.rds")
rm(polis_dat, spolis_dat, overweight_polis_dat, overweight_spolis_dat)


### 2.5 income data
income_dat <- readRDS("data/parents_jgz/income_dat_jgz.rds")

join_income_mo <- join_jgz_mo(join_data, income_dat)
join_income_fa <- join_jgz_fa(join_data, income_dat)

selection_4m_income_mo <- selection_4m_mo(join_income_mo)
selection_4m_income_fa <- selection_4m_fa(join_income_fa)

overweight_income_dat <- selection_4m(selection_4m_income_mo, selection_4m_income_fa)

saveRDS(overweight_income_dat, "case_jgz/selection/overweight/overweight_income_dat.rds")
rm(income_dat)

### 2.6 health data
health_dat <- readRDS("data/parents_jgz/health_dat_jgz.rds")

join_health_mo <- join_jgz_mo(join_data, health_dat)
join_health_fa <- join_jgz_fa(join_data, health_dat)

# select health care costs for t - 1, i.e. the calender year before doe-moment 4 maanden
selection_4m_health_mo <- join_health_mo %>% 
  filter((doe_moment_4m - years(1)) %within% interval(start_date_mo, end_date_mo))
selection_4m_health_fa <- join_health_fa %>% 
  filter((doe_moment_4m - years(1)) %within% interval(start_date_fa, end_date_fa))

overweight_health_dat <- selection_4m(selection_4m_health_mo, selection_4m_health_fa)

saveRDS(overweight_health_dat, "case_jgz/selection/overweight/overweight_health_dat.rds")
rm(health_dat)


### 2.7 Household type data
gba_hh <- readRDS("data/parents_jgz/gba_hh_dat_jgz.rds")

join_gba_hh_dat <- join_data %>% 
  left_join(gba_hh, by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON"))

overweight_gba_hh_dat <- join_gba_hh_dat %>% 
  filter(doe_moment_4m %within% interval(DATUMAANVANGHH, DATUMEINDEHH))

saveRDS(overweight_gba_hh_dat, "case_jgz/selection/overweight/overweight_gba_hh_dat.rds")
rm(gba_hh)

### 2.8 Household income
hh_income_dat_jgz <- readRDS("H:/Mirthe/data/parents_jgz/hh_income_dat_jgz.rds") %>% 
  mutate(l_income_hh_pov_binary  = as.factor(l_income_hh_pov_binary),
         l_income_hh_pov_4j_binary = as.factor(l_income_hh_pov_4j_binary),
         l_income_hh_min_binary = as.factor(l_income_hh_min_binary),
         l_income_hh_min_4j_binary = as.factor(l_income_hh_min_4j_binary),
         l_income_hh_eur_binary = as.factor(l_income_hh_eur_binary),
         l_income_hh_eur_4j_binary = as.factor(l_income_hh_eur_4j_binary))

join_hh_income_kid <- join_data %>% 
  left_join(hh_income_dat_jgz, by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"))

overweight_hh_income_dat <- join_hh_income_kid %>% 
  filter(year(doe_moment_4m) == year)

# or to get more complete records
join_hh_income_mo <- join_jgz_mo(join_data, hh_income_dat_jgz)

overweight_hh_income_mo <- join_hh_income_mo %>% 
  filter(year(doe_moment_4m) == year_mo)

saveRDS(overweight_hh_income_dat, "case_jgz/selection/overweight/overweight_hh_income_dat.rds")
saveRDS(overweight_hh_income_mo, "case_jgz/selection/overweight/overweight_hh_income_mo.rds")
rm(hh_income_dat_jgz)


### 2.9 Household vermogen
hh_schulden_jgz <- readRDS("H:/Mirthe/data/parents_jgz/hh_schulden_jgz.rds")

join_hh_schulden_kid <- join_data %>% 
  left_join(hh_schulden_jgz, by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"))

overweight_hh_schulden_dat <- join_hh_schulden_kid %>% 
  filter(year(doe_moment_4m) == year)

# or to get more complete records
join_hh_schulden_mo <- join_jgz_mo(join_data, hh_schulden_jgz)

overweight_hh_schulden_mo <- join_hh_schulden_mo %>% 
  filter(year(doe_moment_4m) == year_mo)

saveRDS(overweight_hh_schulden_dat, "case_jgz/selection/overweight/overweight_hh_schulden_dat.rds")
saveRDS(overweight_hh_schulden_mo, "case_jgz/selection/overweight/overweight_hh_schulden_mo.rds")
rm(hh_schulden_jgz)

# remove the files that are no longer necessary 
rm(join_residence_mo, join_residence_fa, join_residence_kids, join_educ_mo, join_educ_fa,
   join_secm_mo, join_secm_fa, join_polis_mo, join_polis_fa, join_gba_hh_dat,
   join_spolis_mo, join_spolis_fa, join_income_mo, join_income_fa, join_health_mo, join_health_fa,
   join_hh_income_kid, join_hh_schulden_kid, join_hh_income_mo, join_hh_schulden_mo)

rm(selection_4m_residence_mo, selection_4m_residence_fa, selection_4m_residence_kids, selection_4m_educ_mo, selection_4m_educ_fa,
   selection_4m_secm_mo, selection_4m_secm_fa, selection_4m_polis_mo, selection_4m_polis_fa,
   selection_4m_spolis_mo, selection_4m_spolis_fa, selection_4m_income_mo, selection_4m_income_fa, 
   selection_4m_health_mo, selection_4m_health_fa, overweight_polis_dat, overweight_spolis_dat)


#### 3. Join the JGZ data and the CBS data at the doe-moment 2 years #### 
# read data generated above if no longer in environment
overweight_residence_woz_dat<- readRDS("case_jgz/selection/overweight/overweight_residence_woz_dat.rds")
#overweight_residence_dat <- readRDS("case_jgz/selection/overweight/overweight_residence_dat.rds") # version without WOZ
overweight_educ_dat <- readRDS("case_jgz/selection/overweight/overweight_educ_dat.rds")
overweight_secm_dat <- readRDS("case_jgz/selection/overweight/overweight_secm_dat.rds")
overweight_spolis_complete <- readRDS("case_jgz/selection/overweight/overweight_spolis_complete.rds")
overweight_income_dat <- readRDS("case_jgz/selection/overweight/overweight_income_dat.rds")
overweight_health_dat <- readRDS("case_jgz/selection/overweight/overweight_health_dat.rds")
overweight_gba_hh_dat <- readRDS("case_jgz/selection/overweight/overweight_gba_hh_dat.rds")
overweight_hh_income_mo <- readRDS("case_jgz/selection/overweight/overweight_hh_income_mo.rds")
overweight_hh_schulden_mo <- readRDS("case_jgz/selection/overweight/overweight_hh_schulden_mo.rds")


overweight_jgz_dat_4m <- jr3maand9_BMI  #readRDS("case_jgz/data_outcome/overweight/jr3maand9_BMI_p.rds")
join_overweight_jgz_dat_4m <- function(overweight_dat) {
  overweight_jgz_dat_4m %>% 
    left_join(y = overweight_dat,
              by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", "rins_mo" = "rins_mo", 
                     "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", "birthdate" = "birthdate",
                     "birthday_kid" = "birthday_kid", "doe_moment_4m" = "doe_moment_4m"))
} 
# use the function and select the dataset you want to join to the parent prnl dataset 
overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_residence_woz_dat) %>% 
  rename(start_date_residence_mo = start_date_mo,
         end_date_residence_mo = end_date_mo,
         start_date_residence_fa = start_date_fa,
         end_date_residence_fa = end_date_fa,
         start_date_residence_kid = start_date,
         end_date_residence_kid = end_date)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_educ_dat) %>% 
  rename(start_date_educ_mo = start_date_mo,
         end_date_educ_mo = end_date_mo,
         start_date_educ_fa = start_date_fa,
         end_date_educ_fa = end_date_fa)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_secm_dat) %>% 
  rename(start_date_secm_mo = start_date_mo,
         end_date_secm_mo = end_date_mo,
         start_date_secm_fa = start_date_fa,
         end_date_secm_fa = end_date_fa)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_spolis_complete) %>% 
  rename(start_date_spolis_mo = start_date_mo,
         end_date_spolis_mo = end_date_mo,
         start_date_spolis_fa = start_date_fa,
         end_date_spolis_fa = end_date_fa)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_income_dat) %>% 
  rename(start_date_income_mo = start_date_mo,
         end_date_income_mo = end_date_mo,
         start_date_income_fa = start_date_fa,
         end_date_income_fa = end_date_fa)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_health_dat) %>% 
  rename(start_date_health_mo = start_date_mo,
         end_date_health_mo = end_date_mo,
         start_date_health_fa = start_date_fa,
         end_date_health_fa = end_date_fa)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_gba_hh_dat) %>% 
  rename(start_date_hh_kid = DATUMAANVANGHH,
         end_date_hh_kid = DATUMEINDEHH)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_hh_income_mo) %>% 
  rename(year_hh_income_mo = year_mo, 
         rins_hh_i = rins_hh_mo, 
         rin_hh_i = rin_hh_mo)

overweight_jgz_dat_4m <- join_overweight_jgz_dat_4m(overweight_hh_schulden_mo) %>% 
  rename(year_hh_schulden_mo = year_mo, 
         rins_hh_s = rins_hh_mo,
         rin_hh_s = rin_hh_mo)

# check the percentage of missing values per column 
round(colSums(is.na(overweight_jgz_dat_4m)/nrow(overweight_jgz_dat_4m)*100), 1)

# remove the separate data files that are no longer needed 
rm(overweight_residence_woz_dat, overweight_educ_dat, overweight_secm_dat, 
   overweight_spolis_complete, overweight_income_dat, overweight_health_dat,
   overweight_hh_income_dat, overweight_hh_schulden_dat, overweight_gba_hh_dat,
   overweight_hh_schulden_mo, overweight_hh_income_mo)


# save the complete dataset for the case overweight
saveRDS(overweight_jgz_dat_4m, "case_jgz/data_outcome/overweight/overweight_jgz_dat_4m_706_p.rds")

overweight_jgz_dat_4m <- readRDS("case_jgz/data_outcome/overweight/overweight_jgz_dat_4m_706_p.rds")


#### 4. Transform the CBS and JGZ data for analysis ####

### 4.1 Birth records 
# create a factor variable for prnl gestational age in weeks 
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(PRNL_gestational_age_weekf = factor(PRNL_gestational_age_week, ordered = TRUE),
         PRNL_parity = ordered(PRNL_parity)) 

# create a factor variable for prnl birthweight ventiles 
quantile(overweight_jgz_dat_4m$PRNL_birthweight, probs = seq(0, 1, 1/20), na.rm = TRUE)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(PRNL_birthweight_5 = ntile(PRNL_birthweight, 20),
         PRNL_birthweight_5 = factor(PRNL_birthweight_5, ordered = TRUE)) 

# do the same for the new perined geboortegew data, we will use only one of these variables
quantile(overweight_jgz_dat_4m$geboortegew, probs = seq(0, 1, 1/20), na.rm = TRUE)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(geboortegew_5 = ntile(geboortegew, 20),
         geboortegew_5 = factor(geboortegew_5, ordered = TRUE)) 

# there are only 2 children in the N = 3 group, so we change 2 to 2-2+
# otherwise this will give issues in the train/test split and in prediction
table(overweight_jgz_dat_4m$N_vroeg_24_37)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(N_vroeg_24_37 = if_else(N_vroeg_24_37 == '2', '2-2+', 
                                 if_else(N_vroeg_24_37 == '3', '2-2+', N_vroeg_24_37)),
         N_vroeg_24_37 = ordered(N_vroeg_24_37, levels = c("nvt", "0", "1", "2-2+", "geen data")),
         
         vroeg_geb_24_28 = as_factor(vroeg_geb_24_28),
         vroeg_geb_28_34 = as_factor(vroeg_geb_28_34),
         vroeg_geb_34_37 = as_factor(vroeg_geb_34_37),
         jaar = as_factor(jaar))

### 4.2 residence data
# dummies to identify if the mother and father are registered at/ live(d) at the same residence
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(residence_same_for_parents = ifelse(residence_mo == residence_fa, 1, 0),
         residence_same_for_parents = as.factor(recode(residence_same_for_parents, 
                                                       "1" = "parents same residence", "0" = "parents not same residence")))

# !!! CHECK MISTAKE IN THE HOUSEHOLD DATA, also check hh_single_parent
table(overweight_jgz_dat_4m$TYPHH)
table(overweight_jgz_dat_4m$hh_married)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(hh_married = ifelse(TYPHH == "Gehuwd paar zonder kinderen" | TYPHH == "Gehuwd paar met kinderen", 1, 0),
         hh_married = as_factor(hh_married))

### 4.3 education data 
# combine the education data from JGZ and CBS to create the most complete variable for education of the parents
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  # group the education level data from CBS to create an education variable with 8 levels
  mutate(educ_cbs_mo = as.numeric(ifelse(educationlevel_mo %in% c(1110, 1111, 1112), '2',
                                         ifelse(educationlevel_mo %in% c(1210, 1211, 1212, 1213), '3',
                                                ifelse(educationlevel_mo %in% c(1220, 1221, 1222), '4',
                                                       ifelse(educationlevel_mo %in% c(2110, 2111, 2112, 2120, 2121), '5',
                                                              ifelse(educationlevel_mo %in% c(2130, 2131, 2132), '6',
                                                                     ifelse(educationlevel_mo %in% c(3110, 3111, 3112), '7',
                                                                            ifelse(educationlevel_mo %in% c(3113, 3210, 3211, 3212, 3213), '8', NA))))))))) %>% 
  mutate(educ_cbs_fa = as.numeric(ifelse(educationlevel_fa %in% c(1110, 1111, 1112), '2',
                                         ifelse(educationlevel_fa %in% c(1210, 1211, 1212, 1213), '3',
                                                ifelse(educationlevel_fa %in% c(1220, 1221, 1222), '4',
                                                       ifelse(educationlevel_fa %in% c(2110, 2111, 2112, 2120, 2121), '5',
                                                              ifelse(educationlevel_fa %in% c(2130, 2131, 2132), '6',
                                                                     ifelse(educationlevel_fa %in% c(3110, 3111, 3112), '7',
                                                                            ifelse(educationlevel_fa %in% c(3113, 3210, 3211, 3212, 3213), '8', NA))))))))) %>%
  # group the education level data from JGZ to create an education variable with 8 levels
  mutate(educ_jgz_mo = as.numeric(ifelse(opl1 == '1_geen', '1',
                                         ifelse(opl1 == '2_basis', '2',
                                                ifelse(opl1 %in% c('3_praktijk', '4_LBO'), '3',
                                                       ifelse(opl1 == '5_MAVO', '4',
                                                              ifelse(opl1 == '6_MBO', '5',
                                                                     ifelse(opl1 == '7_HAVOVWO', '6',
                                                                            ifelse(opl1 == '8_HBO', '7',
                                                                                   ifelse(opl1 == '9_WO', '8', NA)))))))))) %>% 
  mutate(educ_jgz_fa = as.numeric(ifelse(opl2 == '1_geen', '1',
                                         ifelse(opl2 == '2_basis', '2',
                                                ifelse(opl2 %in% c('3_praktijk', '4_LBO'), '3',
                                                       ifelse(opl2 == '5_MAVO', '4',
                                                              ifelse(opl2 == '6_MBO', '5',
                                                                     ifelse(opl2 == '7_HAVOVWO', '6',
                                                                            ifelse(opl2 == '8_HBO', '7',
                                                                                   ifelse(opl2 == '9_WO', '8', NA))))))))))

# if both JGZ and CBS contain records on the education of the parent and the levels are not consistent, 
# use the education level according to CBS (the assumption is that the administrative data is more accurate)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(educc_mo = ifelse(is.na(educ_cbs_mo), educ_jgz_mo,
                           ifelse(is.na(educ_jgz_mo), educ_cbs_mo,
                                  ifelse(!is.na(educ_cbs_mo) & !is.na(educ_jgz_mo) & educ_cbs_mo == educ_jgz_mo, educ_cbs_mo, 
                                         ifelse(!is.na(educ_cbs_mo) & !is.na(educ_jgz_mo) & educ_cbs_mo != educ_jgz_mo, pmax(educ_cbs_mo, educ_jgz_mo, na.rm = TRUE), NA)))),
         educc_fa = ifelse(is.na(educ_cbs_fa), educ_jgz_fa,
                           ifelse(is.na(educ_jgz_fa), educ_cbs_fa,
                                  ifelse(!is.na(educ_cbs_fa) & !is.na(educ_jgz_fa) & educ_cbs_fa == educ_jgz_fa, educ_cbs_fa, 
                                         ifelse(!is.na(educ_cbs_fa) & !is.na(educ_jgz_fa) & educ_cbs_fa != educ_jgz_fa, pmax(educ_cbs_fa, educ_jgz_fa, na.rm = TRUE), NA)))),
         educc_mo = factor(ifelse(educc_mo == '1', '1_geen',
                                  ifelse(educc_mo == '2', '2_basis',
                                         ifelse(educc_mo == '3', '3_vmbopraktijk',
                                                ifelse(educc_mo == '4', '4_vmbomavo',
                                                       ifelse(educc_mo == '5', '5_mbo',
                                                              ifelse(educc_mo == '6', '6_havovwo',
                                                                     ifelse(educc_mo == '7', '7_hbo',
                                                                            ifelse(educc_mo == '8', '8_womaster', NA)))))))), ordered = TRUE),
         educc_fa = factor(ifelse(educc_fa == '1', '1_geen',
                                  ifelse(educc_fa == '2', '2_basis',
                                         ifelse(educc_fa == '3', '3_vmbopraktijk',
                                                ifelse(educc_fa == '4', '4_vmbomavo',
                                                       ifelse(educc_fa == '5', '5_mbo',
                                                              ifelse(educc_fa == '6', '6_havovwo',
                                                                     ifelse(educc_fa == '7', '7_hbo',
                                                                            ifelse(educc_fa == '8', '8_womaster', NA)))))))), ordered = TRUE)) %>%
  select(-c(educ_cbs_mo, educ_cbs_fa, educ_jgz_mo, educ_jgz_fa))


### 4.4 income data
# sum the income of the mother and father to obtain income of the parents
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  rowwise() %>% 
  mutate(income_parents = ifelse(is.na(income_mo) & is.na(income_fa), NA, sum(income_mo, income_fa, na.rm = TRUE))) %>% 
  ungroup()

### 4.5 health data
# sum the total health data costs per individual
# sum the total health data costs per individual
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(ZVWK_sumtotal_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_other_mo)), na.rm = TRUE),
         ZVWK_sumtotal_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_other_fa)), na.rm = TRUE)) 

# sum the total health data costs per individual
#overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
#  # do not include thebirth_maternitycare and birth_obstetrician costs (and for the mother hospital costs)
#  mutate(ZVWK_sumtotal_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_dentalcare_mo, ZVWK_physical_therapy_mo:ZVWK_patient_transport_lie_mo, ZVWK_abroad_mo:ZVWK_other_mo)), na.rm = TRUE),
#         ZVWK_sumtotal_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_patient_transport_lie_fa, ZVWK_abroad_fa:ZVWK_other_fa)), na.rm = TRUE)) 


### 4.6 length and weight data
sum(is.na(overweight_jgz_dat_4m$BMI_4wks))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_8wks))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_3months))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_4months))/nrow(overweight_jgz_dat_4m)*100

# the standardised scores can be used to exclude outliers from the dataset 
sum(overweight_jgz_dat_4m$lengte_3yrs9m_znl > 5, na.rm = TRUE)
sum(overweight_jgz_dat_4m$lengte_3yrs9m_znl < -5, na.rm = TRUE)
sum(overweight_jgz_dat_4m$gewicht_3yrs9m_znl > 5, na.rm = TRUE)
sum(overweight_jgz_dat_4m$gewicht_3yrs9m_znl < -5, na.rm = TRUE)
sum(overweight_jgz_dat_4m$bmi_3yrs9m_znl > 5, na.rm = TRUE)
sum(overweight_jgz_dat_4m$bmi_3yrs9m_znl < -5, na.rm = TRUE)

# remove outliers based on standardised lengte, gewicht and bmi scores (there are 15 outliers)
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>%
  filter(lengte_3yrs9m_znl >= -5 & lengte_3yrs9m_znl <= 5 | is.na(lengte_3yrs9m_znl),
         gewicht_3yrs9m_znl >= -5 & gewicht_3yrs9m_znl <= 5 | is.na(gewicht_3yrs9m_znl),
         bmi_3yrs9m_znl >= -5 & bmi_3yrs9m_znl <= 5 | is.na(bmi_3yrs9m_znl))

# change outliers in observations at earlier contactmomenten to NA 
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>% 
  mutate(lengte_4wks_znl = ifelse(lengte_4wks_znl < -5 | lengte_4wks_znl > 5, NA, lengte_4wks_znl),
         lengte_8wks_znl = ifelse(lengte_8wks_znl < -5 | lengte_8wks_znl > 5, NA, lengte_8wks_znl),
         lengte_3mnd_znl = ifelse(lengte_3mnd_znl < -5 | lengte_3mnd_znl > 5, NA, lengte_3mnd_znl),
         lengte_4mnd_znl = ifelse(lengte_4mnd_znl < -5 | lengte_4mnd_znl > 5, NA, lengte_4mnd_znl),
         
         gewicht_4wks_znl = ifelse(gewicht_4wks_znl < -5 | gewicht_4wks_znl > 5, NA, gewicht_4wks_znl),
         gewicht_8wks_znl = ifelse(gewicht_8wks_znl < -5 | gewicht_8wks_znl > 5, NA, gewicht_8wks_znl),
         gewicht_3mnd_znl = ifelse(gewicht_3mnd_znl < -5 | gewicht_3mnd_znl > 5, NA, gewicht_3mnd_znl),
         gewicht_4mnd_znl = ifelse(gewicht_4mnd_znl < -5 | gewicht_4mnd_znl > 5, NA, gewicht_4mnd_znl),
         
         bmi_4wks_znl = ifelse(bmi_4wks_znl < -5 | bmi_4wks_znl > 5, NA, bmi_4wks_znl),
         bmi_8wks_znl = ifelse(bmi_8wks_znl < -5 | bmi_8wks_znl > 5, NA, bmi_8wks_znl),
         bmi_3mnd_znl = ifelse(bmi_3mnd_znl < -5 | bmi_3mnd_znl > 5, NA, bmi_3mnd_znl),
         bmi_4mnd_znl = ifelse(bmi_4mnd_znl < -5 | bmi_4mnd_znl > 5, NA, bmi_4mnd_znl))



### 4.7 Create the outcome variable: overweight 
# look at BMI at the outcome-age 
summary(overweight_jgz_dat_4m$BMI_3yrs9m)
overweight_jgz_dat_4m %>% ggplot(aes(BMI_3yrs9m))+
  geom_histogram()+
  xlim(10,30)

# overweight is created based on the thresholds from Cole et al., 2000 and the NCJ guidelines 
overweight_jgz_dat_4m <- overweight_jgz_dat_4m %>%  
  mutate(overweight = ifelse(lft_jaren_3yrs9m < 3.75 & geslacht == "mannelijk" & BMI_3yrs9m >= 17.69, 1, 0),
         overweight = ifelse(lft_jaren_3yrs9m >= 3.75 & geslacht == "mannelijk" & BMI_3yrs9m >= 17.55, 1, overweight),
         overweight = ifelse(lft_jaren_3yrs9m < 3.75 & geslacht == "vrouwelijk" & BMI_3yrs9m >= 17.40, 1, overweight),
         overweight = ifelse(lft_jaren_3yrs9m >= 3.75 & geslacht == "vrouwelijk" & BMI_3yrs9m >= 17.28, 1, overweight)) %>% 
  mutate(overweight = as_factor(overweight))

# look at overweight prevalence 
sum(is.na(overweight_jgz_dat_4m$lft_jaren_3yrs9m))
table(overweight_jgz_dat_4m$overweight)
prop.table(table(overweight_jgz_dat_4m$overweight)) * 100
# 9.3% of the children in the cohort are overweight

# look at the distribution of the outcome variable
overweight_jgz_dat_4m %>%
  ggplot() + 
  geom_histogram(mapping = aes(x = overweight), stat = "count", fill = "black") +
  labs(title = "Histogram of overweight children")



#### 5. Prepare the data set for analysis: select the predictors ####

# look at missings in the BMI records
sum(is.na(overweight_jgz_dat_4m$BMI_3yrs9m))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_4wks))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_8wks))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_3months))/nrow(overweight_jgz_dat_4m)*100
sum(is.na(overweight_jgz_dat_4m$BMI_4months))/nrow(overweight_jgz_dat_4m)*100

# select only more complete records 
overweight_jgz_dat_4m_complete <- overweight_jgz_dat_4m %>% 
  filter(!if_all(c("BMI_4wks", "BMI_8wks", "BMI_3months", "BMI_4months"), is.na))

# FULL MODEL 
# this model includes all available predictors from JGZ, CBS and Perined
predictors_full_model <- overweight_jgz_dat_4m_complete %>%
  # select the outcome variable overweight which we created from JGZ variables lengte en gewicht
  select(c(overweight,
           # select the variables from the JGZ data
           STED, geslacht, # the variable geslacht is more complete than sex_at_birth
           LAND_ETNG_gebl1, LAND_ACHTS_gebl1, LAND_ETNG_gebl2, LAND_ACHTS_gebl2,
           # education level CBS and JGZ combined
           educc_mo, educc_fa,
           # add length and weight data of individual from previous contact moments
           lengte_4wks_znl, lengte_8wks_znl, lengte_3mnd_znl, lengte_4mnd_znl,
           gewicht_4wks_znl, gewicht_8wks_znl, gewicht_3mnd_znl, gewicht_4mnd_znl,
           bmi_4wks_znl, bmi_8wks_znl, bmi_3mnd_znl, bmi_4mnd_znl,
           
           # select the perined data
           jaar, lft_concept_mo, lft_concept_fa, par_cat, grav_cat, 
           sectio_ia, amww_f, amddd1ond_cat, geboortegew, # instead of gesl we use geslacht from jgz
           overdracht, pediater, verantw_bb, verantw_eb, verantw_zw, 
           partus, meerling, episiotomie, gebplaats, ligging, nicuopname,
           pijnbestrijding2, robson, ruptuur, congenafw_ja, fluxus, 
           vroeg_geb_24_37, vroeg_geb_24_28, vroeg_geb_28_34, vroeg_geb_34_37,
           laag_geb_gewicht, interpreg_cat,
           # select birth history data; we don't include the subgroups of N_vroeg and vooraf_zw_vroeg
           N_vroeg_24_37, #N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37,
           vooraf_zw_vroeg_24_37, #vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
           vooraf_sga, N_vooraf_sga, 
           
           # select CBS data
           residence_same_for_parents,
           LAND_ETNG_mo, LAND_ACHTS_mo, GBA_generation_mo, GBA_generation_kid,
           LAND_ETNG_fa, LAND_ACHTS_fa, GBA_generation_fa, 
           TYPHH, PLHH, AANTALPERSHHf, hh_single_parent, hh_married,
           ED_rentown, ED_woz, starts_with("SECM_"), starts_with("SPOLIS_"), 
           income_mo, income_fa, income_parents, income_hh, income_hh_source,
           l_income_hh_min_binary, l_income_hh_pov_binary, l_income_hh_eur_binary,
           l_income_hh_min_4j_binary, l_income_hh_pov_4j_binary, l_income_hh_eur_4j_binary,
           house_ownership, hh_vermogen, 
           # from health care data:
           starts_with("ZVWK_"),
           # remove birth care costs variables
           -c(ZVWK_birth_obstetrician_mo, ZVWK_birth_maternitycare_mo, ZVWK_birth_obstetrician_fa, ZVWK_birth_maternitycare_fa,
              ZVWK_hospital_mo, ZVWK_hospital_fa),
           # remove health care costs variables with 100% missingness
           -c(ZVWK_mentalh_spec_long_mo, ZVWK_mentalh_spec_long_fa, ZVWK_geriatric_mo, ZVWK_geriatric_fa,
              ZVWK_localnurse_mo, ZVWK_localnurse_fa, ZVWK_multidisc_mo, ZVWK_multidisc_fa, 
              ZVWK_sensory_mo, ZVWK_sensory_fa, ZVWK_total_mo, ZVWK_total_fa, ZVWK_deductible_mo, 
              ZVWK_deductible_fa, ZVWK_primarycare_residence_mo, ZVWK_primarycare_residence_fa,
              ZVWK_abroad_sub1_mo, ZVWK_abroad_sub1_fa, ZVWK_abroad_sub2_mo, ZVWK_abroad_sub2_fa)))

# QUESTION FOR POLINA: did we later decide to include the birth care costs variables in the JGZ cases? 
# I think they are included in the other JGZ cases

glimpse(predictors_full_model)
saveRDS(predictors_full_model, "case_jgz/data_outcome/overweight/overweight_jgz_dat_4m_comp_predictors_706_p.rds")


# now that we have joined the CBS and JGZ data
# we can move to the next step: dealing with missing data using multiple imputation in 05_jgz_preprocess_MI





