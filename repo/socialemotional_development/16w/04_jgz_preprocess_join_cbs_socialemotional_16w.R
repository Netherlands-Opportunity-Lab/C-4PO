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


#### THE CASE: SOCIAL-EMOTIONAL DEVELOPMENT with SDQ (NL: SOCIAAL-EMOTIONELE ONTWIKKELING met SDQ) ####
# 'doe-moment': consultation moment during which the child is approx. 16 weeks gestational age
# 'outcome-age': consultation moment during which the child is approx. 5 years of age


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


#### 2. Filter the CBS data at the doe-moment 2 years ####

# read the case-specific data created in 03_jgz_preprocess_filter
groep2_sdq_16w <- readRDS("case_jgz/data_outcome/sdq_16w/groep2_sdq_16w_p.rds")
# specify the doe-moment
doe_moment_ind <- "doe_moment_16w"

# we use the birth mother in case available because we're predicting during pregnancy
groep2_sdq_16w <- groep2_sdq_16w %>% 
  mutate(rins_mo = if_else(!is.na(rins_mo_pr_clean), rins_mo_pr_clean, rins_mo),
         rin_mo = if_else(!is.na(rin_mo_pr_clean), rin_mo_pr_clean, rin_mo))
sum(groep2_sdq_16w$rin_mo != groep2_sdq_16w$Rinpersoon_Moeder, na.rm = T)

join_data <- join_jgz(groep2_sdq_16w)
# exclude children for who doe-moment is missing, due to PRNL_gestational_age and zwduur being missing N = 1771
sum(is.na(join_data$doe_moment_16w))
join_data <- join_data %>% drop_na(doe_moment_16w)


# function for the selection with doe-moment 16 weeks
selection_16w_mo <- function(dat) {
  dat %>% 
    filter(doe_moment_16w %within% interval(start_date_mo, end_date_mo))
}

selection_16w_fa <- function(dat) {
  dat %>% 
    filter(doe_moment_16w %within% interval(start_date_fa, end_date_fa))
}

selection_16w <- function(selection_16w_mo, selection_16w_fa) {
  full_join(selection_16w_mo, selection_16w_fa,
            by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", 
                   "rins_mo" = "rins_mo", "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", 
                   "birthdate" = "birthdate", "birthday_kid" = "birthday_kid", "doe_moment_16w" = "doe_moment_16w"))
}



# CBS microdata files:
### 2.1 residence data
residence_dat <- readRDS("data/parents_jgz/residence_dat_jgz.rds") # don't need to load these data again if loaded for the previous case

join_residence_mo <- join_jgz_mo(join_data = join_data, subject_data = residence_dat)
join_residence_fa <- join_jgz_fa(join_data, residence_dat)

selection_16w_residence_mo <- selection_16w_mo(join_residence_mo)
selection_16w_residence_fa <- selection_16w_fa(join_residence_fa)

nrow(join_data) - nrow(selection_16w_residence_mo) 
nrow(join_data) - nrow(selection_16w_residence_fa) 

sdq_residence_dat16w <- selection_16w(selection_16w_residence_mo, selection_16w_residence_fa)
rm(residence_dat)

eigendomwoz_dat <- readRDS("data/eigendomwoz_dat.rds")

selection_16w_residence_woz_mo <- selection_16w_residence_mo %>% 
  left_join(eigendomwoz_dat, by =c("residence_type_mo" = "SOORTOBJECTNUMMER", "residence_mo" = "RINOBJECTNUMMER")) %>% 
  rename_at(vars(c(ED_rentown, ED_woz, year)), 
            function(x) paste0(x, "_mo")) %>% 
  filter(year(doe_moment_16w) == year_mo) %>% 
  select(-c(year_mo))

selection_16w_residence_woz_fa <- selection_16w_residence_fa %>% 
  left_join(eigendomwoz_dat, by =c("residence_type_fa" = "SOORTOBJECTNUMMER", "residence_fa" = "RINOBJECTNUMMER")) %>% 
  rename_at(vars(c(ED_rentown, ED_woz, year)), 
            function(x) paste0(x, "_fa")) %>% 
  filter(year(doe_moment_16w) == year_fa) %>% 
  select(-c(year_fa))

sdq_residence_woz_dat16w_mo <- left_join(sdq_residence_dat16w, selection_16w_residence_woz_mo)  
sdq_residence_woz_dat16w_fa <- left_join(sdq_residence_dat16w, selection_16w_residence_woz_fa)

sdq_residence_woz_dat16w <- full_join(sdq_residence_woz_dat16w_mo, sdq_residence_woz_dat16w_fa)

saveRDS(sdq_residence_woz_dat16w, "case_jgz/selection/sdq_16w/sdq_residence_woz_dat16w.rds")
rm(eigendomwoz_dat, sdq_residence_dat16w)

# postcode and STED classification in the JGZ data represent records of the child
# in this case, we're interested in the records of the birth mother because we predict the outcome at 16 weeks gestational age
# add postcode and STED of the birth mother's residence to the data set
postcode_dat <- read_sav('G:///BouwenWonen/VSLPOSTCODEBUS/VSLPOSTCODEBUSV2023031.sav') %>% 
  mutate(DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
         DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
         SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
         POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM), 
         POSTCODENUM = as_factor(POSTCODENUM))

# add classification of stedelijkheid as an predictor 
pc4_sted <- read_excel("K:///Utilities/HULPbestanden/PC4/cbs_pc4_2020_v2.xlsx",
                       range = cell_rows(c(9, 11:4078))) 
pc4_sted <- pc4_sted[-c(1),] %>% select(c("Postcode-4", "...134")) %>% 
  rename(PC4 = "Postcode-4",
         STED = `...134`) %>% 
  mutate(STED = ifelse(STED == "-99997", NA, STED),
         #PC4 = as.factor(PC4),
         STED = as.factor(STED))

pc4_gem <- read_sav("K:/Utilities/HULPbestanden/PWR/PWR2020.sav", 
                    col_select = c("postc", "gemc")) %>% # select all variables to join labels
  mutate(postc = as.factor(postc),
         gemc = as.factor(gemc)) # municipality code 

gem_corop <- read_excel("K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx",
                        range = cell_cols("CA:CB")) %>% 
  mutate(GM2020 = as.factor(GM2020), 
         COROP2020 = as.factor(COROP2020)) %>% 
  unique()

postcode_dat <- postcode_dat %>% 
  left_join(pc4_sted, by = c("POSTCODENUM" = "PC4")) %>% 
  left_join(pc4_gem, by = c("POSTCODENUM" = "postc")) %>% 
  left_join(gem_corop, by = c("gemc" = "GM2020"))
rm(pc4_gem, pc4_sted, gem_corop)

postcode_dat <- postcode_dat %>% 
  drop_na(POSTCODENUM) %>%  
  group_by(SOORTOBJECTNUMMER, RINOBJECTNUMMER) %>% 
  slice(which.max(DATUMAANVPOSTCODENUMADRES))

saveRDS(postcode_dat, "data/postcode_dat.rds")

sdq_residence_woz_sted_dat16w_mo <- sdq_residence_woz_dat16w %>% 
  left_join(postcode_dat, by = c("residence_type_mo" = "SOORTOBJECTNUMMER", "residence_mo" = "RINOBJECTNUMMER")) %>% 
  rename_at(vars(c(POSTCODENUM, STED, gemc, COROP2020)), 
            function(x) paste0(x, "_mo")) %>% 
  select(-c(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES))

sdq_residence_woz_sted_dat16w_fa <- sdq_residence_woz_dat16w %>% 
  left_join(postcode_dat, by = c("residence_type_fa" = "SOORTOBJECTNUMMER", "residence_fa" = "RINOBJECTNUMMER")) %>% 
  rename_at(vars(c(POSTCODENUM, STED, gemc, COROP2020)), 
            function(x) paste0(x, "_fa")) %>% 
  select(-c(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES))

sdq_residence_woz_sted_dat16w <- full_join(sdq_residence_woz_sted_dat16w_mo, sdq_residence_woz_sted_dat16w_fa)

saveRDS(sdq_residence_woz_sted_dat16w, "case_jgz/selection/sdq_16w/sdq_residence_woz_sted_dat16w.rds")
rm(postcode_dat, sdq_residence_woz_sted_dat16w_mo, sdq_residence_woz_sted_dat16w_fa)


### 2.2 education level data

educ_dat <- readRDS("data/parents_jgz/educ_dat_ref_jgz.rds")

join_educ_mo <- join_jgz_mo(join_data, educ_dat)
join_educ_fa <- join_jgz_fa(join_data, educ_dat)

selection_16w_educ_mo <- selection_16w_mo(join_educ_mo)
selection_16w_educ_fa <- selection_16w_fa(join_educ_fa)

nrow(join_data) - nrow(selection_16w_educ_mo) 
nrow(join_data) - nrow(selection_16w_educ_fa) 

sdq_educ_dat16w <- selection_16w(selection_16w_educ_mo, selection_16w_educ_fa)

#attempt to get more educ observations

selection_16w_educm_mo <- join_educ_mo %>% 
  filter(!join_educ_mo$rin_mo %in% selection_16w_educ_mo$rin_mo) %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  slice(which.min(start_date_mo))

selection_16w_educm_mo <- bind_rows(selection_16w_educ_mo, selection_16w_educm_mo)

selection_16w_educm_fa <- join_educ_fa %>% 
  filter(!join_educ_fa$rin_fa %in% selection_16w_educ_fa$rin_fa) %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  slice(which.min(start_date_fa))

selection_16w_educm_fa <- bind_rows(selection_16w_educ_fa, selection_16w_educm_fa)

sdq_educm_dat16w <- selection_16w(selection_16w_educm_mo, selection_16w_educm_fa)

saveRDS(sdq_educ_dat16w, "case_jgz/selection/sdq_16w/sdq_educ_dat16w.rds")
saveRDS(sdq_educm_dat16w, "case_jgz/selection/sdq_16w/sdq_educm_dat16w.rds")
rm(educ_dat)

### 2.3 socio-economic data
secm_dat <- readRDS("data/parents_jgz/secm_dat_NA0_jgz.rds")
# load the secm dataset with NA set to 0, for the secm categories NA (as opposed to 1) implies that the category does not apply

join_secm_mo <- join_jgz_mo(join_data, secm_dat)
join_secm_fa <- join_jgz_fa(join_data, secm_dat)

selection_16w_secm_mo <- selection_16w_mo(join_secm_mo)
selection_16w_secm_fa <- selection_16w_fa(join_secm_fa)

nrow(join_data) - nrow(selection_16w_secm_mo) 
nrow(join_data) - nrow(selection_16w_secm_fa) 

sdq_secm_dat16w <- selection_16w(selection_16w_secm_mo, selection_16w_secm_fa)

saveRDS(sdq_secm_dat16w, "case_jgz/selection/sdq_16w/sdq_secm_dat16w.rds")
rm(secm_dat)

### 2.4 (S)POLIS DATA

polis_dat <- readRDS("data/parents_jgz/polis_dat_jgz.rds")

join_polis_mo <- join_jgz_mo(join_data, polis_dat)
join_polis_fa <- join_jgz_fa(join_data, polis_dat)

selection_16w_polis_mo <- selection_16w_mo(join_polis_mo)
selection_16w_polis_mo <- selection_16w_polis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_16w_polis_fa <- selection_16w_fa(join_polis_fa)
selection_16w_polis_fa <- selection_16w_polis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

sdq_polis_dat16w <- selection_16w(selection_16w_polis_mo, selection_16w_polis_fa)
#saveRDS(sdq_polis_dat16w, "case_jgz/selection/sdq_16w/sdq_polis_dat16w.rds")

spolis_dat <- readRDS("data/parents_jgz/spolis_dat_jgz.rds")

join_spolis_mo <- join_jgz_mo(join_data, spolis_dat)
join_spolis_fa <- join_jgz_fa(join_data, spolis_dat)

selection_16w_spolis_mo <- selection_16w_mo(join_spolis_mo)
selection_16w_spolis_mo <- selection_16w_spolis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_16w_spolis_fa <- selection_16w_fa(join_spolis_fa)
selection_16w_spolis_fa <- selection_16w_spolis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

sdq_spolis_dat16w <- selection_16w(selection_16w_spolis_mo, selection_16w_spolis_fa)
#saveRDS(sdq_spolis_dat, "case_jgz/selection/sdq_16w/sdq_spolis_dat.rds")

nrow(join_data) - nrow(selection_16w_polis_mo) - nrow(selection_16w_spolis_mo)
nrow(join_data) - nrow(selection_16w_polis_fa) - nrow(selection_16w_spolis_fa) 

sdq_spolis_complete16w <- rbind(sdq_polis_dat16w, sdq_spolis_dat16w)
saveRDS(sdq_spolis_complete16w, "case_jgz/selection/sdq_16w/sdq_spolis_complete16w.rds")
rm(polis_dat, spolis_dat)

### 2.5 income data

income_dat <- readRDS("data/parents_jgz/income_dat_jgz.rds")

join_income_mo <- join_jgz_mo(join_data, income_dat)
join_income_fa <- join_jgz_fa(join_data, income_dat)

selection_16w_income_mo <- selection_16w_mo(join_income_mo)
selection_16w_income_fa <- selection_16w_fa(join_income_fa)

nrow(join_data) - nrow(selection_16w_income_mo) 
nrow(join_data) - nrow(selection_16w_income_fa) 

sdq_income_dat16w <- selection_16w(selection_16w_income_mo, selection_16w_income_fa)

saveRDS(sdq_income_dat16w, "case_jgz/selection/sdq_16w/sdq_income_dat16w.rds")
rm(income_dat)

### 2.6 health data
health_dat <- readRDS("data/parents_jgz/health_dat_jgz.rds")

join_health_mo <- join_jgz_mo(join_data, health_dat)
join_health_fa <- join_jgz_fa(join_data, health_dat)

# select health care costs for t - 1, i.e. the calender year before doe-moment 16 weeks
selection_16w_health_mo <- join_health_mo %>% 
  filter((doe_moment_16w - years(1)) %within% interval(start_date_mo, end_date_mo))
selection_16w_health_fa <- join_health_fa %>% 
  filter((doe_moment_16w - years(1)) %within% interval(start_date_fa, end_date_fa))

nrow(join_data) - nrow(selection_16w_health_mo) 
nrow(join_data) - nrow(selection_16w_health_fa) 
# health data is only available from 2009 on

sdq_health_dat16w <- selection_16w(selection_16w_health_mo, selection_16w_health_fa)

saveRDS(sdq_health_dat16w, "case_jgz/selection/sdq_16w/sdq_healtht-1_dat16w.rds")
rm(health_dat)


### 2.7 HOUSEHOLD TYPE DATA
gba_hh_dat <- readRDS("data/parents_jgz/gba_hh_dat_jgz.rds") %>% 
  rename(rins = RINPERSOONS,
         rin = RINPERSOON, 
         start_date = DATUMAANVANGHH,
         end_date = DATUMEINDEHH)

join_gba_hh_mo <- join_jgz_mo(join_data, gba_hh_dat)
join_gba_hh_fa <- join_jgz_fa(join_data, gba_hh_dat)

selection_16w_gba_hh_mo <- selection_16w_mo(join_gba_hh_mo)
selection_16w_gba_hh_fa <- selection_16w_fa(join_gba_hh_fa)

nrow(join_data) - nrow(selection_16w_gba_hh_mo) 
nrow(join_data) - nrow(selection_16w_gba_hh_fa) 

sdq_gba_hh_dat16w <- selection_16w(selection_16w_gba_hh_mo, selection_16w_gba_hh_fa)

saveRDS(sdq_gba_hh_dat16w, "case_jgz/selection/sdq_16w/sdq_gba_hh_dat16w.rds")
rm(gba_hh_dat)


### 2.8 Household income
hh_income_dat_jgz <- readRDS("H:/Mirthe/data/parents_jgz/hh_income_dat_jgz.rds") %>% 
  mutate(l_income_hh_pov_binary = as_factor(l_income_hh_pov_binary),
         l_income_hh_min_binary = as_factor(l_income_hh_min_binary),
         l_income_hh_eur_binary = as_factor(l_income_hh_eur_binary),
         l_income_hh_pov_4j_binary = as_factor(l_income_hh_pov_4j_binary),
         l_income_hh_min_4j_binary = as_factor(l_income_hh_min_4j_binary),
         l_income_hh_eur_4j_binary = as_factor(l_income_hh_eur_4j_binary))

join_hh_income_mo <- join_jgz_mo(join_data, hh_income_dat_jgz)
join_hh_income_fa <- join_jgz_fa(join_data, hh_income_dat_jgz)

selection_16w_hh_income_mo <- join_hh_income_mo %>% 
  filter(year(doe_moment_16w) == year_mo)

selection_16w_hh_income_fa <- join_hh_income_fa %>% 
  filter(year(doe_moment_16w) == year_fa)

nrow(join_data) - nrow(selection_16w_hh_income_mo) # 89
nrow(join_data) - nrow(selection_16w_hh_income_fa) # 404

sdq_hh_income_dat16w <- selection_16w(selection_16w_hh_income_mo, selection_16w_hh_income_fa)

saveRDS(sdq_hh_income_dat16w, "case_jgz/selection/sdq_16w/sdq_hh_income_dat16w.rds")
rm(hh_income_dat_jgz)

### 2.9 Household vermogen
hh_schulden_jgz <- readRDS("H:/Mirthe/data/parents_jgz/hh_schulden_jgz.rds")

join_hh_schulden_mo <- join_jgz_mo(join_data, hh_schulden_jgz)
join_hh_schulden_fa <- join_jgz_fa(join_data, hh_schulden_jgz)

selection_16w_hh_schulden_mo <- join_hh_schulden_mo %>% 
  filter(year(doe_moment_16w) == year_mo)

selection_16w_hh_schulden_fa <- join_hh_schulden_fa %>% 
  filter(year(doe_moment_16w) == year_fa)

nrow(join_data) - nrow(selection_16w_hh_schulden_mo) # 89
nrow(join_data) - nrow(selection_16w_hh_schulden_fa) # 404

sdq_hh_schulden_dat16w <- selection_16w(selection_16w_hh_schulden_mo, selection_16w_hh_schulden_fa)

saveRDS(sdq_hh_schulden_dat16w, "case_jgz/selection/sdq_16w/sdq_hh_schulden_dat16w.rds")
rm(hh_schulden_jgz)

# remove the files that are no longer necessary 
rm(join_residence_mo, join_residence_fa, join_educ_mo, join_educ_fa,
   join_secm_mo, join_secm_fa, join_polis_mo, join_polis_fa,
   join_spolis_mo, join_spolis_fa, join_income_mo, join_income_fa, 
   join_health_mo, join_health_fa, join_hh_income_fa, join_hh_income_mo, 
   join_hh_schulden_mo, join_hh_schulden_fa, join_gba_hh_mo, join_gba_hh_fa)

rm(selection_16w_residence_mo, selection_16w_residence_fa, selection_16w_residence_woz_mo, selection_16w_residence_woz_fa,
   sdq_residence_woz_dat16w_mo, sdq_residence_woz_dat16w_fa, selection_16w_educ_mo, selection_16w_educ_fa,
   selection_16w_educm_mo, selection_16w_educm_fa, selection_16w_secm_mo, selection_16w_secm_fa, 
   selection_16w_polis_mo, selection_16w_polis_fa, selection_16w_spolis_mo, selection_16w_spolis_fa,
   sdq_polis_dat16w, sdq_spolis_dat16w, selection_16w_income_mo, selection_16w_income_fa, 
   selection_16w_health_mo, selection_16w_health_fa, selection_16w_hh_income_mo, selection_16w_hh_income_fa,
   selection_16w_hh_schulden_mo, selection_16w_hh_schulden_fa, selection_16w_gba_hh_mo, selection_16w_gba_hh_fa)



#### 3. Join the JGZ data and the CBS data at the doe-moment 2 years #### 
# read data generated above if no longer in environment

sdq_residence_woz_sted_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_residence_woz_sted_dat16w.rds")
sdq_educ_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_educ_dat16w.rds")
sdq_gba_hh_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_gba_hh_dat16w.rds")
sdq_secm_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_secm_dat16w.rds")
sdq_spolis_complete16w <- readRDS("case_jgz/selection/sdq_16w/sdq_spolis_complete16w.rds")
sdq_income_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_income_dat16w.rds")
sdq_health_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_healtht-1_dat16w.rds")
sdq_hh_income_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_hh_income_dat16w.rds")
sdq_hh_schulden_dat16w <- readRDS("case_jgz/selection/sdq_16w/sdq_hh_schulden_dat16w.rds")

sdq_jgz_dat_16w <- groep2_sdq_16w %>% #readRDS("case_jgz/data_outcome/sdq_16w/groep2_sdq_16w_p.rds") %>% 
  drop_na(doe_moment_16w)

join_sdq_jgz_dat_16w <- function(sdq_dat) {
  sdq_jgz_dat_16w %>% 
    left_join(y = sdq_dat,
              by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", "rins_mo" = "rins_mo", 
                     "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", "birthdate" = "birthdate",
                     "birthday_kid" = "birthday_kid", "doe_moment_16w" = "doe_moment_16w"))
} 
# use the function and select the dataset you want to join to the parent prnl dataset 
sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_residence_woz_sted_dat16w) %>% 
  rename(start_date_residence_mo = start_date_mo,
         end_date_residence_mo = end_date_mo,
         start_date_residence_fa = start_date_fa,
         end_date_residence_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_educ_dat16w) %>% 
  rename(start_date_educ_mo = start_date_mo,
         end_date_educ_mo = end_date_mo,
         start_date_educ_fa = start_date_fa,
         end_date_educ_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_gba_hh_dat16w) %>% 
  rename(start_date_hh_mo = start_date_mo,
         end_date_hh_mo = end_date_mo,
         start_date_hh_fa = start_date_fa,
         end_date_hh_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_secm_dat16w) %>% 
  rename(start_date_secm_mo = start_date_mo,
         end_date_secm_mo = end_date_mo,
         start_date_secm_fa = start_date_fa,
         end_date_secm_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_spolis_complete16w) %>% 
  rename(start_date_spolis_mo = start_date_mo,
         end_date_spolis_mo = end_date_mo,
         start_date_spolis_fa = start_date_fa,
         end_date_spolis_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_income_dat16w) %>% 
  rename(start_date_income_mo = start_date_mo,
         end_date_income_mo = end_date_mo,
         start_date_income_fa = start_date_fa,
         end_date_income_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_health_dat16w) %>% 
  rename(start_date_health_mo = start_date_mo,
         end_date_health_mo = end_date_mo,
         start_date_health_fa = start_date_fa,
         end_date_health_fa = end_date_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_hh_income_dat16w) %>% 
  rename(year_hh_income_mo = year_mo,
         year_hh_income_fa = year_fa)

sdq_jgz_dat_16w <- join_sdq_jgz_dat_16w(sdq_hh_schulden_dat16w) %>% 
  rename(year_hh_schulden_mo = year_mo,
         year_hh_schulden_fa = year_fa)

# select only unique observations, there are no duplicates 
sdq_jgz_dat_16w$RINPERSOON[duplicated(sdq_jgz_dat_16w$RINPERSOON)] 

# check the number of missing values per column 
colSums(is.na(sdq_jgz_dat_16w))

# remove the separate data files that are no longer needed 
rm(sdq_residence_woz_sted_dat16w, sdq_educ_dat16w, sdq_educm_dat16w, sdq_secm_dat16w, sdq_spolis_complete16w, 
   sdq_income_dat16w, sdq_health_dat16w, sdq_hh_income_dat16w, sdq_hh_schulden_dat16w, sdq_gba_hh_dat16w)

# save the complete dataset for the case language development 
saveRDS(sdq_jgz_dat_16w, "case_jgz/data_outcome/sdq_16w/sdq_jgz_dat_16w_p.rds")




#### 4. Transform the CBS and JGZ data for analysis ####

### 4.1 birth history records
# look at N_vroeg_24_37 categories to see if there are categories with too little observations
table(sdq_jgz_dat_16w$N_vroeg_24_37)
# not necessary to change groups because 0 in group 3
#sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
#  mutate(N_vroeg_24_37 = if_else(N_vroeg_24_37 == '2', '2-2+', 
#                                 if_else(N_vroeg_24_37 == '3', '2-2+', N_vroeg_24_37)),
#         N_vroeg_24_37 = ordered(N_vroeg_24_37, levels = c("nvt", "0", "1", "2-2+", "geen data")))

# set vroeg_geb variables to factor
sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
  mutate(jaar = as_factor(jaar))


### 4.2 residence data
# dummies to identify if the mother and father are registered at/ live(d) at the same residence
sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
  mutate(residence_same_for_parents = ifelse(residence_mo == residence_fa, 1, 0),
         residence_same_for_parents = as.factor(recode(residence_same_for_parents, 
                                                       "1" = "parents same residence", "0" = "parents not same residence")))

sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
  mutate(hh_parents_different = ifelse(income_hh_mo != income_hh_fa, 1, 0),
         hh_parents_different = as.factor(recode(hh_parents_different, 
                                                 "1" = "different households", "0" = "same households")))

### 4.3 education data
sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
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

sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
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
sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
  rowwise() %>% 
  mutate(income_parents = ifelse(is.na(income_mo) & is.na(income_fa), NA, sum(income_mo, income_fa, na.rm = TRUE))) %>% 
  ungroup()

### 4.5 health data
# sum the total health data costs per individual
sdq_jgz_dat_16w <- sdq_jgz_dat_16w %>% 
  mutate(ZVWK_sumtotal_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_other_mo)), na.rm = TRUE),
         ZVWK_sumtotal_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_other_fa)), na.rm = TRUE)) 



#### 5. Prepare the data set for analysis: select the predictors ####

glimpse(sdq_jgz_dat_2y)

# FULL MODEL 
# this model includes all available predictors from JGZ, CBS and Perined
predictors_full_model <- sdq_jgz_dat_16w %>%
  # select the outcome variable sdq_risk which we created from JGZ variable sdq_t
  select(c(sdq_risk,
           # select the variables from the JGZ data
           STED_mo, geslacht, # STED_fa
           LAND_ETNG_gebl1, LAND_ACHTS_gebl1, LAND_ETNG_gebl2, LAND_ACHTS_gebl2,
           educc_mo, educc_fa,
           # perined data
           jaar, par_cat, grav_cat, meerling, 
           lft_concept_mo, lft_concept_fa,
           vooraf_zw_vroeg_24_37, #vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
           N_vroeg_24_37, #N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37,
           vooraf_sga, N_vooraf_sga, interpreg_cat,
           # select CBS data
           LAND_ACHTS_mo, GBA_generation_mo, LAND_ETNG_mo,
           LAND_ACHTS_fa, GBA_generation_fa, LAND_ETNG_fa, 
           TYPHH_mo, PLHH_mo, AANTALPERSHHf_mo, #TYPHH_fa, PLHH_fa, AANTALPERSHHf_fa,
           hh_single_parent_mo, hh_married_mo, #hh_single_parent_fa, hh_married_fa,
           residence_same_for_parents,
           ED_woz_mo, ED_woz_fa, ED_rentown_mo, ED_rentown_fa,
           starts_with("SECM_"), 
           starts_with("SPOLIS_"), income_fa, income_parents, income_mo,
           income_hh_mo, house_ownership_mo, income_hh_source_mo, hh_parents_different, # income_hh_fa, house_ownership_fa, income_hh_source_fa,
           l_income_hh_pov_binary_mo, l_income_hh_min_binary_mo, l_income_hh_eur_binary_mo, #l_income_hh_pov_binary_fa, l_income_hh_min_binary_fa,
           l_income_hh_pov_4j_binary_mo, l_income_hh_min_4j_binary_mo, l_income_hh_eur_4j_binary_mo,
           hh_vermogen_mo, starts_with("ZVWK_"), # hh_vermogen_fa,
           # remove variables with 100% missingness
           -c(ZVWK_mentalh_spec_long_mo, ZVWK_mentalh_spec_long_fa, ZVWK_geriatric_mo, ZVWK_geriatric_fa,
              ZVWK_localnurse_mo, ZVWK_localnurse_fa, ZVWK_multidisc_mo, ZVWK_multidisc_fa, 
              ZVWK_sensory_mo, ZVWK_sensory_fa, ZVWK_total_mo, ZVWK_total_fa, ZVWK_deductible_mo, 
              ZVWK_deductible_fa, ZVWK_primarycare_residence_mo, ZVWK_primarycare_residence_fa,
              ZVWK_abroad_sub1_mo, ZVWK_abroad_sub1_fa, ZVWK_abroad_sub2_mo, ZVWK_abroad_sub2_fa)))

glimpse(predictors_full_model)
saveRDS(predictors_full_model, "case_jgz/data_outcome/sdq_16w/sdq_predictors_p.rds")


# now that we have joined the CBS and JGZ data
# we can move to the next step: dealing with missing data using multiple imputation in 05_jgz_preprocess_MI



