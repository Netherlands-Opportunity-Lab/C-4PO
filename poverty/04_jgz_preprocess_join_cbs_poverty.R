#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and (05_jgz_preprocess_MI) we prepare the JGZ data for analysis in 05_jgz_prediction.R

#### 04_jgz_preprocess_join_cbs.R ####
# THE GOAL OF THIS SCRIPT IS TO ADD THE CBS DATA TO THE JGZ DATA 

# load the necessary libraries 
library(tidyverse)
library(haven)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(dplyr)
setwd("H:/Mirthe/")

#### CASE: POVERTY (ARMOEDE) AT 2 YEARS, DOE-MOMENT 2 YEARS ####
# in this case, we do not predict a future outcome, we use the model to identify children in a situation (at risk) of poverty

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
jr2_armoede <- readRDS("case_jgz/repo/poverty/data/jr2_armoede_p.rds")

# specify the doe-moment
doe_moment_ind <- "doe_moment_2y"

join_data <- join_jgz(jr2_armoede)

# function for the selection with doe-moment 2 years
selection_2y_mo <- function(dat) {
  dat %>% 
    filter(doe_moment_2y %within% interval(start_date_mo, end_date_mo))
}

selection_2y_fa <- function(dat) {
  dat %>% 
    filter(doe_moment_2y %within% interval(start_date_fa, end_date_fa))
}

selection_2y <- function(selection_2y_mo, selection_2y_fa) {
  full_join(selection_2y_mo, selection_2y_fa,
            by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", 
                   "rins_mo" = "rins_mo", "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", 
                   "birthdate" = "birthdate", "birthday_kid" = "birthday_kid", "doe_moment_2y" = "doe_moment_2y"))
}


# CBS microdata files:
### 2.1 residence data

residence_dat <- readRDS("data/parents_jgz/residence_dat_jgz.rds") # don't need to load these data again if loaded for the previous case
residence_dat_kids <- readRDS("data/parents_jgz/residence_dat_kids_jgz.rds")

join_residence_mo <- join_jgz_mo(join_data = join_data, subject_data = residence_dat)
join_residence_fa <- join_jgz_fa(join_data, residence_dat)
join_residence_kids <- join_data %>% left_join(residence_dat_kids, by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"))

selection_2y_residence_mo <- selection_2y_mo(join_residence_mo)
selection_2y_residence_fa <- selection_2y_fa(join_residence_fa)
selection_2y_residence_kids <- join_residence_kids %>% 
  filter(doe_moment_2y %within% interval(start_date, end_date))

sum(is.na(join_data$rin_fa)) # 2547
nrow(join_data) - nrow(selection_2y_residence_mo) # 320
nrow(join_data) - nrow(selection_2y_residence_fa) # 3305
nrow(join_data) - nrow(selection_2y_residence_kids) # 162

selection_2y_residence_kids <- selection_2y_residence_kids %>% 
  left_join(readRDS("data/eigendomwoz_dat.rds"), 
            by = c("residence_type" = "SOORTOBJECTNUMMER", "residence" = "RINOBJECTNUMMER"), 
            relationship = "many-to-many") %>% 
  filter(year(doe_moment_2y) == year) %>% 
  select(-c(year))

pov_residence_dat <- selection_2y(selection_2y_residence_mo, selection_2y_residence_fa)

pov_residence_woz_dat <- full_join(pov_residence_dat, selection_2y_residence_kids,
                                   by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", 
                                          "rins_mo" = "rins_mo", "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", 
                                          "birthdate" = "birthdate", "birthday_kid" = "birthday_kid", "doe_moment_2y" = "doe_moment_2y"))


saveRDS(pov_residence_woz_dat, "case_jgz/selection/armoede_2y/pov_residence_woz_dat.rds")
rm(residence_dat, residence_dat_kids, sdq_residence_dat)
pov_residence_woz_dat$RINPERSOON[duplicated(pov_residence_woz_dat$RINPERSOON)]


### 2.2 education level data

educ_dat <- readRDS("data/parents_jgz/educ_dat_ref_jgz.rds")

join_educ_mo <- join_jgz_mo(join_data, educ_dat)
join_educ_fa <- join_jgz_fa(join_data, educ_dat)

selection_2y_educ_mo <- selection_2y_mo(join_educ_mo)
selection_2y_educ_fa <- selection_2y_fa(join_educ_fa)

pov_educ_dat <- selection_2y(selection_2y_educ_mo, selection_2y_educ_fa)

saveRDS(pov_educ_dat, "case_jgz/selection/armoede_2y/pov_educ_dat.rds")
rm(educ_dat)

### 2.3 socio-economic data

secm_dat <- readRDS("data/parents_jgz/secm_dat_NA0_jgz.rds")
# load the secm dataset with NA set to 0, for the secm categories NA (as opposed to 1) implies that the category does not apply

join_secm_mo <- join_jgz_mo(join_data, secm_dat)
join_secm_fa <- join_jgz_fa(join_data, secm_dat)

selection_2y_secm_mo <- selection_2y_mo(join_secm_mo)
selection_2y_secm_fa <- selection_2y_fa(join_secm_fa)

pov_secm_dat <- selection_2y(selection_2y_secm_mo, selection_2y_secm_fa)

saveRDS(pov_secm_dat, "case_jgz/selection/armoede_2y/pov_secm_dat.rds")
rm(secm_dat)

### 2.4 (S)POLIS DATA

polis_dat <- readRDS("data/parents_jgz/polis_dat_jgz.rds")

join_polis_mo <- join_jgz_mo(join_data, polis_dat)
join_polis_fa <- join_jgz_fa(join_data, polis_dat)

selection_2y_polis_mo <- selection_2y_mo(join_polis_mo)
selection_2y_polis_mo <- selection_2y_polis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_2y_polis_fa <- selection_2y_fa(join_polis_fa)
selection_2y_polis_fa <- selection_2y_polis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

pov_polis_dat <- selection_2y(selection_2y_polis_mo, selection_2y_polis_fa)
#saveRDS(pov_polis_dat, "case_jgz/selection/armoede_2y/pov_polis_dat.rds")

spolis_dat <- readRDS("data/parents_jgz/spolis_dat_jgz.rds")

join_spolis_mo <- join_jgz_mo(join_data, spolis_dat)
join_spolis_fa <- join_jgz_fa(join_data, spolis_dat)

selection_2y_spolis_mo <- selection_2y_mo(join_spolis_mo)
selection_2y_spolis_mo <- selection_2y_spolis_mo %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_mo, rin_mo) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_mo, .keep_all = TRUE)

selection_2y_spolis_fa <- selection_2y_fa(join_spolis_fa)
selection_2y_spolis_fa <- selection_2y_spolis_fa %>% 
  group_by(RINPERSOONS, RINPERSOON, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(RINPERSOON, rin_fa, .keep_all = TRUE)

pov_spolis_dat <- selection_2y(selection_2y_spolis_mo, selection_2y_spolis_fa)
#saveRDS(pov_spolis_dat, "case_jgz/selection/armoede_2y/pov_spolis_dat.rds")

pov_spolis_complete <- rbind(pov_polis_dat, pov_spolis_dat)
saveRDS(pov_spolis_complete, "case_jgz/selection/armoede_2y/pov_spolis_complete.rds")
rm(polis_dat, spolis_dat, pov_polis_dat, pov_spolis_dat)

### 2.5 income data: do not include income data given the outcome is created based on a household income variable

### 2.6 health data

health_dat <- readRDS("data/parents_jgz/health_dat_jgz.rds")

join_health_mo <- join_jgz_mo(join_data, health_dat)
join_health_fa <- join_jgz_fa(join_data, health_dat)

# select health care costs for t - 1, i.e. the calender year before doe-moment 2 years
selection_2y_health_mo <- join_health_mo %>% 
  filter((doe_moment_2y - years(1)) %within% interval(start_date_mo, end_date_mo))
selection_2y_health_fa <- join_health_fa %>% 
  filter((doe_moment_2y - years(1)) %within% interval(start_date_fa, end_date_fa))

pov_health_dat <- selection_2y(selection_2y_health_mo, selection_2y_health_fa)

saveRDS(pov_health_dat, "case_jgz/selection/armoede_2y/pov_health_dat.rds")
rm(health_dat)

### 2.7 household type data

gba_hh <- readRDS("data/parents_jgz/gba_hh_dat_jgz.rds")

join_gba_hh_dat <- join_data %>% 
  left_join(gba_hh, by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON"))

pov_gba_hh_dat <- join_gba_hh_dat %>% 
  filter(doe_moment_2y %within% interval(DATUMAANVANGHH, DATUMEINDEHH))

saveRDS(pov_gba_hh_dat, "case_jgz/selection/armoede_2y/pov_gba_hh_dat.rds")
rm(gba_hh)


# remove the files that are no longer necessary 
rm(join_residence_mo, join_residence_fa, join_residence_kids, join_educ_mo, join_educ_fa,
   join_gba_hh_dat, join_secm_mo, join_secm_fa, join_polis_mo, join_polis_fa,
   join_spolis_mo, join_spolis_fa, join_health_mo, join_health_fa)

rm(selection_2y_residence_mo, selection_2y_residence_fa, selection_2y_residence_kids, selection_2y_educ_mo, selection_2y_educ_fa,
   selection_2y_secm_mo, selection_2y_secm_fa, selection_2y_polis_mo, selection_2y_polis_fa,
   selection_2y_spolis_mo, selection_2y_spolis_fa, selection_2y_health_mo, selection_2y_health_fa)

#### 3. Join the JGZ data and the CBS data at the doe-moment 2 years #### 
# read data generated above if no longer in environment
pov_residence_woz_dat <- readRDS("case_jgz/selection/armoede_2y/pov_residence_woz_dat.rds")
pov_educ_dat <- readRDS("case_jgz/selection/armoede_2y/pov_educ_dat.rds")
pov_secm_dat <- readRDS("case_jgz/selection/armoede_2y/pov_secm_dat.rds")
pov_spolis_complete <- readRDS("case_jgz/selection/armoede_2y/pov_spolis_complete.rds")
pov_health_dat <- readRDS("case_jgz/selection/armoede_2y/pov_health_dat.rds")
pov_gba_hh_dat <- readRDS("case_jgz/selection/armoede_2y/pov_gba_hh_dat.rds")

pov_jgz_dat_2y <- jr2_armoede
join_pov_jgz_dat_2y <- function(pov_dat) {
  pov_jgz_dat_2y %>% 
    left_join(y = pov_dat,
              by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON", "rins_mo" = "rins_mo", 
                     "rin_mo" = "rin_mo", "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", "birthdate" = "birthdate",
                     "birthday_kid" = "birthday_kid", "doe_moment_2y" = "doe_moment_2y"))
} 
# use the function and select the dataset you want to join to the parent prnl dataset 
pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_residence_woz_dat) %>% 
  rename(start_date_residence_mo = start_date_mo,
         end_date_residence_mo = end_date_mo,
         start_date_residence_fa = start_date_fa,
         end_date_residence_fa = end_date_fa)

pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_educ_dat) %>% 
  rename(start_date_educ_mo = start_date_mo,
         end_date_educ_mo = end_date_mo,
         start_date_educ_fa = start_date_fa,
         end_date_educ_fa = end_date_fa)

pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_secm_dat) %>% 
  rename(start_date_secm_mo = start_date_mo,
         end_date_secm_mo = end_date_mo,
         start_date_secm_fa = start_date_fa,
         end_date_secm_fa = end_date_fa)

pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_spolis_complete) %>% 
  rename(start_date_spolis_mo = start_date_mo,
         end_date_spolis_mo = end_date_mo,
         start_date_spolis_fa = start_date_fa,
         end_date_spolis_fa = end_date_fa)

pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_health_dat) %>% 
  rename(start_date_health_mo = start_date_mo,
         end_date_health_mo = end_date_mo,
         start_date_health_fa = start_date_fa,
         end_date_health_fa = end_date_fa)

pov_jgz_dat_2y <- join_pov_jgz_dat_2y(pov_gba_hh_dat) %>% 
  rename(start_date_hh_kid = DATUMAANVANGHH,
         end_date_hh_kid = DATUMEINDEHH)

# there are only unique records per kid 
pov_jgz_dat_2y$RINPERSOON[duplicated(pov_jgz_dat_2y$RINPERSOON)]

# check the number of missing values per column 
colSums(is.na(pov_jgz_dat_2y))

# remove the separate data files that are no longer needed 
rm(join_data, pov_residence_woz_dat, pov_educ_dat, pov_secm_dat, pov_spolis_complete, 
   pov_health_dat, pov_gba_hh_dat)

# save the complete dataset for the case language development 
saveRDS(pov_jgz_dat_2y, "case_jgz/repo/poverty/data/pov_jgz_dat_2y_p.rds")


#### 4. Transform the CBS and JGZ data for analysis ####

### 4.1 Birth records 
# create a factor variable for prnl gestational age in weeks 
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(PRNL_gestational_age_weekf = factor(PRNL_gestational_age_week, ordered = TRUE)) 

# create a factor variable for prnl birthweight ventiles 
quantile(pov_jgz_dat_2y$PRNL_birthweight, probs = seq(0, 1, 1/20), na.rm = TRUE)
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(PRNL_birthweight_5 = ntile(PRNL_birthweight, 20),
         PRNL_birthweight_5 = factor(PRNL_birthweight_5)) #ordered = TRUE

# do the same for the new perined geboortegew data, we will use only one of these variables
quantile(pov_jgz_dat_2y$geboortegew, probs = seq(0, 1, 1/20), na.rm = TRUE)
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(geboortegew_5 = ntile(geboortegew, 20),
         geboortegew_5 = factor(geboortegew_5)) #ordered = TRUE

pov_jgz_dat_2y <- pov_jgz_dat_2y %>% filter(!geslacht == "onbekend")

# not necessary to change groups because 0 in group 3
table(pov_jgz_dat_2y$N_vroeg_24_37)
#pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
#  mutate(N_vroeg_24_37 = if_else(N_vroeg_24_37 == '2', '2-2+', 
#                                 if_else(N_vroeg_24_37 == '3', '2-2+', N_vroeg_24_37)),
#         N_vroeg_24_37 = ordered(N_vroeg_24_37, levels = c("nvt", "0", "1", "2-2+", "geen data")))

# set vroeg_geb variables to factor
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(vroeg_geb_24_28 = as_factor(vroeg_geb_24_28),
         vroeg_geb_28_34 = as_factor(vroeg_geb_28_34),
         vroeg_geb_34_37 = as_factor(vroeg_geb_34_37),
         jaar = as_factor(jaar))


### 4.2 residence data
# dummies to identify if the mother and father are registered at/ live(d) at the same residence
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(residence_same_for_parents = ifelse(residence_mo == residence_fa, 1, 0),
         residence_same_for_parents = as.factor(recode(residence_same_for_parents, 
                                                       "1" = "parents same residence", "0" = "parents not same residence")))

# !!! CHECK MISTAKE IN THE HOUSEHOLD DATA, also check hh_single_parent
table(pov_jgz_dat_2y$TYPHH)
table(pov_jgz_dat_2y$hh_married)
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(hh_married = ifelse(TYPHH == "Gehuwd paar zonder kinderen" | TYPHH == "Gehuwd paar met kinderen", 1, 0),
         hh_married = as_factor(hh_married))
table(pov_jgz_dat_2y$hh_single_parent)

### 4.3 education data 
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
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

pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
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
                                                                            ifelse(educc_mo == '8', '8_womaster', NA))))))))), #ordered = TRUE),
         educc_fa = factor(ifelse(educc_fa == '1', '1_geen',
                                  ifelse(educc_fa == '2', '2_basis',
                                         ifelse(educc_fa == '3', '3_vmbopraktijk',
                                                ifelse(educc_fa == '4', '4_vmbomavo',
                                                       ifelse(educc_fa == '5', '5_mbo',
                                                              ifelse(educc_fa == '6', '6_havovwo',
                                                                     ifelse(educc_fa == '7', '7_hbo',
                                                                            ifelse(educc_fa == '8', '8_womaster', NA)))))))))) %>%  #ordered = TRUE)) %>%
  select(-c(educ_cbs_mo, educ_cbs_fa, educ_jgz_mo, educ_jgz_fa))


### 4.5 health data
# sum the total health data costs per individual
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(ZVWK_sumtotal_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_other_mo)), na.rm = TRUE),
         ZVWK_sumtotal_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_other_fa)), na.rm = TRUE)) 

### 4.6 length and weight data
sum(pov_jgz_dat_2y$bmi_2yrs_znl > 5, na.rm = TRUE)
sum(pov_jgz_dat_2y$bmi_2yrs_znl < -5, na.rm = TRUE)
summary(pov_jgz_dat_2y$bmi_2yrs_znl)

# change outliers in observations at earlier contactmomenten to NA 
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(lengte_2yrs_znl = ifelse(lengte_2yrs_znl < -5 | lengte_2yrs_znl > 5, NA, lengte_2yrs_znl),
         gewicht_2yrs_znl = ifelse(gewicht_2yrs_znl < -5 | gewicht_2yrs_znl > 5, NA, gewicht_2yrs_znl),
         bmi_2yrs_znl = ifelse(bmi_2yrs_znl < -5 | bmi_2yrs_znl > 5, NA, bmi_2yrs_znl))

### 4.7 language development data
class(pov_jgz_dat_2y$taalomg)
pov_jgz_dat_2y <- pov_jgz_dat_2y %>% 
  mutate(taalomg = factor(taalomg, ordered = FALSE))
# mutate(taalomg = ordered(ifelse(!is.na(d_taalomg_3) & d_taalomg_3 == 1, "onvoldoende", 
#                                 ifelse(!is.na(d_taalomg_2) & d_taalomg_2 == 1, "matig",
#                                        ifelse(!is.na(d_taalomg_1) & d_taalomg_1 == 1, "voldoende", NA))), 
#                          levels = c("onvoldoende", "matig", "voldoende")))

table(pov_jgz_dat_2y$bst_nl)
table(pov_jgz_dat_2y$taalomg)
table(pov_jgz_dat_2y$meertaal)

saveRDS(pov_jgz_dat_2y, "case_jgz/repo/poverty/data/pov_jgz_dat_2y_p.rds")


#### 5. Prepare the data set for analysis: select the predictors ####

# FULL MODEL 
# this model includes all available predictors from JGZ, CBS and Perined
predictors <- pov_jgz_dat_2y %>%
  # select the outcome variable overweight which we created from JGZ variables lengte en gewicht
  select(c(l_income_hh_pov_binary,
           # select the variables from the JGZ data
           STED, geslacht, # COROP2020,
           LAND_ETNG_gebl1, LAND_ACHTS_gebl1, LAND_ETNG_gebl2, LAND_ACHTS_gebl2,
           educc_mo, educc_fa, #JGZ and CBS combi
           # add van wiechen items of individual from previous contact moments
           zin2w, pop6, taal_ont_2y,
           bst_nl, taalomg, meertaal,
           # add length and weight data of individual from previous contact moments
           bmi_2yrs_znl, # lengte_2yrs_znl, gewicht_2yrs_znl,
           # select the perined data
           jaar, lft_concept_mo, lft_concept_fa, par_cat, grav_cat, 
           sectio_ia, amww_f, amddd1ond_cat, geboortegew, # instead of gesl we use geslacht from jgz
           overdracht, pediater, verantw_bb, verantw_eb, verantw_zw, 
           partus, meerling, episiotomie, gebplaats, ligging, nicuopname,
           pijnbestrijding2, robson, ruptuur, congenafw_ja, fluxus, 
           vroeg_geb_24_37, vroeg_geb_24_28, vroeg_geb_28_34, vroeg_geb_34_37,
           vooraf_zw_vroeg_24_37, #vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
           N_vroeg_24_37, #N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37,
           laag_geb_gewicht, vooraf_sga, N_vooraf_sga, interpreg_cat,
           # select CBS data
           residence_same_for_parents,
           LAND_ETNG_mo, LAND_ACHTS_mo, GBA_generation_mo, GBA_generation_kid,
           LAND_ETNG_fa, LAND_ACHTS_fa, GBA_generation_fa, 
           TYPHH, PLHH, AANTALPERSHHf, hh_single_parent, hh_married,
           ED_rentown, ED_woz, starts_with("SECM_"), starts_with("SPOLIS_"), 
           income_hh_source, house_ownership, starts_with("ZVWK_"),
           # remove variables with 100% or >90% missingness
           -c(ZVWK_mentalh_spec_long_mo, ZVWK_mentalh_spec_long_fa, ZVWK_geriatric_mo, ZVWK_geriatric_fa,  #> 75% missing for geriatric or 0
              ZVWK_localnurse_mo, ZVWK_localnurse_fa, ZVWK_multidisc_mo, ZVWK_multidisc_fa, 
              ZVWK_sensory_mo, ZVWK_sensory_fa, ZVWK_total_mo, ZVWK_total_fa, ZVWK_deductible_mo, 
              ZVWK_deductible_fa, ZVWK_primarycare_residence_mo, ZVWK_primarycare_residence_fa,
              ZVWK_abroad_sub1_mo, ZVWK_abroad_sub1_fa, ZVWK_abroad_sub2_mo, ZVWK_abroad_sub2_fa)))

glimpse(predictors)
saveRDS(predictors, "case_jgz/repo/poverty/data/poverty_predictors_p.rds")

# now that we have joined the CBS and JGZ data
# we can move to the next step: analysis in 05_jgz_prediction.R



