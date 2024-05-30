# load the necessary libraries 
library(tidyverse)
library(readxl)
library(haven)
library(lubridate)
library(dplyr)
#library(mice)
setwd("H:/Mirthe")

#################################################################################
# 02_PREPROCESSING: READY THE PERINED AND CBS DATA
#################################################################################

#### 1. CREATE ONE BIG DATA SET WITH BOTH PERINED AND CBS MICRODATA

### Child and parent information

# load the birth cohort data; this cohort contains all births in 1999-2020 as registered by perined
# we have 1 cleaned the data and 2 generated birth history variables
birthcohort_join <- readRDS("/Polina/perined_cleaned_for_models_complete_14Nov2023.rds") %>% 
  ungroup()

# look at the data
glimpse(birthcohort_join)

# set rinpersoons to factor
birthcohort_join <- birthcohort_join %>% 
  mutate(Rinpersoons_Kind = as_factor(Rinpersoons_Kind),
         Rinpersoons_Moeder = as_factor(Rinpersoons_Moeder))

# read the kindouder data from CBS
kindouder_data <- read_sav("G:/Bevolking/KINDOUDERTAB/KINDOUDER2022TABV1.sav")
kindouder_data <- kindouder_data %>% 
  mutate(RINPERSOONS = as_factor(RINPERSOONS, levels = "values"),
         RINPERSOONSpa = as_factor(RINPERSOONSpa, levels = "values"),
         RINPERSOONSMa = as_factor(RINPERSOONSMa, levels = "values"))

# add the RINPERSOON/RINPERSOONS of the (legal) parents from kindoudertab to the birthcohort data
birthcohort_join <- left_join(
  x = birthcohort_join,
  y = kindouder_data, 
  by = c("Rinpersoons_Kind" = "RINPERSOONS", "Rinpersoon_Kind" = "RINPERSOON")) %>% 
  rename(rins_fa = RINPERSOONSpa,
         rin_fa = RINPERSOONpa) %>% 
  # in this case we use the birth mother from the perined data instead of the legal mother from CBS data, 
  # we use the birth mother because we are interested in birth outcomes 
  select(-c("RINPERSOONSMa", "RINPERSOONMa", "XKOPPELNUMMER"))

# in 99.7% of the records the birth mother in perined and the legal mother in CBS are the same person
# sum(birthcohort_join$Rinpersoons_Moeder == birthcohort_join$RINPERSOONSMa & birthcohort_join$Rinpersoon_Moeder == birthcohort_join$RINPERSOONMa, na.rm = T)/ 
#   nrow(birthcohort_join)*100

# clean the birthcohort_join to recognise NAs in kindouder_path
birthcohort_join <- birthcohort_join %>% 
  mutate(rin_fa = if_else(rin_fa == "---------", NA, rin_fa),
         Rinpersoon_Moeder = if_else(Rinpersoon_Moeder == "---------", NA, Rinpersoon_Moeder),
         Rinpersoon_Moeder = if_else(Rinpersoon_Moeder == "000000000", NA, Rinpersoon_Moeder))
rm(kindouder_data)

# save the data
#saveRDS(birthcohort_join, "case1_preterm_birth/data/prnl/birthcohort_join.rds")


# add information about parents' birthday, origin and generation as recorded in GBAPERSOONSTAB
gba_dat <- readRDS("data/parents_prnl/gba_dat.rds")
birthcohort_join <- birthcohort_join %>% 
  left_join(gba_dat,
            by = c("Rinpersoons_Moeder" = "rins", "Rinpersoon_Moeder" = "rin")) %>% 
  rename_at(vars(c("GBA_origin", "GBA_generation", "birthday")), 
            function(x) paste0(x, "_mo")) %>% 
  left_join(gba_dat,
            by = c("rins_fa" = "rins", "rin_fa" = "rin")) %>% 
  rename_at(vars(c("GBA_origin", "GBA_generation", "birthday")), 
            function(x) paste0(x, "_fa"))
rm(gba_dat)


# create a new variable, conception of the child, by subtracting gestational age in days from the birthday
birthcohort_join <- birthcohort_join %>% 
  mutate(conception = ymd(ddgeb) - weeks(amww))

# create the outcome (i.e. dependent) variable, premature_birth, based on gestational age in weeks
# children born after less than 32 or 37 weeks of preganacy are considered premature, pregnancies of less than 24 weeks are not always recorded so we disregard these pregnancies
birthcohort_join <- birthcohort_join %>%
  mutate(premature_birth = ifelse(amww >= 24 & amww < 32, 1, 0),
         premature_birth = as_factor(premature_birth),
         premature_birth37 = ifelse(amww >= 24 & amww < 37, 1, 0),
         premature_birth37 = as_factor(premature_birth37))


#### Add predictor variables from GBA #### 

# Add age of parents at the time of conception 
birthcohort_join <- birthcohort_join %>%
  mutate(age_at_concp_mo = trunc((birthday_mo %--% conception) / years(1)),
         age_at_concp_fa = trunc((birthday_fa %--% conception) / years(1)))

# look at the age at conception of the mother and father
hist(birthcohort_join$age_at_concp_mo)
hist(birthcohort_join$age_at_concp_fa)

# clean unrealistic age values by setting them to NA 
birthcohort_join <- birthcohort_join %>% 
  mutate(age_at_concp_mo = ifelse(age_at_concp_mo < 12, NA, age_at_concp_mo),
         age_at_concp_fa = ifelse(age_at_concp_fa < 10, NA, age_at_concp_fa))

#categorize the age at conception variables (we don't use these in analysis)
birthcohort_join <- birthcohort_join %>% 
  mutate(age_at_concp_mo_young = ifelse(age_at_concp_mo < 21, 1, 0),
         age_at_concp_mo_mid = ifelse(age_at_concp_mo >= 21 & age_at_concp_mo < 40, 1, 0),
         age_at_concp_mo_old = ifelse(age_at_concp_mo >= 40, 1, 0),
         age_at_concp_fa_young = ifelse(age_at_concp_fa < 21, 1, 0),
         age_at_concp_fa_mid = ifelse(age_at_concp_fa >= 21 & age_at_concp_fa < 45, 1, 0),
         age_at_concp_fa_old = ifelse(age_at_concp_fa >= 45, 1, 0),
         across(c(starts_with("age_at_concp_mo_")), ~as.factor(.)),
         across(c(starts_with("age_at_concp_fa_")), ~as.factor(.)))

# add classification for GBA_origin from CBS herkomst referentiebestand
origin_ref <- read_sav(loc$origin_ref, # loc$origin_ref = "K:/Utilities/Code_Listings/SSBreferentiebestanden/LANDAKTUEELREFV13.SAV"
                       col_select = c("LAND", "ETNGRP", "LANDAKTUEELACHTERSTANDBO")) %>% 
  rename(GBA_origin_ACHTS = LANDAKTUEELACHTERSTANDBO,
         GBA_origin_ETNG = ETNGRP) %>%
  mutate(LAND = as_factor(LAND, level = "values"),
         GBA_origin_ACHTS = as_factor(GBA_origin_ACHTS, level = "values"),
         GBA_origin_ETNG = as_factor(GBA_origin_ETNG, level = "values"))

birthcohort_join <- birthcohort_join %>%  
  left_join(origin_ref, by = c("GBA_origin_mo" = "LAND"), suffix = c("_mo", "_fa")) %>%   # GBAHERKOMSTGROEPERING_mo
  left_join(origin_ref, by = c("GBA_origin_fa" = "LAND"), suffix = c("_mo", "_fa"))       # GBAHERKOMSTGROEPERING_fa

# save and read the dataset 
#saveRDS(birthcohort_join, "case1_preterm_birth/data/prnl/birthcohort_join.rds")
birthcohort_join <- readRDS("case1_preterm_birth/data/prnl/birthcohort_join.rds")


#### JOIN THE CBS MICRODATA  ####
# Step 1: take the birthcohort and join CBS microdata on a selected subject by RINPERSOONS/RINPERSOON of the mother and father 
# Step 2: select the CBS microdata record of interest based on the doe-moment; we're interested in information of the parents at the moment of conception

# create a dataset to identify children and their parents, which also records birthday and conception of the child
# we'll use this dataset to join the CBS microdata from various subjects
join_conception <- birthcohort_join %>% 
  select(c("Rinpersoons_Kind", "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", 
           "rins_fa", "rin_fa", "ddgeb", "conception"))
# check if the records are unique: all N 3401872 records are unique 

#saveRDS(join_conception, "case1_preterm_birth/data/prnl/join_conception.rds")
join_conception <- readRDS("case1_preterm_birth/data/prnl/join_conception.rds")

# function to join a CBS dataset with join_conception, to link data from the parents to child 
# for the mother 
join_conception_mo <- function(dat) {
  join_conception %>% 
    left_join(y = dat,
              by = c("Rinpersoons_Moeder" = "rins", "Rinpersoon_Moeder" = "rin"),
              relationship = "many-to-many") %>% 
    rename_at(vars(-c("Rinpersoons_Kind", "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", 
                      "rins_fa", "rin_fa", "ddgeb", "conception")), 
              function(x) paste0(x, "_mo"))
}

# for the father
join_conception_fa <- function(dat) {
  join_conception %>% 
    left_join(y = dat,
              by = c("rins_fa" = "rins", "rin_fa" = "rin"),
              relationship = "many-to-many") %>% 
    rename_at(vars(-c("Rinpersoons_Kind", "Rinpersoon_Kind", "Rinpersoons_Moeder", "Rinpersoon_Moeder", 
                      "rins_fa", "rin_fa", "ddgeb", "conception")), 
              function(x) paste0(x, "_fa"))
}

# in the join we add records of the parents for all available years, we are interested in the record/information
# at the time of conception (our doe-moment/ moment at which we predict), we filter to include only those records
# for the mother
selection_mo <- function(dat) {
  dat %>% 
    filter(conception %within% interval(start_date_mo, end_date_mo))
}

#for the father
selection_fa <- function(dat) {
  dat %>% 
    filter(conception %within% interval(start_date_fa, end_date_fa))
}

# function to join the records of the mother and the father
selection <- function(selection_mo, selection_fa) {
  full_join(selection_mo, selection_fa,
            by = c("Rinpersoons_Kind" = "Rinpersoons_Kind", "Rinpersoon_Kind" = "Rinpersoon_Kind", 
                   "Rinpersoons_Moeder" = "Rinpersoons_Moeder", "Rinpersoon_Moeder" = "Rinpersoon_Moeder",
                   "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", "ddgeb" = "ddgeb", "conception" = "conception"))
} 


#### OBTAIN AND FILTER THE DATA PER SUBJECT ####

#### 1.1 residence data ####
# read CBS microdata file
residence_dat <- readRDS("data/parents_prnl/residence_dat.rds")

# join records of the parents
join_residence_mo <- join_conception_mo(residence_dat)
join_residence_fa <- join_conception_fa(residence_dat)

# filter and select records at doe-moment
selection_residence_mo <- selection_mo(join_residence_mo)
selection_residence_fa <- selection_fa(join_residence_fa)

# look at for how many mothers/fathers we don't have a record on this subject 
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_residence_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_residence_fa)) / nrow(birthcohort_join) * 100 

# combine selected subject data of the mother and father 
pretbirth_residence_dat <- selection(selection_residence_mo, selection_residence_fa)

# save the subject data
#saveRDS(pretbirth_residence_dat, "case1_preterm_birth/data/prnl/pretbirth_residence_dat.rds")

#### 1.2 EIGENDOMWOZ data ####
# read CBS microdata file
eigendomwoz_dat <- readRDS("data/parents_prnl/eigendomwoz_dat.rds")

selection_woz_mo <- selection_residence_mo %>% 
  # join by SOORTOBJECTNUMMER/RINOBJECTNUMMER, the ID of the place of residence
  left_join(eigendomwoz_dat, 
            by = c("residence_type_mo" = "SOORTOBJECTNUMMER", "residence_mo" = "RINOBJECTNUMMER")) %>% 
  filter(conception %within% interval(start_date, end_date)) %>% # same as the filter function
  select(-c(start_date, end_date)) %>% 
  rename(ED_rentown_mo = ED_rentown,
         ED_woz_mo = ED_woz)

# join by SOORTOBJECTNUMMER/RINOBJECTNUMMER, the ID of the place of residence
selection_woz_fa <- selection_residence_fa %>% 
  left_join(eigendomwoz_dat, 
            by = c("residence_type_fa" = "SOORTOBJECTNUMMER", "residence_fa" = "RINOBJECTNUMMER")) %>% 
  filter(conception %within% interval(start_date, end_date)) %>% # same as the filter function
  select(-c(start_date, end_date)) %>% 
  rename(ED_rentown_fa = ED_rentown,
         ED_woz_fa = ED_woz)

# look at for how many mothers/fathers we don't have a record on this subject 
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_woz_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_woz_fa)) / nrow(birthcohort_join) * 100 
sum(!is.na(pretbirth_residence_woz_sted_dat$ED_woz_mo))/ nrow(birthcohort_join)*100

# join residence data and eigendomwoz data of the parents
pretbirth_residence_woz_dat <- full_join(pretbirth_residence_dat, selection_woz_mo)
pretbirth_residence_woz_dat <- full_join(pretbirth_residence_woz_dat, selection_woz_fa)

# save subject data
saveRDS(pretbirth_residence_woz_dat, "case1_preterm_birth/data/prnl/pretbirth_residence_woz_dat.rds")
# remove from environment what you don't need anymore
rm(residence_dat, join_residence_mo, join_residence_fa, selection_residence_mo, selection_residence_fa, pretbirth_residence_dat, 
   selection_woz_mo, selection_woz_fa, eigendomwoz_dat)

#### 1.3 POSTCODE DATA ####
# read CBS microdata file
postcode_dat <- read_sav(file.path(loc$data_folder, loc$postcode_data)) %>% 
  mutate(DATUMAANVPOSTCODENUMADRES = ymd(DATUMAANVPOSTCODENUMADRES),
         DATUMEINDPOSTCODENUMADRES = ymd(DATUMEINDPOSTCODENUMADRES),
         SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
         POSTCODENUM = ifelse(POSTCODENUM == "----", NA, POSTCODENUM), 
         POSTCODENUM = as_factor(POSTCODENUM))

# add classification of stedelijkheid which is available in CBS HULPbestanden
pc4_sted <- read_excel("K:/Utilities/HULPbestanden/PC4/cbs_pc4_2020_v2.xlsx",
                       range = cell_rows(c(9, 11:4078)))
pc4_sted <- pc4_sted[-c(1),] %>% select('Postcode-4', '...134') %>% 
  rename(PC4 = 'Postcode-4',
         STED = '...134') %>% 
  mutate(STED = ifelse(STED == "-99997", NA, STED),
         #PC4 = as.factor(PC4),
         STED = as.factor(STED))

# add corop region classification: we can only link corop by municipality code so we first add the municipality code (gemc)
pc4_gem <- read_sav("K:/Utilities/HULPbestanden/PWR/PWR2020.sav", 
                    col_select = c("postc", "gemc")) %>% # select all variables to join labels
  mutate(postc = as.factor(postc),
         gemc = as.factor(gemc)) # municipality code 

gem_corop <- read_excel("K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx",
                        range = cell_cols("CA:CB")) %>% 
  mutate(GM2020 = as.factor(GM2020), 
         COROP2020 = as.factor(COROP2020)) %>% 
  unique()

# create one dataset with postcode, municipality code and corop region
postcode_dat <- postcode_dat %>% 
  left_join(pc4_sted, by = c("POSTCODENUM" = "PC4")) %>% 
  left_join(pc4_gem, by = c("POSTCODENUM" = "postc")) %>% 
  left_join(gem_corop, by = c("gemc" = "GM2020"))

# some residential places have more than one postcode, we select the most recent postcode record
postcode_dat <- postcode_dat %>% 
  group_by(SOORTOBJECTNUMMER, RINOBJECTNUMMER) %>% 
  slice(which.max(DATUMAANVPOSTCODENUMADRES)) %>% 
  ungroup()
# remove files you don't need anymore
rm(pc4_gem, pc4_sted, gem_corop)

# Join residence_woz_sted data with postcode records by SOORTOBJECTNUMMER/RINOBJECTNUMMER = residence ID 
# for the mother
pretbirth_residence_woz_sted_dat <- pretbirth_residence_woz_dat %>% 
  left_join(postcode_dat, by = c("residence_type_mo" = "SOORTOBJECTNUMMER", "residence_mo" = "RINOBJECTNUMMER")) %>% 
  rename(POSTCODENUM_mo = POSTCODENUM, 
         STED_mo = STED,
         gemc_mo = gemc,
         COROP2020_mo = COROP2020) %>% 
  select(-c(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES))

# for the father
pretbirth_residence_woz_sted_dat <- pretbirth_residence_woz_sted_dat %>% 
  left_join(postcode_dat, by = c("residence_type_fa" = "SOORTOBJECTNUMMER", "residence_fa" = "RINOBJECTNUMMER")) %>% 
  rename(POSTCODENUM_fa = POSTCODENUM, 
         STED_fa = STED,
         gemc_fa = gemc,
         COROP2020_fa = COROP2020) %>% 
  select(-c(DATUMAANVPOSTCODENUMADRES, DATUMEINDPOSTCODENUMADRES))

# look at for how many mothers/fathers we don't have a record on this subject 
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_residence_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_residence_fa)) / nrow(birthcohort_join) * 100 

# create variable to identify if the mother and father are registered at/ live(d) at the same residence
pretbirth_residence_woz_sted_dat <- pretbirth_residence_woz_sted_dat %>% 
  mutate(residence_same_for_parents = ifelse(residence_mo == residence_fa, 1, 0),
         residence_same_for_parents = as.factor(recode(residence_same_for_parents, 
                                                       "1" = "parents same residence", "0" = "parents not same residence")))

# save subject data
#saveRDS(pretbirth_residence_woz_sted_dat, "case1_preterm_birth/data/prnl/pretbirth_residence_woz_sted_dat.rds")


#### 2. education level data ####
# read cbs microdata file
educ_dat <- readRDS("data/parents_prnl/educ_dat_ref.rds")

# join records of the parents
join_educ_mo <- join_conception_mo(educ_dat) %>% 
  select(-c(educationnr_mo, CTO2016V_mo, OPLNIVSOI2016AGG4HB_mo))
join_educ_fa <- join_conception_fa(educ_dat) %>% 
  select(-c(educationnr_fa, CTO2016V_fa, OPLNIVSOI2016AGG4HB_fa))

# filter and select records at doe-moment
selection_educ_mo <- selection_mo(join_educ_mo)
selection_educ_fa <- selection_fa(join_educ_fa)

# look at for how many mothers/fathers we don't have a record on this subject 
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_educ_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_educ_fa)) / nrow(birthcohort_join) * 100 

# combine selected subject data of the mother and father 
pretbirth_educ_dat <- selection(selection_educ_mo, selection_educ_fa)

# save the subject data
#saveRDS(pretbirth_educ_dat, "case1_preterm_birth/data/prnl/pretbirth_educ_dat.rds")
# remove files you don't need anymore 
rm(educ_dat, join_educ_mo, join_educ_fa, selection_educ_mo, selection_educ_fa)

# test I did to see if a doe-moment of one year later would result in more records on education being available - did not matter much 
#test_selection_educ_mo <- join_educ_mo %>% filter((conception + years(1)) %within% interval(start_date_mo, end_date_mo))
#sum(!test_selection_educ_mo$Rinpersoon_Moeder %in% selection_educ_mo$Rinpersoon_Moeder)

#### 3. SOCIO-ECONOMIC DATA ####
# read CBS microdata file
secm_dat <- readRDS("data/parents_prnl/secm_dat.rds")

# join records of the parents
join_secm_mo <- join_conception_mo(secm_dat)
join_secm_fa <- join_conception_fa(secm_dat)

# filter and select records at doe-moment
selection_secm_mo <- selection_mo(join_secm_mo)
selection_secm_fa <- selection_fa(join_secm_fa)

# look at for how many mothers/fathers we don't have a record on this subject 
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_secm_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_secm_fa)) / nrow(birthcohort_join) * 100 

# combine selected subject data of the mother and father 
pretbirth_secm_dat <- selection(selection_secm_mo, selection_secm_fa)

# save the subject data
#saveRDS(pretbirth_secm_dat, "case1_preterm_birth/data/prnl/pretbirth_secm_dat.rds")
rm(secm_dat, join_secm_mo, join_secm_fa, selection_secm_mo, selection_secm_fa)


#### 4. (S)POLIS DATA: JOBS AND WAGES ####
# read CBS microdata file
# polisbus contains data ob jobs and wages from 2006-2009
polis_dat <- readRDS("data/parents_prnl/polis_dat.rds")

# join records of the parents
join_polis_mo <- join_conception_mo(polis_dat)
join_polis_fa <- join_conception_fa(polis_dat)

# filter and select records at doe-moment
selection_polis_mo <- selection_mo(join_polis_mo)
# if the parent has multiple jobs (with wages and paid hours) we select the one with the highest wages*paidhours
selection_polis_mo <- selection_polis_mo %>% 
  group_by(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(Rinpersoon_Kind, Rinpersoon_Moeder, .keep_all = TRUE)

selection_polis_fa <- selection_fa(join_polis_fa)
# if the parent has multiple jobs (with wages and paid hours) we select the one with the highest wages*paidhours
selection_polis_fa <- selection_polis_fa %>% 
  group_by(Rinpersoons_Kind, Rinpersoon_Kind, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(Rinpersoon_Kind, rin_fa, .keep_all = TRUE)

# combine selected subject data of the mother and father 
pretbirth_polis_dat <- selection(selection_polis_mo, selection_polis_fa)

#saveRDS(pretbirth_polis_dat, "case1_preterm_birth/data/prnl/pretbirth_polis_dat.rds")
rm(polis_dat)

# read CBS microdata file
# spolisbus contains data available from 2010 on 
spolis_dat <- readRDS("data/parents_prnl/spolis_dat.rds")

# join records of the parents
join_spolis_mo <- join_conception_mo(spolis_dat)
join_spolis_fa <- join_conception_fa(spolis_dat)

# filter and select records at doe-moment
selection_spolis_mo <- selection_mo(join_spolis_mo)
# if the parent has multiple jobs (with wages and paid hours) we select the one with the highest wages*paidhours
selection_spolis_mo <- selection_spolis_mo %>% 
  group_by(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder) %>% 
  arrange(desc(SPOLIS_wages_mo*SPOLIS_paidhours_mo), .by_group = TRUE) %>% 
  distinct(Rinpersoon_Kind, Rinpersoon_Moeder, .keep_all = TRUE)

selection_spolis_fa <- selection_fa(join_spolis_fa)
# if the parent has multiple jobs (with wages and paid hours) we select the one with the highest wages*paidhours
selection_spolis_fa <- selection_spolis_fa %>% 
  group_by(Rinpersoons_Kind, Rinpersoon_Kind, rins_fa, rin_fa) %>% 
  arrange(desc(SPOLIS_wages_fa*SPOLIS_paidhours_fa), .by_group = TRUE) %>% 
  distinct(Rinpersoon_Kind, rin_fa, .keep_all = TRUE)

# combine selected subject data of the mother and father 
pretbirth_spolis_dat <- selection(selection_spolis_mo, selection_spolis_fa)
#saveRDS(pretbirth_spolis_dat, "case1_preterm_birth/data/prnl/pretbirth_spolis_dat.rds")

# bind polis and spolisbus data of the parents
pretbirth_spolis_complete <- rbind(pretbirth_polis_dat, pretbirth_spolis_dat)

# save the subject data
#saveRDS(pretbirth_spolis_complete, "case1_preterm_birth/data/prnl/pretbirth_spolis_complete.rds")

# look at for how many mothers/fathers we don't have a record on this subject
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - sum(!is.na(pretbirth_spolis_complete$SPOLIS_wages_mo))) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - sum(!is.na(pretbirth_spolis_complete$SPOLIS_wages_fa))) / nrow(birthcohort_join) * 100 

# remove files you no longer need 
rm(spolis_dat, join_polis_mo, join_polis_fa, join_spolis_mo, join_spolis_fa, selection_polis_mo, selection_polis_fa,
   selection_spolis_mo, selection_spolis_fa, pretbirth_polis_dat, pretbirth_spolis_dat)

#### 5. INCOME DATA ####
# read CBS microdata file
income_dat <- readRDS("data/parents_prnl/income_dat.rds")

# join records of the parents
join_income_mo <- join_conception_mo(income_dat)
join_income_fa <- join_conception_fa(income_dat)

# filter and select records at doe-moment
selection_income_mo <- selection_mo(join_income_mo)
selection_income_fa <- selection_fa(join_income_fa)

# look at for how many mothers/fathers we don't have a record on this subject
(sum(!is.na(birthcohort_join$Rinpersoon_Moeder)) - nrow(selection_income_mo)) / nrow(birthcohort_join) * 100 
(sum(!is.na(birthcohort_join$rin_fa)) - nrow(selection_income_fa)) / nrow(birthcohort_join) * 100 

# combine selected subject data of the mother and father 
pretbirth_income_dat <- selection(selection_income_mo, selection_income_fa)

# create a variable for the income of the parents, which adds the income of the mother and the father and gives NA in case both are missing
pretbirth_income_dat <- pretbirth_income_dat %>% 
  mutate(income_parents = ifelse(is.na(income_mo) & is.na(income_fa), NA, rowSums(across(c(income_mo, income_fa)), na.rm = TRUE)))

# save the subject data
#saveRDS(pretbirth_income_dat, "case1_preterm_birth/data/prnl/pretbirth_income_dat.rds")
rm(income_dat, join_income_mo, join_income_fa, selection_income_mo, selection_income_fa)


#### 6. HEALTH DATA ####
# read CBS microdata file
health_dat <- readRDS("data/parents_prnl/health_dat.rds")

# join records of the parents
join_health_mo <- join_conception_mo(health_dat)
join_health_fa <- join_conception_fa(health_dat)

# filter and select records at doe-moment
# select health care costs for t - 1, i.e. the calender year before the conception/doe-moment
selection_health_mo <- join_health_mo %>% 
  filter((conception - years(1)) %within% interval(start_date_mo, end_date_mo))
selection_health_fa <- join_health_fa %>% 
  filter((conception - years(1)) %within% interval(start_date_fa, end_date_fa))

# combine selected subject data of the mother and father 
pretbirth_health_dat <- selection(selection_health_mo, selection_health_fa)

# an option is to look at broader categories of health care costs, therefore we group the health care variables 
pretbirth_health_dat_grouped <- pretbirth_health_dat_grouped %>% 
  mutate(ZVWK_GP_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_GP_other_mo)), na.rm = T),
         ZVWK_GP_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_GP_other_fa)), na.rm = T),
         ZVWK_patient_transport_mo = rowSums(across(c(ZVWK_patient_transport_sit_mo:ZVWK_patient_transport_lie_mo)), na.rm = T),
         ZVWK_patient_transport_fa = rowSums(across(c(ZVWK_patient_transport_sit_fa:ZVWK_patient_transport_lie_fa)), na.rm = T),
         # do not include the birth_maternitycare and birth_obstetrician costs
         ZVWK_sumtotal_mo = rowSums(across(c(ZVWK_GP_basic_mo:ZVWK_patient_transport_lie_mo, ZVWK_abroad_mo:ZVWK_other_mo)), na.rm = TRUE),
         ZVWK_sumtotal_fa = rowSums(across(c(ZVWK_GP_basic_fa:ZVWK_patient_transport_lie_fa, ZVWK_abroad_fa:ZVWK_other_fa)), na.rm = TRUE))

# another option is to create a binary variable of ggz (mental health care) use
# pretbirth_health_dat_grouped <- pretbirth_health_dat_grouped %>% 
#   mutate(ZVWK_ggz_binary_mo = ifelse(ZVWK_mentalhealth_bas_mo > 0 | ZVWK_mentalhealth_spec_mo > 0 | ZVWK_mentalhealth_spec_stay_mo > 0 |
#                                      ZVWK_mentalh_spec_nostay_inst_mo > 0 | ZVWK_mentalh_spec_nostay_ind_mo > 0 | ZVWK_mentalh_spec_other_mo > 0,
#                                      1, 0),
#          ZVWK_ggz_binary_fa = ifelse(ZVWK_mentalhealth_bas_fa > 0 | ZVWK_mentalhealth_spec_fa > 0 | ZVWK_mentalhealth_spec_stay_fa > 0 |
#                                      ZVWK_mentalh_spec_nostay_inst_fa > 0 | ZVWK_mentalh_spec_nostay_ind_fa > 0 | ZVWK_mentalh_spec_other_fa > 0,
#                                      1, 0),
#          ZVWK_ggz_binary_mo = as_factor(ZVWK_ggz_binary_mo),
#          ZVWK_ggz_binary_fa = as_factor(ZVWK_ggz_binary_fa))

# save the subject data
#saveRDS(pretbirth_health_dat, "case1_preterm_birth/data/prnl/pretbirth_health_dat.rds")
#saveRDS(pretbirth_health_dat_grouped, "case1_preterm_birth/data/prnl/pretbirth_health_dat_grouped.rds")

# remove files you no longer need
rm(health_dat, join_health_mo, join_health_fa, selection_health_mo, selection_health_fa, pretbirth_health_dat)


#### 8. HOUSEHOLD TYPE DATA
# read CBS microdata file
gba_hh <- readRDS("data/parents_prnl/gba_hh_dat.rds")

# the household data is not available at the individual (RINPERSOONS/RINPERSOON) level but at the household level 
# the unborn child has no household ID yet, therefore we look at the household of the birth mother 
# we also create a variable to capture whether the birth mother and legal father are in the same household (have the same household ID)

# join records of the mother
join_gba_hh <- join_conception %>% 
  left_join(gba_hh, by = c("Rinpersoons_Moeder" = "RINPERSOONS", "Rinpersoon_Moeder" = "RINPERSOON"))

# filter and select records at doe-moment
pretbirth_gba_hh_dat <- join_gba_hh %>% 
  filter(conception %within% interval(DATUMAANVANGHH, DATUMEINDEHH))

# save the subject data
#saveRDS(pretbirth_gba_hh_dat, "case1_preterm_birth/data/prnl/pretbirth_gba_hh_dat.rds")
rm(gba_hh, join_gba_hh)


#### 9. HOUSEHOLD INCOME DATA ####
# read CBS microdata file
hh_income_dat <- readRDS("data/parents_prnl/hh_income_dat.rds")

# join records of the mother
join_hh_income_mo <- join_conception_mo(hh_income_dat)

# filter and select records at doe-moment
pretbirth_hh_income_dat <- selection_mo(join_hh_income_mo)

pretbirth_hh_income_dat <- pretbirth_hh_income_dat %>%
  select(-c(year_mo)) %>% 
  mutate(l_income_hh_pov_binary_mo = as_factor(l_income_hh_pov_binary_mo, levels = "values"),
         l_income_hh_pov_4j_binary_mo = as_factor(l_income_hh_pov_4j_binary_mo, levels = "values"),
         l_income_hh_min_binary_mo = as_factor(l_income_hh_min_binary_mo, levels = "values"),
         l_income_hh_min_4j_binary_mo = as_factor(l_income_hh_min_4j_binary_mo, levels = "values"),
         l_income_hh_eur_binary_mo = as_factor(l_income_hh_eur_binary_mo, levels = "values"),
         l_income_hh_eur_4j_binary_mo = as_factor(l_income_hh_eur_4j_binary_mo, levels = "values"))

# save the subject data
#saveRDS(pretbirth_hh_income_dat, "case1_preterm_birth/data/prnl/pretbirth_hh_income_dat.rds")
# remove the files you no longer need
rm(hh_income_dat, join_hh_income_mo)

#### 10. HOUSEHOLD VERMOGEN DATA ####
# read CBS microdata file
hh_vermogen_dat <- readRDS("data/parents_prnl/hh_schulden.rds")

# join records of the mother
join_hh_vermogen_mo <- join_conception_mo(hh_vermogen_dat)

# filter and select records at doe-moment
pretbirth_hh_vermogen_dat <- selection_mo(join_hh_vermogen_mo) %>% 
  select(-c(year_mo))
# no data before 2006

# save the subject data
#saveRDS(pretbirth_hh_vermogen_dat, "case1_preterm_birth/data/prnl/pretbirth_hh_vermogen_dat.rds")
#remove the files you no longer need
rm(hh_vermogen_dat, join_hh_vermogen_mo)


#### COMBINE ALL SELECTED DATA ####
# in this step we combine the perined data and the CBS microdata from the aforementioned subjects 

# load the data selected for the case preterm birth if they are no longer in your environment
pretbirth_residence_woz_sted_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_residence_woz_sted_dat.rds") %>% 
  select(-c(start_date_mo, end_date_mo, residence_type_mo, residence_mo, 
            start_date_fa, end_date_fa, residence_type_fa, residence_fa, 
            POSTCODENUM_mo, POSTCODENUM_fa, gemc_mo, gemc_fa))

pretbirth_educ_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_educ_dat.rds") %>% 
  select(-c(start_date_mo, end_date_mo, start_date_fa, end_date_fa))

pretbirth_secm_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_secm_dat.rds") %>% 
  select(-c(start_date_mo, end_date_mo, start_date_fa, end_date_fa))

pretbirth_spolis_complete <- readRDS("case1_preterm_birth/data/prnl/pretbirth_spolis_complete.rds") %>% 
  select(-c(start_date_mo, end_date_mo, start_date_fa, end_date_fa))

pretbirth_income_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_income_dat.rds") %>% 
  select(-c(start_date_mo, end_date_mo, start_date_fa, end_date_fa))

pretbirth_health_dat_grouped <- readRDS("case1_preterm_birth/data/prnl/pretbirth_health_dat_grouped.rds") %>% 
  select(-c(start_date_mo, end_date_mo, start_date_fa, end_date_fa)) %>% 
  # select certain variables 
  select(c(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, rins_fa, rin_fa, ddgeb, conception, 
           ZVWK_GP_mo, ZVWK_pharmacy_mo, ZVWK_dentalcare_mo, ZVWK_hospital_mo, ZVWK_physical_therapy_mo, ZVWK_physical_other_mo,
           ZVWK_appliances_mo, ZVWK_patient_transport_mo, ZVWK_abroad_mo, ZVWK_mentalhealth_bas_mo, ZVWK_mentalhealth_spec_stay_mo,
           ZVWK_mentalh_spec_nostay_inst_mo, ZVWK_mentalh_spec_nostay_ind_mo, ZVWK_mentalh_spec_other_mo, ZVWK_other_mo, ZVWK_sumtotal_mo,
           
           ZVWK_GP_fa, ZVWK_pharmacy_fa, ZVWK_dentalcare_fa, ZVWK_hospital_fa, ZVWK_physical_therapy_fa, ZVWK_physical_other_fa,
           ZVWK_appliances_fa, ZVWK_patient_transport_fa, ZVWK_abroad_fa, ZVWK_mentalhealth_bas_fa, ZVWK_mentalhealth_spec_stay_fa,
           ZVWK_mentalh_spec_nostay_inst_fa, ZVWK_mentalh_spec_nostay_ind_fa, ZVWK_mentalh_spec_other_fa, ZVWK_other_fa, ZVWK_sumtotal_fa))

pretbirth_gba_hh_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_gba_hh_dat.rds") %>% 
  select(-c(DATUMAANVANGHH, DATUMEINDEHH))

pretbirth_hh_income_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_hh_income_dat.rds") %>% 
  select(-c(start_date_mo, end_date_mo))

pretbirth_hh_vermogen_dat <- readRDS("case1_preterm_birth/data/prnl/pretbirth_hh_vermogen_dat.rds") %>% 
  select(-c(rins_hh_mo, rin_hh_mo, start_date_mo, end_date_mo))


# join all the preterm birth data at the time of conception 
pretbirth_dat_atto_conception <- birthcohort_join # readRDS("case1_preterm_birth/data/prnl/birthcohort_join.rds") 

# function to join the data 
join_pretbirth_dat_atto_conception <- function(pretbirth_dat) {
  pretbirth_dat_atto_conception %>% 
    left_join(y = pretbirth_dat,
              by = c("Rinpersoons_Kind" = "Rinpersoons_Kind", "Rinpersoon_Kind" = "Rinpersoon_Kind", 
                     "Rinpersoons_Moeder" = "Rinpersoons_Moeder", "Rinpersoon_Moeder" = "Rinpersoon_Moeder",
                     "rins_fa" = "rins_fa", "rin_fa" = "rin_fa", "ddgeb" = "ddgeb",
                     "conception" = "conception"))
} 
# use the function and select the CBS subject data you want to join to the birthcohort_join dataset 
pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_residence_woz_sted_dat)

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_educ_dat)

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_secm_dat) 

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_spolis_complete)

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_income_dat) 

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_health_dat_grouped)

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_gba_hh_dat) 

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_hh_income_dat)

pretbirth_dat_atto_conception <- join_pretbirth_dat_atto_conception(pretbirth_hh_vermogen_dat) 
# N = 3401872 unique observations

# check the number of missing values per column 
round(colSums(is.na(pretbirth_dat_atto_conception))/nrow(pretbirth_dat_atto_conception)*100, 1)

# save the complete dataset for the case preterm birth 
#saveRDS(pretbirth_dat_atto_conception, "case1_preterm_birth/data/prnl/pretbirth_dat_atto_conception.rds")

#look at the complete birthcohort dataset
glimpse(pretbirth_dat_atto_conception)

# remove the files you no longer need
rm(pretbirth_residence_woz_sted_dat, pretbirth_educ_dat, pretbirth_secm_dat, pretbirth_spolis_complete,
   pretbirth_income_dat, pretbirth_hh_income_dat, pretbirth_gba_hh_dat, pretbirth_hh_vermogen_dat, pretbirth_health_dat_grouped)



#### 2. CREATE AND MUTATE VARIABES IN THE DATA SET ####

# create a new variable for if one of the parents' (or both parents') records are missing
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  mutate(parent_miss = ifelse(Rinpersoons_Moeder == "G" | is.na(rin_fa), 1, 0),
         parent_miss = as_factor(parent_miss))

# restructure household type data to indicate whether the mother has a partner and/or a previous child
# I only did this for the restricted models, not the full models
table(pretbirth_dat_atto_conception$PLHH)
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  mutate(plhh_partner_child = fct_collapse(PLHH, 
                                           partner_met_kind = c(" 5", " 6"),
                                           partner_znd_kind = c(" 3", " 4"),
                                           overig = c(" 8", " 9", "10")),
         plhh_partner_child = recode(plhh_partner_child, " 1" = "thuiswonend_kind",
                                     " 2" = "alleenstd_znd_kind", " 7" = "alleenstd_met_kind"))
table(pretbirth_dat_atto_conception$plhh_partner_child)

# restructure the education data to create a variable with 6 ordered categories
table(pretbirth_dat_atto_conception$educationlevel_fa)
table(pretbirth_dat_atto_conception$educationlevel_mo)
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  # collapse factor levels
  mutate(educationlevel_mo = fct_collapse(educationlevel_mo, basis = c("1111", "1112"), 
                                          vmbo_hg = c("1211", "1212", "1213", "1221", "1222"),
                                          mbo2 = c("2111"),
                                          mbo3_4 = c("2112", "2121"),
                                          havovwo_hg = c("2131", "2132"),
                                          hbo_wo = c("3111", "3112", "3113", "3211", "3212", "3213")),
         educationlevel_fa = fct_collapse(educationlevel_fa, basis = c("1111", "1112"),
                                          vmbo_hg = c("1211", "1212", "1213", "1221", "1222"),
                                          mbo2 = c("2111"),
                                          mbo3_4 = c("2112", "2121"),
                                          havovwo_hg = c("2131", "2132"),
                                          hbo_wo = c("3111", "3112", "3113", "3211", "3212", "3213")),
         educationlevel_mo = ordered(educationlevel_mo),
         educationlevel_fa = ordered(educationlevel_fa))
class(pretbirth_dat_atto_conception$educationlevel_fa)

# restructure the inter-pregnancy interval variable: up until now we kept the categorical variable in the data
# to retain as much information as possible and for multiple imputation
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  mutate(interpreg_cat = ifelse(interpreg == -88, "nvt", # create a nvt (not applicable) category for all first pregnancies 
                                ifelse(interpreg == -99, "geen data", # create a separate category for cases where no data is available
                                       ifelse(interpreg < 0, "geen data", 
                                              ifelse(interpreg < (30.436*6), "<6 months", 
                                                     ifelse(interpreg >= (30.436*6) & interpreg < (30.436*12), "6-12 months", 
                                                            ifelse(interpreg >= (30.436*12) & interpreg < (30.436*18), "12-18 months", 
                                                                   ifelse(interpreg >= (30.436*18) & interpreg < (30.436*24), "18-24 months",
                                                                          ifelse(interpreg >= (30.436*24) & interpreg < (30.436*30), "24-30 months",
                                                                                 ifelse(interpreg >= (30.436*30), ">30 months", NA))))))))),
         interpreg_cat = as_factor(interpreg_cat)) 
table(pretbirth_dat_atto_conception$interpreg_cat)                                
#levels = c("nvt", "<6 months", "6-12 months", "12-18 months", "18-24 months", "24-30 months", ">30 months")))

# combine two categories for amddd1ond, which indicates how far along the pregnancy is at the first consultation/ moment of pregnancy care
# at the 16 week moment, during the consultation, we make our prediction, therefore we do not want to include info we do not know at 16 weeks. 
table(pretbirth_dat_atto_conception$amddd1ond_cat)
pretbirth_dat_atto_conception$amddd1ond_cat <- fct_collapse(pretbirth_dat_atto_conception$amddd1ond_cat, ">113 dagen" = c("113 - 140 dagen", ">140 dagen"))

# potential include year of conception (/registration) to be able to say something about whether there are trends/ whether year is associated with the outcome
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>%
  mutate(year_conception = year(conception), 
         year_conception = as.factor(year_conception))

# save dataset
#saveRDS(pretbirth_dat_atto_conception, "case1_preterm_birth/pretbirth_dat_atto_conception_5feb2024.rds")
pretbirth_dat_atto_conception <- readRDS("case1_preterm_birth/pretbirth_dat_atto_conception_5feb2024.rds")

#################################################################################

#### 3. PREPARE THE DATA SET FOR ANALYSIS: SELECT DATA OF INTEREST

# look at the proportion of preterm births in the full dataset for 
# all births (singleton and multiple) between 1999 and 2020
# preterm birth < 32 weeks
table(pretbirth_dat_atto_conception$premature_birth)
round(prop.table(table(pretbirth_dat_atto_conception$premature_birth))*100, digits = 2)
# 1.05% of the children in the cohort are born extremely prematurely

# preterm birth < 37 weeks
table(pretbirth_dat_atto_conception$premature_birth37)
round(prop.table(table(pretbirth_dat_atto_conception$premature_birth37))*100, digits = 2)
# 7.29% of the children in the cohort are born prematurely 


# look more closely at the parent data
# there are 7478 records where there is not rinpersoon number for the mother, so no data is available
sum(pretbirth_dat_atto_conception$Rinpersoons_Moeder == "G")
# there are 121954 records without a recorded father
sum(is.na(pretbirth_dat_atto_conception$rin_fa))
# in 7190 cases there is no rinpersoon number for the mother but there is a record for the father
sum(pretbirth_dat_atto_conception$Rinpersoons_Moeder == "G" & !is.na(pretbirth_dat_atto_conception$rin_fa))
# in 288 cases both the father and mother are missing
sum(pretbirth_dat_atto_conception$Rinpersoons_Moeder == "G" & is.na(pretbirth_dat_atto_conception$rin_fa))


# in this analysis, we include only data from 2010 on because of the many changes in health care practice between 1999-2009 and 2010-2020
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  filter(jaar >= 2010)

# omv_cat reflects the number of babies in the pregnancy 
# in this analysis, we exclude multiples (as discussed with our birth care professionals) 
table(pretbirth_dat_atto_conception$omv_cat)
pretbirth_dat_atto_conception <- pretbirth_dat_atto_conception %>% 
  filter(omv_cat == 1)

# look at the proportion of preterm births in the data 
# preterm birth < 32 weeks
table(pretbirth_dat_atto_conception$premature_birth)
round(prop.table(table(pretbirth_dat_atto_conception$premature_birth))*100, digits = 2)
# 0.82% of the children in the cohort are born extremely prematurely

# preterm birth < 37 weeks
table(pretbirth_dat_atto_conception$premature_birth37)
round(prop.table(table(pretbirth_dat_atto_conception$premature_birth37))*100, digits = 2)
# 5.48% of the children in the cohort are born prematurely 

# save the pretbirth_dat_atto_conception dataset after filtering to include only singeltons and the years 2010-2020
saveRDS(pretbirth_dat_atto_conception, "case1_preterm_birth/PTB_data/pretbirth_dat_atto_conception_singelton_20102020.rds")

### Specify predictors: we will use these predictors in the analysis 

# 1. FULL MODEL
# this model includes all  available predictors from the Perined and CBS data
# outcome: PTB <32w
predictors_full_model32 <- pretbirth_dat_atto_conception %>%
  # un-select variables not included in analysis          
  select(-c(# disregard id variables
            Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, 
            ddgeb, rins_fa, rin_fa, birthday_mo, birthday_fa, rins_hh_mo, rin_hh_mo,
            pc4, pc6, lft_concep, year_conception,
            # disregard variables used to create the outcome variable preterm birth
            amww, conception,
            # disregard unreliable/ low quality PRNL origin variables
            etnic,
            # disregard grouped variables, we use GBA_origin_ACHTS and GBA_origin_ETNG instead
            GBA_origin_mo, GBA_origin_fa,
            # disregard birth variables (information we do not have at time of prediction)
            sectio_ia, geboortegew, overdracht, pediater, verantw_bb, verantw_eb, gesl,
            verantw_zw, partus, omv_cat, episiotomie, gebplaats, ligging, nicuopname, pijnbestrijding2,
            robson, ruptuur, congenafw_ja, fluxus, laag_geb_gewicht, par, 
            # par_cat, # will be included to create the primi/multi sample but will not be included in analysis
            vroeg_geb_24_37, vroeg_geb_24_28, vroeg_geb_28_34, vroeg_geb_34_37,
            # disregard omv because only omv = 1 is included in this analysis
            omv, omv_cat,
            # use categorical variables instead 
            interpreg, grav, amddd1ond,
            # disregard this variable created for the restricted model, the original variable (PLHH) is included in the full model
            plhh_partner_child,
            # !keep only the PTB outcome variable you're using
            premature_birth37)) # in case the outcome variable is PTB <37w, exclude premature_birth

saveRDS(predictors_full_model32, "case1_preterm_birth/PTB_data/predictors_full_model32.rds")

# this model includes all  available predictors from the Perined and CBS data
# outcome: PTB <37w
predictors_full_model37 <- pretbirth_dat_atto_conception %>%
  # un-select variables not included in analysis  
  select(-c(# disregard id variables
            Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, 
            ddgeb, rins_fa, rin_fa, birthday_mo, birthday_fa, rins_hh_mo, rin_hh_mo,
            pc4, pc6, lft_concep, year_conception, 
            # disregard variables used to create the outcome variable preterm birth
            amww, conception,
            # disregard unreliable/ low quality PRNL origin variables
            etnic,
            # disregard grouped variables, we use GBA_origin_ACHTS and GBA_origin_ETNG instead
            GBA_origin_mo, GBA_origin_fa,
            # disregard birth variables (information we do not have at time of prediction)
            sectio_ia, geboortegew, overdracht, pediater, verantw_bb, verantw_eb, gesl,
            verantw_zw, partus, omv_cat, episiotomie, gebplaats, ligging, nicuopname, pijnbestrijding2,
            robson, ruptuur, congenafw_ja, fluxus, laag_geb_gewicht, par,
            # par_cat, # will be included to create the primi/multi sample but will not be included in analysis
            vroeg_geb_24_37, vroeg_geb_24_28, vroeg_geb_28_34, vroeg_geb_34_37,
            # disregard omv because only omv = 1 is included in this analysis
            omv, omv_cat,
            # use categorical variables instead 
            interpreg, grav, amddd1ond,
            # disregard this variable created for the restricted model, the original variable (PLHH) is included in the full model
            plhh_partner_child,
            # !keep only the PTB outcome variable you're using
            premature_birth)) # in case the outcome variable is PTB <32w, exclude premature_birth37

saveRDS(predictors_full_model37, "case1_preterm_birth/PTB_data/ptb_37weeks/predictors_full_model37.rds")


# 2. RESTRICTED MODEL
# this model includes a restricted set of variables; the restricted model includes only variables that are available 
# in a clinical, health care setting (EHR, and are currently registered by Perined) or can reasonably be collected in this setting

# DELETE LATER
# in this analysis, we exclude mothers for whom we have (almost) no information (cannot be linked to CBS data) and for whom we have no birth history outcomes
# these pregnancies are excluded 1 for practical reasons, to facilitate multiple imputation, and 
# 2 it might be relevant to look at this group separately and ask ourselves why little information about these individuals is available
# pretbirth_dat_atto_conception_rest <- pretbirth_dat_atto_conception %>% 
#   filter(Rinpersoons_Moeder != "G") # & vooraf_zw_vroeg_24_37 != "geen data") 
# nrow(pretbirth_dat_atto_conception) - nrow(pretbirth_dat_atto_conception_rest)


predictors_rest_model <- pretbirth_dat_atto_conception %>%
  # select variables included in analysis 
  select(c(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, rins_fa, rin_fa,
           premature_birth, premature_birth37,
           N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37, 
           vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
           # disregard the vroeg_24_37 variables; we'll use this predictors set for multiple imputation and recreate these variables after
           #N_vroeg_24_37, vooraf_zw_vroeg_24_37
           N_vooraf_sga, vooraf_sga, gesl,
           interpreg, amddd1ond, grav,
           lft_concep, age_at_concp_fa, # instead age_at_concp_mo_cat, age_at_concp_fa_cat,
           gesl, COROP2020_mo, STED_mo, educationlevel_mo, educationlevel_fa, 
           house_ownership_mo, plhh_partner_child, 
           income_hh_mo, # we made the choice to include household income of the mother (instead of personal income) as household income might have more of an effect on preterm birth 
           year_conception, # use this for income categorisation after MI
           par # use this for checking data validity after MI
           ))

saveRDS(predictors_rest_model, "case1_preterm_birth/PTB_data/predictors_rest_model.rds")
