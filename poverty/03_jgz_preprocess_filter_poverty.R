#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and (05_jgz_preprocess_MI) we prepare the JGZ data for analysis in 05_jgz_prediction.R

#### 03_jgz_preprocess_filter.R ####
# THE GOAL OF THIS SCRIPT IS TO FILTER THE RELEVANT RECORDS FOR THE CASE 

# what records and health and developmental observations are we interested in for this specific case 
# for each case we selected a 'doe-moment':  a consultation moment during which the child is in a specific age range   
# during which we make the prediction and during which there may be an intervention 
# additionally for each case we select an 'outcome-age': age (moment in the future) for which we predict 

# load the necessary libraries 
library(tidyverse)
library(haven)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(AGD)
setwd("H:/Mirthe/")

# read the JGZ data set
jgz_data <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_p.rds")
# and the non-changing information set
jgz_data_demo <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_demo_p.rds")


#### THE CASE: POVERTY (NL: armoede) ####
# 'doe-moment': consultation moment during which the child is approx. two years of age
# 'outcome-age': consultation moment during which the child is approx. two years of age
# in this case, we do not predict a future outcome, we use the model to identify children in a situation (at risk) of poverty

# poverty definition: Huishoudens inkomen ten opzichte van de lage inkomensgrens in het verslagjaar (<110%)
# this is the variable l_income_hh_pov_binary (or for previous 4 years l_income_hh_pov_4j_binary) available from 2005 on 

#### 1. Subset the observations for the poverty case and the contactmomenten ####
# and have a look at which van Wiechen items are observed at which contactmomenten 


# load outcome data - poverty 
hh_income_dat_jgz <- readRDS("H:/Mirthe/data/parents_jgz/hh_income_dat_jgz.rds") %>% 
  mutate(l_income_hh_pov_binary  = as.factor(l_income_hh_pov_binary),
         l_income_hh_pov_4j_binary = as.factor(l_income_hh_pov_4j_binary),
         l_income_hh_min_binary = as.factor(l_income_hh_min_binary),
         l_income_hh_min_4j_binary = as.factor(l_income_hh_min_4j_binary),
         l_income_hh_eur_binary = as.factor(l_income_hh_eur_binary),
         l_income_hh_eur_4j_binary = as.factor(l_income_hh_eur_4j_binary))


table(hh_income_dat_jgz$l_income_hh_pov_binary)
prop.table(table(hh_income_dat_jgz$l_income_hh_pov_binary))*100
table(hh_income_dat_jgz$l_income_hh_pov_4j_binary)
prop.table(table(hh_income_dat_jgz$l_income_hh_pov_4j_binary))*100

# select the non-changing, i.e. demographic, records of the child 
jr2_armoede <- jgz_data_demo %>% 
  rename(PRNL_year = year) %>% 
  left_join(hh_income_dat_jgz, by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"),
            relationship = "many-to-many")

# keep only records for the doe-moment: 2 years of age
jr2_armoede <- jr2_armoede %>% 
  filter(year(doe_moment_2y) == year)

# drop records where outcome variable is not available
sum(is.na((jr2_armoede$l_income_hh_pov_binary)))
jr2_armoede <- jr2_armoede %>% drop_na(l_income_hh_pov_binary)

# add JGZ records of children available at 2 years of age (prior records not available for the majority of the children )
# weight and length related records
jgz_data_BMI <- readRDS("case_jgz/data_outcome/overweight/jgz_data_BMI_711.rds") %>% 
  select(c(RINPERSOONS, RINPERSOON, lft_jaren_2yrs, postcode_2yrs, bmi_2yrs_znl, lengte_2yrs_znl, gewicht_2yrs_znl)) %>% 
  mutate(RINPERSOONS = as.factor(RINPERSOONS), 
         postcode_2yrs = as.factor(postcode_2yrs)) %>% 
  drop_na(bmi_2yrs_znl)

jr2_armoede <- jr2_armoede %>% left_join(jgz_data_BMI, by = c("RINPERSOONS", "RINPERSOON")) 

# language development records
wiechen_cm_2years <- readRDS("case_jgz/data_outcome/wiechen/wiechen_cm_2years_623.rds") %>% 
  mutate(RINPERSOONS = as.factor(RINPERSOONS),
         bst_nl = as_factor(ifelse(!is.na(d_bst_nl_2) & d_bst_nl_2 == 1, "langzaam", 
                                   ifelse(d_bst_nl_1 == 1, "adequaat of sneller", NA))),
         taalomg = ordered(ifelse(!is.na(d_taalomg_3) & d_taalomg_3 == 1, "onvoldoende", 
                                  ifelse(!is.na(d_taalomg_2) & d_taalomg_2 == 1, "matig",
                                         ifelse(!is.na(d_taalomg_1) & d_taalomg_1 == 1, "voldoende", NA))), 
                           levels = c("onvoldoende", "matig", "voldoende")),
         meertaal = as_factor(ifelse(!is.na(d_meertaal_2) & d_meertaal_2 == 1 | !is.na(d_meertaal_3) & d_meertaal_3 == 1, "meertalig",
                                     ifelse(!is.na(d_meertaal_1) & d_meertaal_1 == 1, "niet meertalig", NA)))) %>% 
  select(-c(d_bst_nl_1, d_bst_nl_2, d_taalomg_1, d_taalomg_2, d_taalomg_3, d_meertaal_1, d_meertaal_2, d_meertaal_3)) %>% 
  rename(postcode_2yw = postcode)

jr2_armoede <- jr2_armoede %>% left_join(wiechen_cm_2years, by = c("RINPERSOONS", "RINPERSOON")) 


jr2_armoede <- jr2_armoede %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  # some children have multiple records for postcode (potentially because they moved over the years)
  # in that case we select the first record so that we have one record per child
  slice_head() %>%  # the assumption is that records closer to the doe-moment (2 years) more accurately reflect the kids environment
  ungroup()

jr2_armoede$RINPERSOON[duplicated(jr2_armoede$RINPERSOON)]
# there are no longer children in the dataset with double records

saveRDS(jr2_armoede, "case_jgz/repo/poverty/data/jr2_armoede_p.rds")

# now that we have filtered the case-relevant JGZ data
# we can move to the next step: 04_jgz_preprocess_join_cbs
