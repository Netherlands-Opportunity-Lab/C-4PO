#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

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
setwd("H:/Mirthe/") # ! CHANGE PATH LATER

# read the JGZ data set
#jgz_data <- readRDS("case_jgz/jgz_data_p.rds")
#currently at this path: 
jgz_data <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_p.rds")
# and the non-changing information set
#jgz_data_demo <- readRDS("case_jgz/jgz_data_demo_p.rds")
jgz_data_demo <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_demo_p.rds")


#### THE CASE: LANGUAGE DEVELOPMENT (NL: SPRAAK- EN TAALONTWIKKELING) ####
# 'doe-moment': consultation moment during which the child is approx. two years of age
# 'outcome-age': consultation moment during which the child is approx. 3 years and 9 months of age

#### 1. Subset the observations for the language development case and the contactmomenten ####
# and have a look at which van Wiechen items are observed at which contactmomenten 
jgz_data_wiechen <- jgz_data %>% 
  select(c(RINPERSOONS, RINPERSOON, leeftijd, zin2w, pop6, noemt, wijstaan, zin3w,
           verstaan1, spontaan, vragen1, verstaan2, vragen2, d_bst_nl_1, d_bst_nl_2,
           d_taalomg_1, d_taalomg_2, d_taalomg_3, d_meertaal_1, d_meertaal_2, d_meertaal_3,
           starts_with("cm_"), postcode))

# van Wiechen items 48, 49 and 50 are recorded at approx. 3 years and 9 months,
# we will use these items to define our outcome; language development
# van Wiechen items 41 and 42 are recorded when children are 2 years of age. Since 2 years of age is
# our doe-moment for this case, we will use these observations in our prediction 
# (van Wiechen items 37, 38, 39 and 40 are not available in this dataset)

# remove all rows where all van wiechen items are NA
jgz_data_wiechen <- jgz_data_wiechen %>% filter(!is.na(zin2w) | !is.na(pop6) | !is.na(noemt) | !is.na(wijstaan) | !is.na(zin3w) | !is.na(verstaan1)
                                                | !is.na(spontaan) | !is.na(vragen1) | !is.na(verstaan2) | !is.na(vragen2))

# within the consultation moment durin which the child is approx. 2 years of age: cm_2years
# look at the availability of the records, what is the percentage of missings in the data?
cm_2years <- jgz_data %>% filter(cm_2years == 1)
sum(is.na(cm_2years$r_gd_vve))/nrow(cm_2years)*100 # reden geen vve
sum(is.na(cm_2years$d_indi_12))/nrow(cm_2years)*100 # indicatie spraak-taalontwikkeling
table(cm_2years$d_indi_12)
sum(is.na(cm_2years$d_verwijs_4))/nrow(cm_2years)*100 # logopedist verwijzing
sum(is.na(cm_2years$d_verwijs_11))/nrow(cm_2years)*100 # VVE verwijzing
sum(is.na(cm_2years$d_bst_nl_1))/nrow(cm_2years)*100 #  beoordeling spraak-taal leeftijdsadequaat of sneller
sum(is.na(cm_2years$d_bst_nl_2))/nrow(cm_2years)*100 #  beoordeling spraak-taal langzamer
sum(is.na(cm_2years$d_taalomg_1))/nrow(cm_2years)*100 #  taalomgeving stimulerend, voldoende
sum(is.na(cm_2years$d_taalomg_2))/nrow(cm_2years)*100 #  taalomgeving stimulerend, matig
sum(is.na(cm_2years$d_taalomg_3))/nrow(cm_2years)*100 #  taalomgeving stimulerend, onvoldoende
sum(is.na(cm_2years$d_meertaal_1))/nrow(cm_2years)*100 #  meertaligheid geen
sum(is.na(cm_2years$d_meertaal_2))/nrow(cm_2years)*100 #  meertaligheid simultaan
sum(is.na(cm_2years$d_meertaal_3))/nrow(cm_2years)*100 #  meertaligheid succesieve
sum(is.na(cm_2years$d_d_vvenum_1))/nrow(cm_2years)*100 #  deelname vve ja
sum(is.na(cm_2years$d_d_vvenum_2))/nrow(cm_2years)*100 #  deelname vve nee

#### 1.1 Doe-moment: 2 years of age ###
# According to the guidelines for monitoring language development, van Wiechen items 41 zin2w and 42 pop6 are
# recorded at approx. 2 years of age. We filter observations for 41 and 42 not just at consultation moment cm_2years,
# but also at cm_18months and cm_14months as language development is sometimes assessed at an earlier age (we don't consider observations recorded at a later age because the child is then more developed)
wiechen_cm_2years <- jgz_data_wiechen %>% filter((!is.na(zin2w) | !is.na(pop6)) & (cm_2years == 1 | cm_18months == 1 | cm_14months == 1)) 

# if there are multiple records for a child we select the record closest to the doe-moment 2 years
# first for van wiechen item 41
wiechen_cm_2years_41 <- wiechen_cm_2years %>% 
  mutate(dist_2y = abs(730 - leeftijd)) %>%  #730 days if 2 years
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(!is.na(zin2w)) %>% 
  slice_min(dist_2y) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd) %>%                #doing both steps takes longer
  ungroup() %>% 
  select(c(RINPERSOONS, RINPERSOON, zin2w, postcode, d_bst_nl_1, d_bst_nl_2,
           d_taalomg_1, d_taalomg_2, d_taalomg_3, d_meertaal_1, d_meertaal_2, d_meertaal_3))
# second for van wiehcen item 42
wiechen_cm_2years_42 <- wiechen_cm_2years %>% 
  mutate(dist_2y = abs(730 - leeftijd)) %>%  #730 days if 2 years
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(!is.na(pop6)) %>% 
  slice_min(dist_2y) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd) %>%                #doing both steps takes longer
  ungroup() %>% 
  select(c(RINPERSOONS, RINPERSOON, pop6, postcode, d_bst_nl_1, d_bst_nl_2,
           d_taalomg_1, d_taalomg_2, d_taalomg_3, d_meertaal_1, d_meertaal_2, d_meertaal_3))

# join the selected records
wiechen_cm_2years <- full_join(wiechen_cm_2years_41, wiechen_cm_2years_42, 
                               by = join_by(RINPERSOONS, RINPERSOON, postcode, d_bst_nl_1, d_bst_nl_2, d_taalomg_1, d_taalomg_2, d_taalomg_3,
                                            d_meertaal_1, d_meertaal_2, d_meertaal_3)) 

wiechen_cm_2years$RINPERSOON[duplicated(wiechen_cm_2years$RINPERSOON)]
# there are still a few children who have a double record, to make sure we have one record per child we:
wiechen_cm_2years <- wiechen_cm_2years %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  fill(names(wiechen_cm_2years), .direction = "updown") %>% # make sure both records contain all information
  distinct() %>% # if the records are the same we keep one
  slice_tail() %>% # if the records are not the same (different postcode) we select latest record (likely more recent postcode)
  ungroup()

# create the variable taal ontwikkeling at 2 years, if one of the van wiechen items is "-" insufficient
wiechen_cm_2years <- wiechen_cm_2years %>% 
  mutate(taal_ont_2y = ifelse(zin2w == "-" | pop6 == "-", 1, 0),
         taal_ont_2y = as_factor(taal_ont_2y))
# look at the created taal ontwikkeling variable
round(sum(is.na(wiechen_cm_2years$taal_ont_2y))/nrow(wiechen_cm_2years)*100, 2)
table(wiechen_cm_2years$taal_ont_2y)
round(prop.table(table(wiechen_cm_2years$taal_ont_2y))*100, 2)

#### 1.2 Outcome-age: 3 years 9 months of age ####
# According to the guidelines for monitoring language development, van Wiechen items 48 vragen1, 49 verstaan2 and 50 vragen2 are
# recorded at approx. 3 years 9 months of age. We filter observations for 48, 49 and 50 not just at consultation moment 
# cm_3years9m but also at cm_3years and cm_2years, as language development is sometimes assessed at an earlier age (we don't consider observations recorded at a later age because the child is then more developed)
wiechen_cm_3years9m <- jgz_data_wiechen %>% filter((!is.na(vragen1) | !is.na(verstaan2) | !is.na(vragen2)) 
                                                   & (cm_3years9m == 1 | cm_3years == 1 | cm_2years == 1)) 

# if there are multiple records for a child we select the record closest to the outcome-age 3 years 9 months
# first for van wiechen item 48
wiechen_cm_3years9m_48 <- wiechen_cm_3years9m %>%
  mutate(dist_3y9m = abs(1368 - leeftijd)) %>%   # 1368 days is 3 years and 9 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(!is.na(vragen1)) %>% 
  slice_min(dist_3y9m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd) %>%                #doing both steps takes longer
  ungroup() %>% 
  select(c(RINPERSOONS, RINPERSOON, vragen1))

# second for van wiechen item 49
wiechen_cm_3years9m_49 <- wiechen_cm_3years9m %>% 
  mutate(dist_3y9m = abs(1368 - leeftijd)) %>%   # 1368 days is 3 years and 9 months
  group_by(RINPERSOONS, RINPERSOON) %>%
  filter(!is.na(verstaan2)) %>% 
  slice_min(dist_3y9m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd) %>%                #doing both steps takes longer
  ungroup() %>% 
  select(c(RINPERSOONS, RINPERSOON, verstaan2))

# third for van wiechen item 50
wiechen_cm_3years9m_50 <- wiechen_cm_3years9m %>% 
  mutate(dist_3y9m = abs(1368 - leeftijd)) %>%   # 1368 days is 3 years and 9 months
  group_by(RINPERSOONS, RINPERSOON) %>% 
  filter(!is.na(vragen2)) %>% 
  slice_min(dist_3y9m) %>% 
  group_by(RINPERSOONS, RINPERSOON) %>%  #step 2 is only necessary if there are still duplicates after step 1
  slice_max(leeftijd) %>%                #doing both steps takes longer
  ungroup() %>% 
  select(c(RINPERSOONS, RINPERSOON, vragen2))

# join the selected records
wiechen_cm_3years9m <- wiechen_cm_3years9m_48 %>% 
  full_join(wiechen_cm_3years9m_49, by = join_by(RINPERSOONS, RINPERSOON)) %>% 
  full_join(wiechen_cm_3years9m_50, by = join_by(RINPERSOONS, RINPERSOON))

# look at the variables used to create the outcome; delayed language development 
sum(is.na(wiechen_cm_3years9m$vragen1))
sum(is.na(wiechen_cm_3years9m$verstaan2))
sum(is.na(wiechen_cm_3years9m$vragen2))
# create the outcome variable delayed language development: taal_ont3y9m 
# if one of the van wiechen items is "-" insufficient we define this as delayed language development
wiechen_cm_3years9m <- wiechen_cm_3years9m %>% 
  mutate(taal_ont3y9m = ifelse(vragen1 == "-" | verstaan2 == "-" | vragen2 == "-", 1, 0),
         taal_ont3y9m = as_factor(taal_ont3y9m))
# look at the created outcome variable
round(sum(is.na(wiechen_cm_3years9m$taal_ont3y9m))/nrow(wiechen_cm_3years9m)*100, 2)
table(wiechen_cm_3years9m$taal_ont3y9m)
round(prop.table(table(wiechen_cm_3years9m$taal_ont3y9m))*100, 2)

#### 1.3 Combine records for doe-moment and outcome-age with non-changing records ####
jgz_data_wiechen_2y <- wiechen_cm_3years9m %>% 
  left_join(wiechen_cm_2years, by = join_by(RINPERSOONS, RINPERSOON))
# remove no longer needed files
rm(wiechen_cm_2years_41, wiechen_cm_2years_42, wiechen_cm_3years9m_48, wiechen_cm_3years9m_49, wiechen_cm_3years9m_50)

# create a data set with 1) non-changing records and 2) records monitoring language development at case-specific consultation moments
jr3maand9_wiechen <- jgz_data_wiechen_2y %>% left_join(jgz_data_demo, by = c("RINPERSOONS", "RINPERSOON")) 

# after the join there are multiple records for some children because of different records in the demo data
jr3maand9_wiechen$RINPERSOON[duplicated(jr3maand9_wiechen$RINPERSOON)]
jr3maand9_wiechen <- jr3maand9_wiechen %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  # some children have multiple records for postcode (potentially because they moved over the years)
  # we select the postcode recorded at the doe-moment 2 years if this record is available
  filter(postcode.x == postcode.y | is.na(postcode.x)) %>% 
  # other children have records at multiple JGZ organisations or opl1/opl2 of the parent changes 
  # in that case we select the latest record so that we have one record per child
  slice_tail() %>%  # the assumption is that more recent records more accurately reflect the kids environment
  ungroup()



#### 2. Add other JGZ records on growth and development of the child ####
#Note: Growth between birth and do-moment could be predictive of the outcome as well. 
#However, data on legnth, weight and bmi before age of 2 in our dataset was largerly missing and thus not included after careful exploration"
jgz_data_BMI <- jgz_data_BMI %>% 
  mutate(postcode_2yrs = as_factor(postcode_2yrs))

# Create a data set with the language development records and growth records of these children
jr3maand9_wiechen <- jr3maand9_wiechen %>% 
  left_join(jgz_data_BMI, by = c("RINPERSOONS", "RINPERSOON", "postcode.y" = "postcode_2yrs"))


# almost all observations for length, weight, BMI at earlier contactmomenten (prior to 2 years) are missing
sum(is.na(jr3maand9_wiechen$BMI_4wks))/nrow(jr3maand9_wiechen)*100
sum(is.na(jr3maand9_wiechen$BMI_8wks))/nrow(jr3maand9_wiechen)*100
sum(is.na(jr3maand9_wiechen$BMI_3months))/nrow(jr3maand9_wiechen)*100
sum(is.na(jr3maand9_wiechen$BMI_4months))/nrow(jr3maand9_wiechen)*100
sum(is.na(jr3maand9_wiechen$BMI_2yrs))/nrow(jr3maand9_wiechen)*100
#therefore we decide to remove these variables and their z-scores
jr3maand9_wiechen <- jr3maand9_wiechen %>% select(-c(lengte_4wks, lengte_8wks, lengte_3months, lengte_4months, 
                                                     gewicht_4wks, gewicht_8wks, gewicht_3months, gewicht_4months,
                                                     BMI_4wks, BMI_8wks, BMI_3months, BMI_4months,
                                                     lengte_4wks_znl, lengte_8wks_znl, lengte_3mnd_znl, lengte_4mnd_znl,
                                                     gewicht_4wks_znl, gewicht_8wks_znl, gewicht_3mnd_znl, gewicht_4mnd_znl,
                                                     bmi_4wks_znl, bmi_8wks_znl, bmi_3mnd_znl, bmi_4mnd_znl, 
                                                     lft_jaren_4wks, lft_jaren_8wks, lft_jaren_3months, lft_jaren_4months,
                                                     postcode_4wks, postcode_8wks, postcode_3months, postcode_4months,
                                                     # we also remove observations after the doe-moment 2 years
                                                     lengte_3yrs, lengte_3yrs_znl, lengte_3yrs9m, lengte_3yrs9m_znl, 
                                                     gewicht_3yrs, gewicht_3yrs_znl, gewicht_3yrs9m, gewicht_3yrs9m_znl, 
                                                     BMI_3yrs, bmi_3yrs_znl, BMI_3yrs9m, bmi_3yrs9m_znl,
                                                     lft_jaren_3yrs, lft_jaren_3yrs9m, postcode_3yrs, postcode_3yrs9m)) 

# save the created data set 
saveRDS(jr3maand9_wiechen, "case_jgz/data_outcome/wiechen/jr3maand9_wiechen707_p.rds")
#saveRDS(wiechen_cm_2years, "case_jgz/data_outcome/wiechen/wiechen_cm_2years_623.rds")
# remove files no longer needed
rm(jgz_data_wiechen, wiechen_cm_2years, wiechen_cm_3years9m, jgz_data_wiechen_2y)

#wiechen_cm_2years <- readRDS("case_jgz/data_outcome/wiechen/wiechen_cm_2years_623.rds")

# now that we have filtered the case-relevant JGZ data
# we can move to the next step: 04_jgz_preprocess_join_cbs
