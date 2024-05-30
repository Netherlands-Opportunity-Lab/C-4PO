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
jgz_data <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_p.rds")
# and the non-changing information set
jgz_data_demo <- readRDS("case_jgz/jgz_data_preprocess/jgz_data_demo_p.rds")


#### THE CASE: OVERWEIGHT (NL: OVERGEWICHT) ####
# 'doe-moment': consultation moment during which the child is approx. 4 months old
# 'outcome-age': consultation moment during which the child is approx. 3 years and 9 months of age

#### 1. Subset the observations for the overweight case and the contactmomenten ####
### add BMI measurements at previous contact moments to the data set
# first create a subset with the variables needed 
jgz_data_BMI <- jgz_data %>%
  mutate(indr_gewicht = as_factor(ifelse(!is.na(d_i_jgz_1) & d_i_jgz_1 == 1, 1, # ondergewicht
                                         ifelse(!is.na(d_i_jgz_2) & d_i_jgz_2 == 1, 2, # normaal gewicht
                                                ifelse(!is.na(d_i_jgz_3) & d_i_jgz_3 == 1, 3, NA))))) %>% # overgewicht
  select(c("RINPERSOONS", "RINPERSOON", "leeftijd", "lft_jaren", "sex_z", "sub_ethn",
           "lengte", "gewicht", "BMI", "cm_4wks", "cm_8wks", "cm_3months", "cm_4months",
           "cm_2years", "cm_3years", "cm_3years9m", "postcode", "indr_gewicht", "d_indi_9")) %>% 
  # convert numeric variables back to numeric because pivot wider does not know what to do with non-numeric double observations
  mutate(postcode = as.numeric(as.character(postcode)),
         indr_gewicht = as.numeric(indr_gewicht),
         d_indi_9 = as.numeric(as.character(d_indi_9)))

# look at other potentially interesting variables
sum(!is.na(jgz_data$d_indi_9))/nrow(jgz_data)*100 # indicatie gewicht gewicht, largely missing at doe-moment and previous cm
sum(!is.na(jgz_data$indr_gewicht))/nrow(jgz_data)*100 # indruk gewicht, largely missing at doe-moment and previous cm
sum(!is.na(jgz_data$d_verwijs_3))/nrow(jgz_data)*100 # verwijs kinderfysio, almost 100% missing
sum(!is.na(jgz_data$d_verwijs_14))/nrow(jgz_data)*100 # peutergym verwijzing, almost 100% missing
sum(!is.na(jgz_data$d_verwijs_21))/nrow(jgz_data)*100 # dietist verwijzing, almost 100% missing
# unfortunately variables that are almost 100% missing cannot be included

# create a new column with all contact moment indicators in one
# we need all contact moments before our doe-moment at 4 months of age
jgz_data_BMI <- jgz_data_BMI %>% 
  mutate(cm_ind = ifelse(cm_3years9m == 1, "3yrs9m", 
                         ifelse(cm_4wks == 1, "4wks", 
                                ifelse(cm_8wks == 1, "8wks", 
                                       ifelse(cm_3months == 1, "3months", 
                                              ifelse(cm_4months == 1, "4months",
                                                     ifelse(cm_2years == 1, "2yrs", 
                                                            ifelse(cm_3years == 1, "3yrs", NA)))))))) %>% 
  select(-c("cm_3years9m", "cm_4wks", "cm_8wks", "cm_3months", "cm_4months", "cm_2years", "cm_3years", "leeftijd")) %>% 
  drop_na(cm_ind)

# drop records where BMI is not available
jgz_data_BMI <- jgz_data_BMI %>% drop_na(BMI)

table(jgz_data_BMI$cm_ind)
# change from long format into wide format, with an individuals observations in one row 
jgz_data_BMI <- jgz_data_BMI %>% 
  pivot_wider(names_from = cm_ind,
              values_from = c(BMI, lengte, gewicht, lft_jaren, indr_gewicht, d_indi_9, postcode),
              #if there is more than one observation at a cm for one person we take the mean
              values_fn = ~mean(.x, na.rm = T), 
              names_repair = "unique")

# ! QUESTION TO POLINA: this is the best solution I had for one there is more than one observation after the pivot 
# ! pivot does not work with double categorical observations and there is no function ~ to select a certain category in the case of multiple categories
# ! Do you have another way of doing this? 
# indr_gewicht_ is a categorical variable so we create and label the categories 
jgz_data_BMI <- jgz_data_BMI %>% 
  mutate(across(starts_with("indr_gewicht_"), ~ ifelse(.x < 2, 1, 
                                                       ifelse(.x == 2, 2,
                                                              ifelse(.x > 2, 3, NA))))) %>% 
  mutate(across(starts_with("indr_gewicht_"), ~ factor(.x, labels = c("ondergewicht", "normaal gewicht", "overgewicht"))),
         across(starts_with("d_indi_9_"), as.factor),
         across(starts_with("d_indi_9_"), ~if_else(.x == "NaN", NA, .x)))

# look at missings in the created data set for the contact moments 
sum(!is.na(jgz_data_BMI$BMI_3yrs9m)) #68360
sum(!is.na(jgz_data_BMI$BMI_3yrs9m) & !is.na(jgz_data_BMI$BMI_4months)) # 17676
sum(!is.na(jgz_data_BMI$BMI_3yrs9m) & (!is.na(jgz_data_BMI$BMI_4months) | !is.na(jgz_data_BMI$BMI_4wks) | 
                                         !is.na(jgz_data_BMI$BMI_8wks | !is.na(jgz_data_BMI$BMI_3months))))  # 18524
sum(!is.na(jgz_data_BMI$BMI_3yrs9m) & !is.na(jgz_data_BMI$BMI_4months) & !is.na(jgz_data_BMI$BMI_4wks) &
      !is.na(jgz_data_BMI$BMI_8wks & !is.na(jgz_data_BMI$BMI_3months)))  # 14863

jgz_data_BMI$RINPERSOON[duplicated(jgz_data_BMI$RINPERSOON)]
# RINPERSOONS RINPERSOON is unique (with one exception)

# standardize BMI, lengte and weight scores at earlier contactmoments based on age
# use to function y2z from the AGD package 
jgz_data_BMI <- jgz_data_BMI %>%
  # set length to length in cm and weight to weight in kg
  mutate(lengte_4wks = (lengte_4wks*0.1),
         lengte_8wks = (lengte_8wks*0.1),
         lengte_3months = (lengte_3months*0.1),
         lengte_4months = (lengte_4months*0.1),
         lengte_2yrs = (lengte_2yrs*0.1),
         lengte_3yrs = (lengte_3yrs*0.1),
         lengte_3yrs9m = (lengte_3yrs9m*0.1),
         gewicht_4wks = (gewicht_4wks*0.001),
         gewicht_8wks = (gewicht_8wks*0.001),
         gewicht_3months = (gewicht_3months*0.001),
         gewicht_4months = (gewicht_4months*0.001),
         gewicht_2yrs = (gewicht_2yrs*0.001),
         gewicht_3yrs = (gewicht_3yrs*0.001),
         gewicht_3yrs9m = (gewicht_3yrs9m*0.001)) %>% 
  mutate(bmi_4wks_znl = y2z(y=BMI_4wks, x=lft_jaren_4wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_8wks_znl = y2z(y=BMI_8wks, x=lft_jaren_8wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_3mnd_znl = y2z(y=BMI_3months, x=lft_jaren_3months, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_4mnd_znl = y2z(y=BMI_4months, x=lft_jaren_4months, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_2yrs_znl = y2z(y=BMI_2yrs, x=lft_jaren_2yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_3yrs_znl = y2z(y=BMI_3yrs, x=lft_jaren_3yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         bmi_3yrs9m_znl = y2z(y=BMI_3yrs9m, x=lft_jaren_3yrs9m, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")),
         
         lengte_4wks_znl = y2z(y=lengte_4wks, x = lft_jaren_4wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_8wks_znl = y2z(y=lengte_8wks, x = lft_jaren_8wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_3mnd_znl = y2z(y=lengte_3months, x = lft_jaren_3months, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_4mnd_znl = y2z(y=lengte_4months, x = lft_jaren_4months, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_2yrs_znl = y2z(y=lengte_2yrs, x = lft_jaren_2yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_3yrs_znl = y2z(y=lengte_3yrs, x = lft_jaren_3yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         lengte_3yrs9m_znl = y2z(y=lengte_3yrs9m, x = lft_jaren_3yrs9m, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")),
         
         gewicht_4wks_znl = y2z(y=gewicht_4wks, x = lft_jaren_4wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_8wks_znl = y2z(y=gewicht_8wks, x = lft_jaren_8wks, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_3mnd_znl = y2z(y=gewicht_3months, x = lft_jaren_3months, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_4mnd_znl = y2z(y=gewicht_4months, x = lft_jaren_4months, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_2yrs_znl = y2z(y=gewicht_2yrs, x = lft_jaren_2yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_3yrs_znl = y2z(y=gewicht_3yrs, x = lft_jaren_3yrs, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         gewicht_3yrs9m_znl = y2z(y=gewicht_3yrs9m, x = lft_jaren_3yrs9m, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt"))) %>% 
  select(-c(sex_z, sub_ethn))

# get an impression of the distribution of the zscore and 
# how often observations at earlier contactmoments are missing 
summary(jgz_data_BMI$bmi_4wks_znl)
summary(jgz_data_BMI$bmi_8wks_znl)
summary(jgz_data_BMI$bmi_3mnd_znl)
summary(jgz_data_BMI$bmi_4mnd_znl)
summary(jgz_data_BMI$bmi_2yrs_znl)
summary(jgz_data_BMI$bmi_3yrs_znl)
summary(jgz_data_BMI$bmi_3yrs9m_znl)

summary(jgz_data_BMI$lengte_4wks_znl)
summary(jgz_data_BMI$lengte_8wks_znl)
summary(jgz_data_BMI$lengte_3mnd_znl)
summary(jgz_data_BMI$lengte_4mnd_znl)
summary(jgz_data_BMI$lengte_3yrs_znl)
summary(jgz_data_BMI$lengte_3yrs9m_znl)

summary(jgz_data_BMI$gewicht_4wks_znl)
summary(jgz_data_BMI$gewicht_8wks_znl)
summary(jgz_data_BMI$gewicht_3mnd_znl)
summary(jgz_data_BMI$gewicht_4mnd_znl)
summary(jgz_data_BMI$gewicht_3yrs_znl)
summary(jgz_data_BMI$gewicht_3yrs9m_znl)


# add the BMI observations at previous cm to the 3 jaar 9 maanden data, joined by person identification
jgz_data_BMI_4m <- jgz_data_BMI %>% 
  left_join(y = jgz_data_demo, by = c("RINPERSOONS", "RINPERSOON"))

jgz_data_BMI_4m <- jgz_data_BMI_4m %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  # some children have multiple records for postcode (potentially because they moved over the years)
  # we select the postcode recorded at the doe-moment 4 months if this record is available
  filter(postcode_4months == postcode | is.na(postcode_4months)) %>%
  # if not available select most recent postcode before doe-moment
  filter(postcode_3months == postcode | is.na(postcode_3months)) %>%
  filter(postcode_8wks == postcode | is.na(postcode_8wks)) %>%
  filter(postcode_4wks == postcode | is.na(postcode_4wks)) %>%
  # if not available select postcode closest to doe-moment
  filter(postcode_2yrs == postcode | is.na(postcode_2yrs)) %>%
  # other children have records at multiple JGZ organisations or opl1/opl2 of the parent changes 
  # in that case we select the latest record so that we have one record per child
  slice_head() %>%  # the assumption is that records closer to the doe-moment (4months) more accurately reflect the kids environment
  ungroup()

# make sure child records are unique 
jgz_data_BMI_4m$RINPERSOON[duplicated(jgz_data_BMI_4m$RINPERSOON)]

# drop the rows where our outcome variable overweight is missing
jr3maand9_BMI <- jgz_data_BMI_4m %>% drop_na(BMI_3yrs9m)

# use this dataset when predicting overweight 
saveRDS(jr3maand9_BMI, "case_jgz/data_outcome/overweight/jr3maand9_BMI_p_711.rds")
#saveRDS(jgz_data_BMI, "case_jgz/data_outcome/overweight/jgz_data_BMI_711.rds")
#jgz_data_BMI <- readRDS("case_jgz/data_outcome/overweight/jgz_data_BMI_711.rds")


# now that we have filtered the case-relevant JGZ data
# we can move to the next step: 04_jgz_preprocess_join_cbs





