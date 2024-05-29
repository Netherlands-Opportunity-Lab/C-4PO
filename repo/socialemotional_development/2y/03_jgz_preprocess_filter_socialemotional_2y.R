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
jgz_data <- readRDS("case_jgz/jgz_data_p.rds")
# and the non-changing information set
jgz_data_demo <- readRDS("case_jgz/jgz_data_demo_p.rds")


#### THE CASE: SOCIAL-EMOTIONAL DEVELOPMENT with SDQ (NL: SOCIAAL-EMOTIONELE ONTWIKKELING met SDQ) ####
# 'doe-moment': consultation moment during which the child is approx. 2 years of age
# 'outcome-age': consultation moment during which the child is approx. 5 years of age


#### Get an impression of the SDQ items in the JGZ data ####
# look at the socio-emotional development variables in the JGZ data
jgz_data %>% select(c(scr_psp, sdq_t, sc_ep, sc_gp, sc_pl, sc_ha, sc_psg, sdq_i)) %>% summary()
# notice there are a lot of NA's
jgz_data_sdq <- jgz_data %>% select(c(scr_psp, sdq_t, sc_ep, sc_gp, sc_pl, sc_ha, sc_psg, sdq_i))
jgz_data_sdq <- jgz_data_sdq %>%
  filter(rowSums(is.na(jgz_data_sdq)) != ncol(jgz_data_sdq))
view(jgz_data_sdq)

# look at the different socio-emotional development measurement instruments recorded in the JGZ data
table(jgz_data$scr_psp) 
# we are interested in 'SDQ 5 jaar'; SDQ at the outcome-age 5 years of age

# the outcome variable is sdq_t = totaalscore SDQ
# how many observations for sdq_t are there per JGZ organisation 
jgz_data %>% 
  filter(!is.na(sdq_t)) %>% 
  group_by(org, sdq_t) %>% 
  summarise(n = n()) %>% 
  summarise(n = sum(n))

# how many observations for sdq_t are there at contactmoment groep 2 'outcome-age'
jgz_data %>% 
  filter(!is.na(sdq_t)) %>% 
  group_by(cm_groep2, sdq_t) %>% 
  summarise(n = n()) %>% 
  summarise(n = sum(n))

# how many observations for sdq_t are there at contactmoment 2 years 'doe-moment'
jgz_data %>% 
  filter(!is.na(sdq_t)) %>% 
  group_by(cm_2years, sdq_t) %>% 
  summarise(n = n()) %>% 
  summarise(n = sum(n))

# how many observations for sdq_t are there at contactmoments before doe-moment
jgz_data %>% 
  filter(!is.na(sdq_t)) %>% 
  group_by(cm_4months, sdq_t) %>% 
  summarise(n = n()) %>% 
  summarise(n = sum(n))

#### Subset the observations for the socio-emotional development case ####
# filter records for the socio-emotional development measurement instruments we're interested in
sdq_5jaar <- jgz_data %>% filter(scr_psp == "SDQ 5 jaar") 

# select only the children in ur intended contactmoment for who SDQ totalscore is recorded
sdq_5jaar <- sdq_5jaar %>% drop_na(sdq_t) 

# keep only unique records
sdq_5jaar$RINPERSOON[duplicated(sdq_5jaar$RINPERSOON)]
# there are three children in the dataset with a double record

# if children have more than one record, select the record with the childs' age closest to the median age 
summary(sdq_5jaar$leeftijd)
sdq_5jaar <- sdq_5jaar %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(abs(2136 - leeftijd)) %>%   
  ungroup() # N = 17201

# check whether there are other interesting variables we could use in the prediction
# the verwijs variables are 100% missing because most children in the sdq cohort are
# not observed and assessed on socio-economic development before the age of 5
test_verwijs <- jgz_data %>% filter(RINPERSOON %in% sdq_5jaar$RINPERSOON) %>% 
  filter(cm_4wks == 1 | cm_8wks == 1 | cm_3months == 1 | cm_4months == 1 | cm_6months == 1 | cm_9months == 1 | cm_11months == 1 | 
           cm_14months == 1 | cm_18months == 1 | cm_2years == 1) 
sum(is.na(test_verwijs$d_indi_10))/nrow(test_verwijs)*100 # indicatie psychosociale/emotionele ontwikkeling, is 99.9% missing
sum(is.na(test_verwijs$d_verwijs_25))/nrow(test_verwijs)*100 # verwijzing psycholoog, is 100% missing
sum(is.na(test_verwijs$d_verwijs_19))/nrow(test_verwijs)*100 # verwijzing cursus/groepsbehandeling, is 100% missing
sum(is.na(test_verwijs$d_verwijs_12))/nrow(test_verwijs)*100 # verwijzing home start, is 100% missing
sum(is.na(test_verwijs$d_verwijs_10))/nrow(test_verwijs)*100 # verwijzing opvoedingsbureau, is 100% missing
sum(is.na(test_verwijs$d_verwijs_9))/nrow(test_verwijs)*100 # verwijzing MEE/Integrale vroeghulp verwijzing, is 100% missing
sum(is.na(test_verwijs$d_verwijs_8))/nrow(test_verwijs)*100 # verwijzing ggz, is 100% missing
sum(is.na(test_verwijs$d_verwijs_7))/nrow(test_verwijs)*100 # verwijzing jeugdzorg, is 100% missing
sum(is.na(test_verwijs$d_verwijs_6))/nrow(test_verwijs)*100 # verwijzing veilig thuis, is 100% missing
sum(is.na(test_verwijs$d_verwijs_5))/nrow(test_verwijs)*100 # verwijzing maatschappelijk werk, is 100% missing
sum(!is.na(test_verwijs$d_inter_miss))/nrow(test_verwijs)*100 # interventie is > 99.5% missing
sum(!is.na(test_verwijs$d_indi_miss))/nrow(test_verwijs)*100 # indicatie is > 99.5% missing


#### Optional: Add other observations from the JGZ data ####
# join length/weight data and van wiechen data from earlier contactmoments
jgz_data_BMI <- readRDS("case_jgz/data_outcome/overweight/jgz_data_BMI_711.rds") %>% 
  mutate(postcode_2yrs = as.factor(postcode_2yrs))
wiechen_cm_2years <- readRDS("case_jgz/data_outcome/wiechen/wiechen_cm_2years_623.rds")
sdq_5jaar <- sdq_5jaar %>% 
  left_join(jgz_data_BMI, by = c("RINPERSOONS", "RINPERSOON", "postcode" = "postcode_2yrs")) %>% 
  left_join(wiechen_cm_2years, by = c("RINPERSOONS", "RINPERSOON", "postcode" = "postcode")) %>% 
  rename(zin2w_2y = zin2w.y,
         pop6_2y = pop6.y)

# get an impression of how often observations at earlier contactmoments are missing 
sum(is.na(sdq_5jaar$bmi_4wks_znl))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$bmi_8wks_znl))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$bmi_3mnd_znl))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$bmi_4mnd_znl))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$d_indi_9_4months))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$indr_gewicht_2yrs))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$d_taalomg_1.y))/nrow(sdq_5jaar)*100
# almost all previous observations are missing, so we exclude these
sum(is.na(sdq_5jaar$bmi_2yrs_znl))/nrow(sdq_5jaar)*100
sum(is.na(sdq_5jaar$zin2w_2y))/nrow(sdq_5jaar)*100

#therefore we decide to remove these variables and their z-scores
sdq_5jaar <- sdq_5jaar %>% select(-c(lengte_4wks, lengte_8wks, lengte_3months, lengte_4months, 
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
                                       lft_jaren_3yrs, lft_jaren_3yrs9m, postcode_3yrs, postcode_3yrs9m,
                                       d_indi_9_4wks, d_indi_9_8wks, d_indi_9_3months, d_indi_9_4months, d_indi_9_2yrs,
                                       indr_gewicht_4wks, indr_gewicht_8wks, indr_gewicht_3months, indr_gewicht_4months, 
                                       indr_gewicht_2yrs, d_indi_9_3yrs, d_indi_9_3yrs9m, indr_gewicht_3yrs, indr_gewicht_3yrs9m,
                                       d_taalomg_1.y, d_taalomg_2.y, d_taalomg_3.y)) 



#### Create the outcome variable sdq_risk ####
sdq_5jaar <- sdq_5jaar %>%  
  mutate(sdq_risk = ifelse(sdq_t >= 11, 1, 0),
         sdq_risk = as.factor(sdq_risk))

# look at the outcome variable
table(sdq_5jaar$sdq_risk)
# about 14% of the observed kids have a sdq_t of 11 or higher
prop.table(table(sdq_5jaar$sdq_risk)) * 100

saveRDS(sdq_5jaar, "case_jgz/data_outcome/sdq_2y/sdq_5jaar_p.rds")


# now that we have filtered the case-relevant JGZ data
# we can move to the next step: 04_jgz_preprocess_join_cbs
