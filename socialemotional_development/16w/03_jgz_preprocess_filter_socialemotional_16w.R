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
# 'doe-moment': consultation moment during which the child is approx. 16 weeks gestational age
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


#### Subset the observations for the socio-emotional development case ####
# filter records for the socio-emotional development measurement instruments we're interested in
sdq_5jaar <- jgz_data %>% filter(scr_psp == "SDQ 5 jaar") 

# select only the children in ur intended contactmoment for who SDQ totalscore is recorded
sdq_5jaar <- sdq_5jaar %>% drop_na(sdq_t) # 17204

# keep only unique records
sdq_5jaar$RINPERSOON[duplicated(sdq_5jaar$RINPERSOON)]
# there are three children in the dataset with a double record

# if children have more than one record, select the record with the childs' age closest to the median age 
summary(sdq_5jaar$leeftijd)
sdq_5jaar <- sdq_5jaar %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  slice_min(abs(2136 - leeftijd)) %>%   
  ungroup() 

# we cannot use other variables from the JGZ data as predictors because the doe-moment is 16 weeks gestational age 
# we only use information available at the doe-moment to predict the outcome 


#### Create the outcome variable sdq_risk ####
sdq_5jaar <- sdq_5jaar %>%  
  mutate(sdq_risk = ifelse(sdq_t >= 11, 1, 0),
         sdq_risk = as.factor(sdq_risk))

# look at the outcome variable
table(sdq_5jaar$sdq_risk)
# about 14% of the observed kids have a sdq_t of 11 or higher
prop.table(table(sdq_5jaar$sdq_risk)) * 100

saveRDS(sdq_5jaar, "case_jgz/data_outcome/sdq_16w/sdq_5jaar_p.rds")


# now that we have filtered the case-relevant JGZ data
# we can move to the next step: 04_jgz_preprocess_join_cbs
