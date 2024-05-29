#### DATA DEFINITION ####

#### 00a_cbsmicrodata_preprocess_id.R ####
# THE GOAL OF THIS SCRIPT IS TO DEFINE THE POPULATION OF INTEREST 

# create a list to identify all individual's (with RINPERSOONS/RINPERSOON) of interest for analysis 
# in this case we take all RINPERSOONS/RINPERSOON of the children and the parents (mother/ father) in 
# a) the JGZ data set with Preventive Youth Health Care records from 4 organizations from 2009-2018, or
# b) the Perined data set which includes all birth records between 1999-2020

# of course there is also option c) you can define your own population of interest by selecting ID's from a data set of choice

# load the necessary libraries 
library(tidyverse)
library(readxl)
library(haven)
library(lubridate)
library(labelled)
library(stringr)
setwd("H:/")

#### POPULATION DATA ####
# read the data set of interest: a) the JGZ data
jgz_data_id <- read_dta("JGZ for Helen and Mirthe/JGZ_panel.dta") %>% 
  select(c(RINPERSOONS, RINPERSOON)) %>% 
  mutate(RINPERSOONS = as.factor(RINPERSOONS)) %>% 
  unique()

nrow(jgz_data_id)


# Identify the legal parents of the children in the JGZ data
jgz_data_id <- left_join(
  x = jgz_data_id,
  y = read_sav("G://Bevolking/KINDOUDERTAB/KINDOUDER2022TABV1.sav") %>% 
    select(-(c("XKOPPELNUMMER"))) %>% 
    as_factor(only_labelled = TRUE, levels = "values"),
  by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON")) %>% 
  rename(rins_mo = RINPERSOONSMa,
         rin_mo = RINPERSOONMa,
         rins_fa = RINPERSOONSpa,
         rin_fa = RINPERSOONpa) 
# change empty fields to NA 
jgz_data_id <- jgz_data_id %>% 
  mutate(rin_mo = ifelse(rin_mo == "---------", NA, rin_mo),
         rin_fa = ifelse(rin_fa == "---------", NA, rin_fa))

# check if all of the jgz kids have a registered mother and father

# first, check NAs resulting from the join
sum(is.na(jgz_data_id$rins_mo) | is.na(jgz_data_id$rin_mo))
sum(is.na(jgz_data_id$rins_fa) | is.na(jgz_data_id$rin_fa))


# pivot longer to make sure every row is one individual RINPERSOONS/RINPERSOON 
jgz_data_id <- jgz_data_id %>% 
  rename(rins_ki = RINPERSOONS, rin_ki = RINPERSOON) %>%
  pivot_longer(rins_ki:rin_mo, 
               names_to = c(".value", "fam_member"), 
               names_sep = "_") %>% 
  unique() 

# we are unable to join microdata for individuals without rin, so we drop those
jgz_data_id <- jgz_data_id %>% 
  filter(!is.na(rin))

# in this case, we're interested in the information of the parents 
jgz_id_parents <- jgz_data_id %>%  
  filter(fam_member != "ki")

# also save the kids ID in a separate set
jgz_id_kids <- jgz_data_id %>%  
  filter(fam_member == "ki")

# save the defined population of interest 
saveRDS(jgz_id_parents, "Mirthe/case_jgz/repo/data_cbs/jgz_id_parents.rds")
saveRDS(jgz_id_kids, "Mirthe/case_jgz/repo/data_cbs/jgz_id_kids.rds")


