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
# read the data set of interest: b) the Perined data
perined_data_id <- readRDS("/Polina/perined_cleaned.rds") %>% 
  select(c(Rinpersoons, Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder)) %>% 
  mutate(Rinpersoons = as_factor(Rinpersoons),
         Rinpersoons_Kind = as_factor(Rinpersoons_Kind),
         Rinpersoons_Moeder = as_factor(Rinpersoons_Moeder))

nrow(perined_data_id)


# Identify the parents of the children in the JGZ data
perined_data_id <- left_join(
  x = perined_data_id,
  y = read_sav("G://Bevolking/KINDOUDERTAB/KINDOUDER2022TABV1.sav") %>% 
    select(-(c("XKOPPELNUMMER"))) %>% 
    as_factor(only_labelled = TRUE, levels = "values"),
  by = c("Rinpersoons_Kind" = "RINPERSOONS", "Rinpersoon_Kind" = "RINPERSOON")) %>%
  rename(rins_mo_cbs = RINPERSOONSMa,
         rin_mo_cbs = RINPERSOONMa,
         rins_fa = RINPERSOONSpa,
         rin_fa = RINPERSOONpa) 
# change empty fields to NA 
perined_data_id <- perined_data_id %>% 
  mutate(Rinpersoon_Moeder = if_else(Rinpersoon_Moeder == "---------", NA, Rinpersoon_Moeder),
         Rinpersoon_Moeder = if_else(Rinpersoon_Moeder == "000000000", NA, Rinpersoon_Moeder),
         rin_mo_cbs = ifelse(rin_mo_cbs == "---------", NA, rin_mo_cbs),
         rin_fa = ifelse(rin_fa == "---------", NA, rin_fa))


# check if all of the perined kids have a registered mother and father

# first, check NAs resulting from the join
sum(is.na(perined_data_id$Rinpersoons_Moeder) | is.na(perined_data_id$Rinpersoon_Moeder)) # birth mother
sum(is.na(perined_data_id$rins_mo_cbs) | is.na(perined_data_id$rin_mo_cbs)) # legal mother in CBS
sum(is.na(perined_data_id$rins_fa) | is.na(perined_data_id$rin_fa)) # legal father in CBS


perined_data_id <- perined_data_id %>% 
  select(c(Rinpersoons_Kind, Rinpersoon_Kind, Rinpersoons_Moeder, Rinpersoon_Moeder, rins_fa, rin_fa)) %>% 
  rename(rins_ki = Rinpersoons_Kind, rin_ki = Rinpersoon_Kind, rins_mo = Rinpersoons_Moeder, rin_mo = Rinpersoon_Moeder) %>% 
  pivot_longer(rins_ki:rin_fa, 
               names_to = c(".value", "fam_member"), 
               names_sep = "_") %>% 
  unique() 

# we are unable to join microdata for individuals without rin, so we drop those
perined_data_id <- perined_data_id %>% 
  filter(!is.na(rin))

# in this case, we're interested in the information of the birth parents 
perined_id_parents <- perined_data_id %>% 
  filter(fam_member != "ki")

# also save the kids ID in a separate set
perined_id_kids <- perined_data_id %>%  
  filter(fam_member == "ki")

# save the defined population of interest 
saveRDS(perined_id_parents, "Mirthe/case_jgz/repo/data_cbs/perined_id_parents.rds")
#saveRDS(perined_id_kids, "Mirthe/case_jgz/repo/data_cbs/perined_id_kids.rds")

