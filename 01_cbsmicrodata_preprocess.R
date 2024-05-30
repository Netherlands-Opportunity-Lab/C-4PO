#### CBS MICRODATA PREPARATION ####

#### 00_cbsmicrodata_preprocess.R ####
# THE GOAL OF THIS SCRIPT IS TO PREPARE THE CBS MICRODATA: READ THE FILES AND VARIABLES OF INTEREST 

# load the necessary libraries 
library(tidyverse)
library(readxl)
library(haven)
library(lubridate)
library(labelled)
library(stringr)
setwd("H:/")


#### CBS microdata files ####
# In this project we use multiple CBS microdata files:
# - KINDOUDERTAB
# - GBAPERSOONTAB
# - KOPPELPERSOONHUISHOUDEN
# - GBAHUISHOUDENSBUS
# - GBAADRESOBJECTBUS / VSLPOSTCODEBUS / VSLGWBTAB
# - PRNL
# - HOOGSTEOPLTAB
# - SECMBUS
# - INPATAB / INTEGRAAL PERSOONLIJK INKOMEN
# - INHATAB/ INTEGRAAL HUISHOUDENS INKOMEN
# - SPOLISBUS/ POLISBUS
# - VEHTAB
# - EIGENDOMWOZBAGTAB / EIGENDOMTAB / EIGENDOMWOZTAB 
# - ZVWZORGKOSTENTAB

# From these cbs microdata files read the years and variables of interest 


#### Select population of interest ####
# define the population of interest as specified in 00a_cbsmicrodata_preprocess_id.R or 00b_cbsmicrodata_preprocess_id.R
perined_id_parents <- readRDS("Mirthe/case_jgz/repo/data_cbs/perined_id_parents.rds")
jgz_id_parents <- readRDS("Mirthe/case_jgz/repo/data_cbs/jgz_id_parents.rds")
jgz_id_kids <- readRDS("Mirthe/case_jgz/repo/data_cbs/jgz_id_kids.rds")

# to filter out the records we need we use id_parents, specify the population here: jgz or perined
id_parents <- jgz_id_parents
#id_parents <- perined_id_parents

# specify the path in accordance with the population of interest
data_path <- "H:/Mirthe/case_jgz/repo/data_cbs/"
#data_path <- "H:/Mirthe/case1_preterm_birth/repo/data_cbs/"

#### Identify microdata file paths and years ####
# we specify the years of interest of the microdata files and the paths indicating the location of these files
# we do this in a separate file, so that if paths change or you want to select different years you only have to update the config file

# input the desired config file here:
cfg_file <- "H:/Mirthe/data/data_scripts/preprocessing_set_V2.yml"

# load the configuration 
cfg <- config::get("data_preparation", file = cfg_file)
loc <- config::get("file_locations", file = cfg_file)


#### Functions file paths ####
# in the RA environment, CBS microdata are saved in different locations in G: in various folders in multiple files (per year)
# we use these functions to automatically read the file paths for the specified years

# function to get latest KINDOUDERTAB version of specified year
get_kindouder_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$kindouder_data, year),
    pattern = paste0("KINDOUDER", year,"V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest KOPPELPERSOONHUISHOUDEN version of specified year 
get_koppelpersoonhuishouden_filename <- function(year) {
  # get all KOPPELPERSOONHUISHOUDEN files from 2004-2010
  if (year >= 2004 & year <= 2010) {
    fl <- list.files(
      path = file.path(loc$data_folder, loc$hh_koppel),
      pattern = paste0("Koppeltabel_IPI_IHI_IVB", year), 
      full.names = TRUE
    )
    sort(fl, decreasing = TRUE)[1]
  }
  
  else if (year >=2011) {
    # get all KOPPELPERSOONHUISHOUDEN files 2011-2021 
    fl <- list.files(
      path = file.path(loc$data_folder, loc$hh_koppel),
      pattern = paste0("KOPPELPERSOONHUISHOUDEN", year), 
      full.names = TRUE
    )
    #return only the latest version
    sort(fl, decreasing = TRUE)[1]
    
  } 
}

# function to get latest residence version of specified year
get_residence_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$residence_data),
    pattern = paste0("GBAADRESOBJECT", year),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest PRNL version of specified year
get_prnl_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$prnl_data, year),
    pattern = paste0(year,"V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest hoogsteopltab version of specified year
get_educ_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$educ_data, year),
    pattern = "(?i)(.sav)",
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest ipi version of specified year
get_ipi_filename <- function(year) {
  # get all ipi files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$income_ipi_data, year),
    pattern = paste0("PERSOONINK", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  #return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest inpa version of specified year
get_inpa_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$income_data),
    pattern = paste0("INPA", year, "TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  #return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest ihi version of speified year (integraal huishoudens inkomen)
get_ihi_filename <- function(year) {
  # get all ihi files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$hh_ihi_data, year),
    pattern = paste0("HUISHBVRINK", year, "TABV[0-9](?i)(.sav)"), 
    full.names = TRUE
  )
  #return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest inhatab version of speified year
get_inhatab_filename <- function(year) {
  # get all inhatab files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$hh_income_data),
    pattern = paste0("INHA", year, "TABV[0-9](?i)(.sav)"),
    full.names = TRUE
  )
  #return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest polis version of specified year
get_polis_filename <- function(year) {
  # get all polis files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$polis_data, year),
    pattern = paste0("POLISBUS", year, "V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest spolis version of specified year
get_spolis_filename <- function(year) {
  # get all spolis files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$spolis_data, year),
    pattern = paste0(year, "V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest vehtab version of speified year
get_vehtab_filename <- function(year) {
  # get all vehtab files with the specified year 
  fl <- list.files(
    path = file.path(loc$data_folder, loc$vermogen),
    pattern = paste0("VEH", year, "TABV[0-9](?i)(.sav)"),
    full.names = TRUE
  )
  #return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest EIGENDOMWOZBAGTAB version of specified year
get_wozbag_filename <- function(year) {
  BAG <- "EIGENDOMWOZBAG|EIGENDOMWOZBAGTAB"
  fl <- list.files(
    path = file.path(loc$data_folder, loc$eigendomwozbag_data, year),
    pattern = paste0(BAG, year, "V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest EIGENDOMWOZTAB version of specified year
get_woz_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$eigendomwoz_data, year),
    pattern = paste0("EIGENDOMWOZTAB", year,"V[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest EIGENDOMTAB version of specified year
get_eigendom_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$eigendomwoz_data),
    pattern = paste0("EIGENDOM", year,"TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}

# function to get latest ZVWKZORGKOSTEN version of specified year
get_zvwzorgkosten_filename <- function(year) {
  fl <- list.files(
    path = file.path(loc$data_folder, loc$zvwzorgkosten_data, year),
    pattern = paste0("ZVWZORGKOSTEN", year,"TABV[0-9]+(?i)(.sav)"),
    full.names = TRUE
  )
  # return only the latest version
  sort(fl, decreasing = TRUE)[1]
}



#### CBS microdata topics: ####
#### Demographic data GBAPERSOONTAB ####
# load additional parent data from GBAPERSOONTAB
gba_dat <-read_sav(file.path(loc$data_folder, loc$gba_data), 
                   col_select = c("RINPERSOONS","RINPERSOON",
                                  "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", 
                                  "GBAGENERATIE", "GBAHERKOMSTGROEPERING")) %>% 
  # filter to keep only records of the population of interest
  filter((RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) | (RINPERSOONS %in% jgz_id_kids$rins & RINPERSOON %in% jgz_id_kids$rin)) %>% 
  mutate(birthday = dmy(paste(GBAGEBOORTEDAG, GBAGEBOORTEMAAND, GBAGEBOORTEJAAR, sep = "-"))) %>% 
  select(-GBAGEBOORTEJAAR, -GBAGEBOORTEMAAND, -GBAGEBOORTEDAG) %>% 
  mutate(RINPERSOONS = as_factor(RINPERSOONS, level = "values"),
         GBAHERKOMSTGROEPERING = ifelse(GBAHERKOMSTGROEPERING == "----", NA, GBAHERKOMSTGROEPERING),
         GBAHERKOMSTGROEPERING = ifelse(GBAHERKOMSTGROEPERING == "0000", NA, GBAHERKOMSTGROEPERING),
         GBAHERKOMSTGROEPERING = as_factor(GBAHERKOMSTGROEPERING),
         GBAGENERATIE = as_factor(GBAGENERATIE)) %>% 
  rename(rins = RINPERSOONS,
         rin = RINPERSOON,
         GBA_origin = GBAHERKOMSTGROEPERING,
         GBA_generation = GBAGENERATIE)

saveRDS(gba_dat, file.path(data_path, paste0("gba_dat.rds")))


#### Identify household with RINPERSOONSHKW/RINPERSOONHKW ####
id_hh_rins <- tibble(rins = factor(), rin = character(), rins_hh = factor(), rin_hh = character())

# loop to bind the rin and rins of the household data from the available years (2004-2021)
for (year in 2004:2021) {
  if (year >= 2004 & year <=2010){
    id_hh_rins <- read_sav(get_koppelpersoonhuishouden_filename(year)) %>% 
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             rins_hh = RINPERSOONSKERN,
             rin_hh = RINPERSOONKERN) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             rins_hh = as_factor(rins_hh, levels = "values"),
             year = year) %>%
      filter(rins %in% id_parents$rins & rin %in% id_parents$rin) %>% 
      bind_rows(id_hh_rins, .)
  } else if (year >= 2011) {
    id_hh_rins <- read_sav(get_koppelpersoonhuishouden_filename(year)) %>% 
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             rins_hh = RINPERSOONSHKW,
             rin_hh = RINPERSOONHKW) %>%
      mutate(rins = as_factor(rins, levels = "values"),
             rins_hh = as_factor(rins_hh, levels = "values"),
             year = year) %>%
      filter(rins %in% id_parents$rins & rin %in% id_parents$rin) %>% 
      bind_rows(id_hh_rins, .)
    
  }
} 

# restructure year into start_data and end_date
id_hh_rins <- id_hh_rins %>% 
  mutate(
    start_date = as.Date(paste(year, 1, 1, sep = "-")),
    end_date = as.Date(paste(year, 12, 31, sep = "-")))

saveRDS(id_hh_rins, file.path(data_path, paste0("id_hh_rins.rds")))

#### Household type data GBAHUISHOUDENSBUS ####
gba_hh <- read_sav(file.path(loc$data_folder, loc$gba__hh_data),
                   col_select = c("RINPERSOONS","RINPERSOON", "DATUMAANVANGHH", "DATUMEINDEHH",
                                  "TYPHH", "PLHH", "AANTALPERSHH")) %>%
  mutate(RINPERSOONS = as_factor(RINPERSOONS, level = "values"),
         DATUMAANVANGHH = ymd(DATUMAANVANGHH),
         DATUMEINDEHH = ymd(DATUMEINDEHH),
         TYPHH = as_factor(TYPHH,  level = "labels"),
         PLHH = as.factor(PLHH)) %>%
  mutate(AANTALPERSHHf = ifelse(AANTALPERSHH >= 8, "8+", AANTALPERSHH),
         AANTALPERSHHf = factor(AANTALPERSHHf, ordered = TRUE), 
         AANTALPERSHH = ifelse(AANTALPERSHH > 25, NA, AANTALPERSHH),
         hh_single_parent = ifelse(TYPHH == "Eenouderhuishouden", 1, 0),
         hh_single_parent = as_factor(hh_single_parent),
         hh_married = ifelse(PLHH == " 4" | PLHH == " 6", 1, 0),
         hh_married = as_factor(hh_married)) %>% 
  # filter to keep only records of the population of interest
  filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) 

saveRDS(gba_hh, file.path(data_path, paste0("gba_hh.rds")))

#### Residence data GBAADRESOBJECTBUS ####
residence_dat <- 
  read_sav(get_residence_filename(cfg$residence_year)) %>% 
  # filter to keep only records of the population of interest
  filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
  rename(rins = RINPERSOONS,
         rin = RINPERSOON,
         start_date = GBADATUMAANVANGADRESHOUDING,
         end_date = GBADATUMEINDEADRESHOUDING,
         residence = RINOBJECTNUMMER,
         residence_type = SOORTOBJECTNUMMER) %>% 
  mutate(rins = as_factor(rins, levels = "values"),
         residence_type = as_factor(residence_type, levels = "values"),
         start_date = ymd(start_date),
         end_date = ymd(end_date))

saveRDS(residence_dat, file.path(data_path, paste0("residence_dat.rds")))

residence_dat_kid <- 
  read_sav(get_residence_filename(cfg$residence_year)) %>% 
  # filter to keep only records of the population of interest
  filter(RINPERSOONS %in% jgz_id_kids$rins & RINPERSOON %in% jgz_id_kids$rin) %>%
  rename(rins = RINPERSOONS,
         rin = RINPERSOON,
         start_date = GBADATUMAANVANGADRESHOUDING,
         end_date = GBADATUMEINDEADRESHOUDING,
         residence = RINOBJECTNUMMER,
         residence_type = SOORTOBJECTNUMMER) %>% 
  mutate(rins = as_factor(rins, levels = "values"),
         residence_type = as_factor(residence_type, levels = "values"),
         start_date = ymd(start_date),
         end_date = ymd(end_date))

saveRDS(residence_dat_kid, file.path(data_path, paste0("residence_dat_kid.rds")))


#### Birth records data from PRNL ####
#loop to bind the prnl data from the available years
# create a dataframe for the prnl data
prnl_dat <- tibble(rins = factor(), rin = character(), rins_mo = factor(),
                   rin_mo = character(), sex_at_birth = factor(), PRNL_mortality = factor(),
                   PRNL_birthweight = double(), PRNL_gestational_age = double(), 
                   PRNL_gestational_age_week = double(),
                   PRNL_parity = factor(), PRNL_origing_mo = factor(), 
                   PRNL_originv_mo = factor(), PRNL_multiples = factor(), year = numeric())

# loop to bind the prnl data from the available years
for (year in seq(format(dmy(cfg$prnl_year_min), "%Y"),
                 format(dmy(cfg$prnl_year_max), "%Y"))) { 
  
  if (year >= 2008) { 
    prnl_dat <- read_sav(get_prnl_filename(year),
                         col_select = c("rinpersoon_kind", "rinpersoons_kind_uitgebreid", 
                                        "rinpersoon_moeder", "rinpersoons_moeder",
                                        "datumkind", "geslachtkind", "sterfte", "gewichtkind_ruw", "amddd",
                                        "amww", "pariteit_cat2", "herkomst_g", "herkomst_v", "meerling")) %>% 
      
      # rename variables for clarity and continuity 
      rename(rins = rinpersoons_kind_uitgebreid,
             rin = rinpersoon_kind,
             rins_mo = rinpersoons_moeder,
             rin_mo = rinpersoon_moeder,
             birthdate = datumkind,
             sex_at_birth = geslachtkind,
             PRNL_mortality = sterfte,
             PRNL_birthweight = gewichtkind_ruw,
             PRNL_gestational_age = amddd,
             PRNL_gestational_age_week = amww,
             PRNL_parity = pariteit_cat2,
             PRNL_origing_mo = herkomst_g,
             PRNL_originv_mo = herkomst_v,
             PRNL_multiples = meerling) %>% 
      mutate(rins = as_factor(rins, levels = "value"),
             rins_mo = as_factor(rins_mo, levels = "value"),
             birthdate = ymd(birthdate),
             # set levels to check the labels 
             sex_at_birth = as_factor(sex_at_birth, levels = "labels"),
             PRNL_mortality = as_factor(PRNL_mortality, levels = "labels"),
             PRNL_parity = as_factor(PRNL_parity, levels = "labels"),
             PRNL_origing_mo = as_factor(PRNL_origing_mo, levels = "labels"),
             PRNL_originv_mo = as_factor(PRNL_originv_mo, levels = "labels"),
             PRNL_multiples = as_factor(PRNL_multiples, levels = "labels"),
             year = year) %>% 
      # add to data
      bind_rows(prnl_dat, .)
    
  } else if (year <= 2007) {
    prnl_dat <- read_sav(get_prnl_filename(year),
                         col_select = c("rinkindnum", "srtnumkind", "rinma", "srtnummoeder",
                                        "datumkind", "geslachtkind", "sterfte", "gewichtkind_ruw",
                                        "amddd", "amww", "pariteit_cat2", "herkomst_g", "herkomst_v", 
                                        "meerling")) %>%
      # rename variables for clarity and continuity
      rename(rins = srtnumkind,
             rin = rinkindnum,
             rins_mo = srtnummoeder,
             rin_mo = rinma,
             birthdate = datumkind,
             sex_at_birth = geslachtkind,
             PRNL_mortality = sterfte,
             PRNL_birthweight = gewichtkind_ruw,
             PRNL_gestational_age = amddd,
             PRNL_gestational_age_week = amww,
             PRNL_parity = pariteit_cat2,
             PRNL_origing_mo = herkomst_g,
             PRNL_originv_mo = herkomst_v,
             PRNL_multiples = meerling) %>%
      mutate(rins = as_factor(rins, levels = "value"),
             rins_mo = as_factor(rins_mo, levels = "value"),
             birthdate = ymd(birthdate),
             # keep leading zeros
             rin = sprintf("%09d", rin),
             rin_mo = sprintf("%09d", rin_mo),
             # convert numeric rin to character
             rin = as.character(rin),
             rin_mo = as.character(rin_mo),
             # clean rin and set NA to empty string
             rin = ifelse(rin == "       NA", "", rin),
             # clean other variables and show labels 
             sex_at_birth = as_factor(sex_at_birth, levels = "labels"),
             PRNL_mortality = as_factor(PRNL_mortality, levels = "labels"),
             PRNL_parity = as_factor(PRNL_parity, levels = "labels"),
             PRNL_origing_mo = as_factor(PRNL_origing_mo, levels = "labels"),
             PRNL_originv_mo = as_factor(PRNL_originv_mo, levels = "labels"),
             PRNL_multiples = as_factor(PRNL_multiples, levels = "labels"),
             year = year) %>%
      # add to data
      bind_rows(prnl_dat, .)
  }
  
}

prnl_dat <- prnl_dat %>% 
  mutate(PRNL_parity = as.factor(recode(PRNL_parity, "para 4+" = "para 4 of meer")))


# if level names start with a capital change it to lower
levels(prnl_dat$sex_at_birth) <- tolower(levels(prnl_dat$sex_at_birth))
levels(prnl_dat$PRNL_mortality) <- tolower(levels(prnl_dat$PRNL_mortality))
levels(prnl_dat$PRNL_parity) <- tolower(levels(prnl_dat$PRNL_parity))
levels(prnl_dat$PRNL_origing_mo) <- tolower(levels(prnl_dat$PRNL_origing_mo))
levels(prnl_dat$PRNL_originv_mo) <- tolower(levels(prnl_dat$PRNL_originv_mo))
levels(prnl_dat$PRNL_multiples) <- tolower(levels(prnl_dat$PRNL_multiples))

saveRDS(prnl_dat, file.path(data_path, paste0("prnl_dat.rds")))

#### Education data HOOGSTEOPLTAB #### 

# create a dataframe for the education level data
educ_dat <- tibble(rins = factor(), rin = character(), educationlevel = factor(), educationnr = factor(), year = numeric())

# loop to bind the hoogsteopltab data from the available years
for (year in seq(format(dmy(cfg$educ_year_min), "%Y"),
                 format(dmy(cfg$educ_year_max), "%Y"))) {
  if (year >= 1999 & year <= 2012){
    educ_dat <- read_sav(get_educ_filename(year),
                         col_select = c("RINPERSOONS","RINPERSOON", "OPLNRHB")) %>%  
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      # rename variables for clarity and consistency
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             educationnr = OPLNRHB) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             educationnr = ifelse(educationnr == "----", NA, educationnr),
             educationnr = as.factor(educationnr),
             year = year) %>% 
      bind_rows(educ_dat, .)
    
  } else if (year >= 2013 & year <= 2018) { 
    educ_dat <- read_sav(get_educ_filename(year),
                         col_select = c("RINPERSOONS","RINPERSOON", "OPLNIVSOI2016AGG4HBMETNIRWO","OPLNRHB")) %>% 
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      # rename variables for clarity and consistency
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             educationlevel = OPLNIVSOI2016AGG4HBMETNIRWO,
             educationnr = OPLNRHB) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             educationlevel = ifelse(educationlevel == "----", NA, educationlevel),
             educationnr = ifelse(educationnr == "----", NA, educationnr),
             educationlevel = as.factor(educationlevel),
             educationnr = as.factor(educationnr),
             year = year) %>% 
      bind_rows(educ_dat, .)
    
  } else if (year >= 2019) {
    educ_dat <- read_sav(get_educ_filename(year),
                         col_select = c("RINPERSOONS","RINPERSOON", "OPLNIVSOI2021AGG4HBmetNIRWO", "OPLNRHB")) %>% 
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      # rename variables for clarity and consistency
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             educationlevel = OPLNIVSOI2021AGG4HBmetNIRWO,
             educationnr = OPLNRHB) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             educationlevel = ifelse(educationlevel == "----", NA, educationlevel),
             educationnr = ifelse(educationnr == "----", NA, educationnr),
             educationlevel = as.factor(educationlevel),
             educationnr = as.factor(educationnr),
             year = year) %>% 
      bind_rows(educ_dat, .)
  }
} 

# restructure year into start_data and end_date
educ_dat <- educ_dat %>% 
  mutate(
    start_date = as.Date(paste(year, 1, 1, sep = "-")),
    end_date = as.Date(paste(year, 12, 31, sep = "-"))) %>% 
  select(-year)

#saveRDS(educ_dat, file.path(data_path, paste0("educ_dat.rds")))

# The variable educationlevel is not made available in years prior to 2013. We use the OPLNR 
# (number to identify an education), which is available from 1999 on, and classify these numbers using 
# the same categories as educationlevels. To categorise the OPLNR we use reference files, opleidsingsnrref and ctoref
opleidingsnrref <- read_sav(loc$opleidingsnrref,
                            col_select = c("OPLNR","CTO2016V")) %>% 
  mutate(OPLNR = ifelse(OPLNR == "------", NA, OPLNR),
         CTO2016V = ifelse(CTO2016V == " ", NA, CTO2016V))
ctoref <- read_sav(loc$ctoref, col_select = c("CTO", "OPLNIVSOI2016AGG4HB")) %>% 
  mutate(CTO = ifelse(CTO == " ", NA, CTO),
         OPLNIVSOI2016AGG4HB = ifelse(OPLNIVSOI2016AGG4HB == " ", NA, OPLNIVSOI2016AGG4HB))

educ_dat_ref <- educ_dat %>% 
  left_join(opleidingsnrref,
            by = c("educationnr" = "OPLNR")) %>% 
  left_join(ctoref, 
            by = c("CTO2016V" = "CTO")) %>%
  mutate(educationlevel = ifelse(is.na(educationlevel), OPLNIVSOI2016AGG4HB, as.character(educationlevel))) %>% 
  mutate(educationnr = as.factor(educationnr),
         educationlevel = as.factor(educationlevel))

saveRDS(educ_dat_ref, file.path(data_path, paste0("educ_dat_ref.rds")))


#### Socio-economic data SECMBUS ####

# select the most recent secm file 
secm_dat <- 
  read_sav(file.path(loc$data_folder, loc$secm_data)) %>% 
  select(-ONDERWIJSNR_crypt) %>% 
  as_factor(only_labelled = TRUE, levels = "values") %>% 
  # filter to keep only records of the population of interest
  filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
  # rename variables for clarity and consistency
  rename(
    rins = RINPERSOONS,
    rin = RINPERSOON,
    start_date = AANVSECM,
    end_date = EINDSECM,
    SECM = SECM,
    SECM_employee = XKOPPELWERKNSECM,
    SECM_director = XKOPPELDGASECM,
    SECM_selfemployed = XKOPPELZELFSTSECM,
    SECM_otherwork = XKOPPELOVACTIEFSECM,
    SECM_unemployed = XKOPPELWERKLUITKSECM,
    SECM_socialassistance = XKOPPELBIJSTANDSECM,
    SECM_otherassistance = XKOPPELSOCVOORZOVSECM,
    SECM_disability = XKOPPELZIEKTEAOSECM,
    SECM_retirement = XKOPPELPENSIOENSECM,
    SECM_student = XKOPPELSCHOLSTUDSECM,
    SECM_familywork = XKOPPELMEEWERKENDSECM) %>% 
  mutate(
    SECM_employee = ifelse(SECM_employee == "", NA, 1),
    SECM_director = ifelse(SECM_director == "", NA, 1),
    SECM_selfemployed = ifelse(SECM_selfemployed == "", NA, 1),
    SECM_otherwork = ifelse(SECM_otherwork == "", NA, 1),
    SECM_unemployed = ifelse(SECM_unemployed == "", NA, 1),
    SECM_socialassistance = ifelse(SECM_socialassistance == "", NA, 1),
    SECM_otherassistance = ifelse(SECM_otherassistance == "", NA, 1),
    SECM_disability = ifelse(SECM_disability == "", NA, 1),
    SECM_retirement = ifelse(SECM_retirement == "", NA, 1),
    SECM_student = ifelse(SECM_student == "", NA, 1),
    SECM_familywork = ifelse(SECM_familywork == "", NA, 1),
    start_date = ymd(start_date),
    end_date = ymd(end_date)) %>% 
  mutate(across(c(starts_with("SECM")), ~as.factor(.)))


secm_dat_NA0 <- secm_dat %>% 
  mutate_at(names(secm_dat %>% select(-c(rins, rin, start_date, end_date, SECM))), 
            function(x) ifelse(is.na(x), 0, x)) %>%
  mutate(across(c(rins, starts_with("SECM")), ~as.factor(.)))

saveRDS(secm_dat_NA0, file.path(data_path, paste0("secm_dat_NA0.rds")))

#### Income data IPI/INPATAB ####

income_dat <- tibble(rins = factor(), rin = character(), income = double())

# loop to bind the income data from the available years
for (year in seq(format(dmy(cfg$parent_income_year_min), "%Y"),
                 format(dmy(cfg$parent_income_year_max), "%Y"))) {
  if (year >= 2011 & year <= 2020) {
    income_dat <- read_sav(get_inpa_filename(year),
                           col_select = c("RINPERSOONS","RINPERSOON", "INPPERSBRUT")) %>% 
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             income = INPPERSBRUT) %>%
      mutate(rins = as_factor(rins, levels = "values"),
             income = as.numeric(income),
             income = ifelse(income == "9999999999", NA, income),
             year = year) %>%
      
      bind_rows(income_dat, .)
    
  } else if (year >= 2003 & year <= 2010) {
    income_dat <- read_sav(get_ipi_filename(year),
                           col_select = c("RINPERSOONS","RINPERSOON", "PERSBRUT")) %>% 
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(rins = RINPERSOONS,
             rin = RINPERSOON,
             income = PERSBRUT) %>%
      mutate(rins = as_factor(rins, levels = "values"),
             income = as.numeric(income),
             income = ifelse(income == "999999999", NA, income),
             year = year) %>% 
      bind_rows(income_dat, .)
    
  }
}

# restructure year into start_data and end_date
income_dat <- income_dat %>% 
  mutate(
    start_date = as.Date(paste(year, 1, 1, sep = "-")),
    end_date = as.Date(paste(year, 12, 31, sep = "-"))) %>% 
  select(-year)

saveRDS(income_dat, file.path(data_path, paste0("income_dat.rds")))

#### Household income data IHI/INHATAB ####

hh_ihi_dat <- NULL 

for (year in seq(format(dmy(cfg$hh_ihi_year_min), "%Y"),
                 format(dmy(cfg$hh_ihi_year_max), "%Y"))) { 
  hh_ihi_dat <- read_sav(get_ihi_filename(year),
                         col_select = c("RINPERSOONSKERN", "RINPERSOONKERN", 
                                        "BVRLICAT", "BVRLICATL", # Inkomen ten opzichte van de lage-inkomensgrens in het verslagjaar / in de laatste vier jaren.
                                        "BVRSOCKL", "BVRSOCKLL", # Inkomen ten opzichte van het beleidsmatig minimum in het verslagjaar / in de laatste vier jaren.
                                        "BVRBBIHJ", # Belangrijkste inkomensbron van het huishouden. 
                                        "BVRGESTINKH", # Gestandaardiseerd besteedbaar inkomen van het huishouden.
                                        "BVRPERCGESTINKH", # Percentielgroepen gestandaardiseerd besteedbaar inkomen particuliere 
                                        #huishoudens inclusief studentenhuishoudens 
                                        "BVREHALG"))%>% #Woningbezit particuliere huishoudens op 1 januari van het verslagjaar)) 
    rename(rins_hh = RINPERSOONSKERN,
           rin_hh = RINPERSOONKERN, 
           l_income_hh_pov = BVRLICAT, 
           l_income_hh_pov_4j = BVRLICATL,
           l_income_hh_min = BVRSOCKL,  
           l_income_hh_min_4j = BVRSOCKLL,
           income_hh_source = BVRBBIHJ,
           income_hh = BVRGESTINKH,
           income_hh_perc_alle_hh = BVRPERCGESTINKH,
           house_ownership = BVREHALG)%>%   
    mutate(rins_hh = as_factor(rins_hh, levels = "values"),
           l_income_hh_pov_binary = as_factor(l_income_hh_pov, levels = "values"),
           l_income_hh_pov_binary = as.numeric(l_income_hh_pov_binary),
           l_income_hh_pov_binary = ifelse(l_income_hh_pov_binary<=3, NA, ifelse(l_income_hh_pov_binary==4, 1, 0)),
           l_income_hh_pov = as_factor(l_income_hh_pov, levels = "values"),
           
           l_income_hh_pov_4j_binary = as_factor(l_income_hh_pov_4j, levels = "values"),
           l_income_hh_pov_4j_binary = as.numeric(l_income_hh_pov_4j_binary),
           l_income_hh_pov_4j_binary = ifelse(l_income_hh_pov_4j_binary<=3, NA, ifelse(l_income_hh_pov_4j_binary==4, 1, 0)),
           l_income_hh_pov_4j = as_factor(l_income_hh_pov_4j, levels = "values"),
           
           l_income_hh_min_binary = as_factor(l_income_hh_min, levels = "values"),
           l_income_hh_min_binary = as.numeric(l_income_hh_min_binary),
           l_income_hh_min_binary = ifelse(l_income_hh_min_binary<=3, NA, ifelse(l_income_hh_min_binary>=4 & l_income_hh_min_binary<=6, 1, 0)),
           l_income_hh_min = as_factor(l_income_hh_min, levels = "values"),
           
           l_income_hh_min_4j_binary = as_factor(l_income_hh_min_4j, levels = "values"), 
           l_income_hh_min_4j_binary = as.numeric(l_income_hh_min_4j_binary),
           l_income_hh_min_4j_binary = ifelse(l_income_hh_min_4j_binary<=3, NA, ifelse(l_income_hh_min_4j_binary>=4 & l_income_hh_min_4j_binary<=6, 1, 0)),
           l_income_hh_min_4j = as_factor(l_income_hh_min_4j, levels = "values"), 
           
           income_hh = as.numeric(income_hh),
           income_hh = ifelse(income_hh == "999999999", NA, income_hh),
           year = year)%>%
    set_value_labels(l_income_hh_pov_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0), 
                     l_income_hh_pov_4j_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0), 
                     l_income_hh_min_binary = c("Income below 110% social min" = 1, "Income above 110% social min" = 0), 
                     l_income_hh_min_4j_binary = c("Income below 110% social min" = 1, "Income above 110% social min" = 0))%>%
    select(-c(l_income_hh_pov, l_income_hh_pov_4j, l_income_hh_min, l_income_hh_min_4j))%>%
    mutate(house_ownership = as_factor(house_ownership, levels="values"),
           income_hh_source=as_factor(income_hh_source, levels="labels"), 
           income_hh_perc_alle_hh = na_if(income_hh_perc_alle_hh, ''), 
           income_hh_perc_alle_hh = as_factor(income_hh_perc_alle_hh, levels="values"))%>%
    # filter to keep only records of the population of interest; use household RINS/RIN
    filter(rins_hh %in% id_hh_rins$rins_hh & rin_hh %in% id_hh_rins$rin_hh)%>%
    bind_rows(hh_ihi_dat, .)
}

#saveRDS(hh_ihi_dat, file.path(data_path, paste0("hh_ihi_dat.rds")))

hh_inha_dat <- NULL

for (year in seq(format(dmy(cfg$hh_income_year_min), "%Y"),
                 format(dmy(cfg$hh_income_year_max), "%Y")))   { 
  hh_inha_dat <- read_sav(get_inhatab_filename(year),
                          col_select = c("RINPERSOONSHKW", "RINPERSOONHKW", 
                                         "INHARMEUR", "INHARMEURL", # Inkomen ten opzichte van de Europese armoedegrens in het verslagjaar / in de laatste vier jaren.
                                         "INHARMLAG", "INHARMLAGL", # Inkomen ten opzichte van de lage-inkomensgrens in het verslagjaar / in de laatste vier jaren.
                                         "INHARMSOC", "INHARMSOCL", # Inkomen ten opzichte van het beleidsmatig minimum in het verslagjaar / in de laatste vier jaren.
                                         "INHBBIHJ", # Belangrijkste inkomensbron van het huishouden. 
                                         "INHGESTINKH", # Gestandaardiseerd besteedbaar inkomen van het huishouden.
                                         "INHP100HGESTES", # Percentielgroepen gestandaardiseerd besteedbaar inkomen particuliere huishoudens exclusief studentenhuishoudens
                                         "INHP100HGEST" , #Percentiegroepen gestandaardiseerd besteedbaar inkomen particuliere huishoudens
                                         "INHEHALGR"))%>% #Woningbezit particuliere huishoudens op 1 januari van het verslagjaar)) 
    rename(rins_hh = RINPERSOONSHKW,
           rin_hh = RINPERSOONHKW, 
           l_income_hh_eur = INHARMEUR, 
           l_income_hh_eur_4j = INHARMEURL,
           l_income_hh_pov = INHARMLAG, 
           l_income_hh_pov_4j = INHARMLAGL,
           l_income_hh_min = INHARMSOC,  
           l_income_hh_min_4j = INHARMSOCL,
           income_hh_source = INHBBIHJ,
           income_hh = INHGESTINKH,
           income_hh_perc_excl_stud = INHP100HGESTES,
           income_hh_perc_alle_hh = INHP100HGEST,
           house_ownership = INHEHALGR)%>% 
    mutate(rins_hh = as_factor(rins_hh, levels = "values"),
           l_income_hh_eur_binary = as.numeric(l_income_hh_eur),
           l_income_hh_eur_binary = ifelse(l_income_hh_eur_binary<0, NA, ifelse(l_income_hh_eur_binary<100, 1, 0)),
           l_income_hh_eur = as_factor(l_income_hh_eur, levels = "values"),
           
           l_income_hh_eur_4j_binary = as.numeric(l_income_hh_eur_4j),
           l_income_hh_eur_4j_binary = ifelse(l_income_hh_eur_4j_binary<0, NA, ifelse(l_income_hh_eur_4j_binary<100, 1, 0)),
           l_income_hh_eur_4j = as_factor(l_income_hh_eur_4j, levels = "values"),
           
           l_income_hh_pov_binary = as.numeric(l_income_hh_pov),
           l_income_hh_pov_binary = ifelse(l_income_hh_pov_binary<0, NA, ifelse(l_income_hh_pov_binary<100, 1, 0)),
           l_income_hh_pov = as_factor(l_income_hh_pov, levels = "values"),
           
           l_income_hh_pov_4j_binary = as.numeric(l_income_hh_pov_4j),
           l_income_hh_pov_4j_binary = ifelse(l_income_hh_pov_4j_binary<0, NA, ifelse(l_income_hh_pov_4j_binary<100, 1, 0)),
           l_income_hh_pov_4j = as_factor(l_income_hh_pov_4j, levels = "values"),
           
           l_income_hh_min_binary = as.numeric(l_income_hh_min),
           l_income_hh_min_binary = ifelse(l_income_hh_min_binary<0, NA, ifelse(l_income_hh_min_binary<110, 1, 0)),
           l_income_hh_min = as_factor(l_income_hh_min, levels = "values"),
           
           l_income_hh_min_4j_binary = as.numeric(l_income_hh_min_4j),
           l_income_hh_min_4j_binary = ifelse(l_income_hh_min_4j_binary<0, NA, ifelse(l_income_hh_min_4j_binary<110, 1, 0)),
           l_income_hh_min_4j = as_factor(l_income_hh_min_4j, levels = "values"), 
           
           income_hh = as.numeric(income_hh),
           income_hh = ifelse(income_hh == "9999999999", NA, income_hh),
           year = year)%>%
    set_value_labels(l_income_hh_eur_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0),
                     l_income_hh_eur_4j_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0),
                     l_income_hh_pov_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0),
                     l_income_hh_pov_4j_binary = c("Income below poverty line" = 1, "Income above poverty line" = 0),
                     l_income_hh_min_binary = c("Income below 110% social min" = 1, "Income above 110% social min" = 0),
                     l_income_hh_min_4j_binary = c("Income below 110% social min" = 1, "Income above 110% social min" = 0))%>%
    select(-c(l_income_hh_eur, l_income_hh_eur_4j, l_income_hh_pov, l_income_hh_pov_4j, l_income_hh_min, l_income_hh_min_4j))%>% 
    mutate(income_hh_perc_alle_hh = as.numeric(income_hh_perc_alle_hh),
           income_hh_perc_alle_hh = ifelse(income_hh_perc_alle_hh<0, NA, income_hh_perc_alle_hh), #code -1 and -2 do not exist in ihi
           income_hh_perc_alle_hh = as_factor(income_hh_perc_alle_hh), 
           income_hh_perc_alle_hh = recode(income_hh_perc_alle_hh, '1' = ' 0', '2' = ' 1', '3' = ' 2', '4' = ' 3', '5' = ' 4', 
                                           '6' = ' 5', '7' = ' 6', '8' = ' 7', '9' = ' 8', '10' = ' 9', 
                                           '11' = '10', '12' = '11', '13' = '12', '14' = '13', '15' = '14', 
                                           '16' = '15', '17' = '16', '18' = '17', '19' = '18', '20' = '19', 
                                           '21' = '20', '22' = '21', '23' = '22', '24' = '23', '25' = '24', 
                                           '26' = '25', '27' = '26', '28' = '27', '29' = '28', '30' = '29', 
                                           '31' = '30', '32' = '31', '33' = '32', '34' = '33', '35' = '34', 
                                           '36' = '35', '37' = '36', '38' = '37', '39' = '38', '40' = '39', 
                                           '41' = '40', '42' = '41', '43' = '42', '44' = '43', '45' = '44', 
                                           '46' = '45', '47' = '46', '48' = '47', '49' = '48', '50' = '49', 
                                           '51' = '50', '52' = '51', '53' = '52', '54' = '53', '55' = '54', 
                                           '56' = '55', '57' = '56', '58' = '57', '59' = '58', '60' = '59', 
                                           '61' = '60', '62' = '61', '63' = '62', '64' = '63', '65' = '64', 
                                           '66' = '65', '67' = '66', '68' = '67', '69' = '68', '70' = '69', 
                                           '71' = '70', '72' = '71', '73' = '72', '74' = '73', '75' = '74', 
                                           '76' = '75', '77' = '76', '78' = '77', '79' = '78', '80' = '79', 
                                           '81' = '80', '82' = '81', '83' = '82', '84' = '83', '85' = '84', 
                                           '86' = '85', '87' = '86', '88' = '87', '89' = '88', '90' = '89', 
                                           '91' = '90', '92' = '91', '93' = '92', '94' = '93', '95' = '94', 
                                           '96' = '95', '97' = '96', '98' = '97', '99' = '98', '100' = '99'), 
           house_ownership = as_factor(house_ownership, levels="values"), 
           house_ownership = recode(house_ownership, '2' = '3', '3' = '2', '8' = '9'), #recode to match hh_ihi_dat; swap 2 and 3, institutineel= onbekend
           income_hh_source = as_factor(income_hh_source, levels="labels"), 
           income_hh_perc_excl_stud = as_factor(income_hh_perc_excl_stud, levels = "labels"))%>% 
    # filter to keep only records of the population of interest; use household RINS/RIN
    filter(rins_hh %in% id_hh_rins$rins_hh & rin_hh %in% id_hh_rins$rin_hh) %>%
    bind_rows(hh_inha_dat, .)
}

#saveRDS(hh_inha_dat, file.path(data_path, paste0("hh_inha_dat.rds")))

#bind hh_income dataset to form one dataset 2004 - 2021, link the individual rin/rins to each hh in each year and drop other hh
hh_income_dat <-bind_rows(hh_ihi_dat, hh_inha_dat)%>%
  mutate(house_ownership = recode(house_ownership, '1' = 'Eigen woning', 
                                  '2'= 'Huurwoning met huurtoeslag', 
                                  '3' =  'Huurwoning zonder huurtoslag', 
                                  '9' = 'Onbekend'))%>%
  right_join(id_hh_rins, by=c("rins_hh", "rin_hh", "year"))


saveRDS(hh_income_dat, file.path(data_path, paste0("hh_income_dat.rds")))


#### Jobs and wages data SPOLISBUS ####
spolis_dat <- tibble(rins = factor(), rin = character(),
                     SPOLIS_wages = double(), SPOLIS_paidhours = double(), SPOLIS_contract = factor())

for (year in seq.int(cfg$parent_spolis_year_min, cfg$parent_spolis_year_max)) {
  if (year >= 2010 & year <= 2021) {
    spolis_dat <- read_sav(get_spolis_filename(year),
                           col_select = c("RINPERSOONS", "RINPERSOON", 
                                          "SDATUMAANVANGIKO", "SDATUMEINDEIKO",
                                          "SBASISLOON", "SBASISUREN", "SCONTRACTSOORT")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(rins = RINPERSOONS, 
             rin = RINPERSOON, 
             start_date = SDATUMAANVANGIKO, 
             end_date = SDATUMEINDEIKO,
             SPOLIS_wages = SBASISLOON, 
             SPOLIS_paidhours = SBASISUREN, 
             SPOLIS_contract = SCONTRACTSOORT) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             SPOLIS_contract = as_factor(SPOLIS_contract, levels = "values"),
             start_date = ymd(start_date),
             end_date = ymd(end_date)) %>% 
      bind_rows(spolis_dat, .)
  }
}

saveRDS(spolis_dat, file.path(data_path, paste0("spolis_dat.rds")))

polis_dat <- tibble(rins = factor(), rin = character(),
                    SPOLIS_wages = double(), SPOLIS_paidhours = double(), SPOLIS_contract = factor())

for (year in seq.int(cfg$parent_polis_year_min, cfg$parent_polis_year_max)) {
  if (year >= 2006 & year <= 2009) {
    polis_dat <- read_sav(get_polis_filename(year),
                          col_select = c("RINPERSOONS", "RINPERSOON", 
                                         "AANVBUS", "EINDBUS",
                                         "BASISLOON", "BASISUREN", "CONTRACTSOORT")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(rins = RINPERSOONS, 
             rin = RINPERSOON, 
             start_date = AANVBUS, 
             end_date = EINDBUS,
             SPOLIS_wages = BASISLOON, 
             SPOLIS_paidhours = BASISUREN, 
             SPOLIS_contract = CONTRACTSOORT) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             SPOLIS_contract = as_factor(SPOLIS_contract, levels = "values"),
             start_date = ymd(start_date),
             end_date = ymd(end_date)) %>% 
      bind_rows(polis_dat, .)
  }
}

saveRDS(polis_dat, file.path(data_path, paste0("polis_dat.rds")))

#### Wealth data VEHTAB #### 

hh_schulden <- tibble(rins_hh = factor(), rin_hh = character(), hh_vermogen = double())

for (year in seq(format(dmy(cfg$hh_capital_year_min), "%Y"),
                 format(dmy(cfg$hh_capital_year_max), "%Y"))) { 
  hh_schulden <- read_sav(get_vehtab_filename(year),
                          col_select = c("RINPERSOONSHKW", "RINPERSOONHKW", 
                                         "VEHW1000VERH"))%>% #Vermogen van het huishouden (euro), Het vermogen is gelijk aan het verschil tussen de bezittingen (VEHW1100BEZH) en de schulden (VEHW1200STOH).
    rename(rins_hh = RINPERSOONSHKW,
           rin_hh = RINPERSOONHKW, 
           hh_vermogen = VEHW1000VERH)%>% 
    mutate(rins_hh = as_factor(rins_hh, levels = "values"),
           hh_vermogen = as.numeric(hh_vermogen),
           hh_vermogen = ifelse(hh_vermogen == "9999999999", NA, hh_vermogen),
           hh_vermogen = ifelse(hh_vermogen == "99999999999", NA, hh_vermogen),  
           year = year)%>%
    # filter to keep only records of the population of interest
    filter(rins_hh %in% id_hh_rins$rins_hh & rin_hh %in% id_hh_rins$rin_hh)%>% 
    bind_rows(hh_schulden, .)
  
}

# join schulden rins and rin with kid rins and rin 
hh_schulden <- hh_schulden %>%
  right_join(id_hh_rins, by=c("rins_hh", "rin_hh", "year"))

saveRDS(hh_schulden_dat, file.path(data_path, paste0("hh_schulden_dat.rds")))

#### Property value data EIGENDOMWOZ(BAG)TAB ####
# load is this is no longer in your environment 
#residence_dat <- readRDS(file.path(data_path, paste0("residence_dat.rds")))

eigendomwoz_dat <- tibble(SOORTOBJECTNUMMER = factor(), RINOBJECTNUMMER = character(), 
                          ED_rentown = factor(), ED_woz = double())

# loop to bind the income data from the available years
for (year in seq.int(cfg$property_year_min, cfg$property_year_max)) {
  if (year >= 2014 & year <= 2021) {
    eigendomwoz_dat <- read_sav(get_wozbag_filename(year),
                                col_select = c("SOORTOBJECTNUMMER","RINOBJECTNUMMER", "VBOEIGENDOMBAG",
                                               "WOZWAARDEOBJECTBAG")) %>%
      # filter to keep only records of the population of interest
      filter(SOORTOBJECTNUMMER %in% residence_dat$residence_type & RINOBJECTNUMMER %in% residence_dat$residence) %>%
      rename(ED_rentown = VBOEIGENDOMBAG,
             ED_woz = WOZWAARDEOBJECTBAG) %>%
      mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
             ED_rentown = as_factor(ED_rentown, levels = "values"),
             ED_woz = as.numeric(ED_woz),
             year = year) %>%
      
      bind_rows(eigendomwoz_dat, .)
    
  } else if (year >= 2012 & year <= 2013) {
    eigendomwoz_dat <- read_sav(get_wozbag_filename(year),
                                col_select = c("Soortobjectnummer","RINObjectnummer", "VboeigendomBAG",
                                               "WozwaardeobjectBAG")) %>% 
      # filter to keep only records of the population of interest
      filter(Soortobjectnummer %in% residence_dat$residence_type & RINObjectnummer %in% residence_dat$residence) %>%
      rename(SOORTOBJECTNUMMER = Soortobjectnummer,
             RINOBJECTNUMMER = RINObjectnummer,
             ED_rentown = VboeigendomBAG,
             ED_woz = WozwaardeobjectBAG) %>%
      mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
             ED_rentown = as_factor(ED_rentown, levels = "values"),
             ED_woz = as.numeric(ED_woz),
             year = year) %>% 
      bind_rows(eigendomwoz_dat, .)
    # before 2006 all fields are empty except SOORTOBJECTNUMMER and RINOBJECTNUMMER, so exclude 1995-2005
  } else if (year >= 2006 & year <= 2011) {
    eigendomwoz_dat <- read_sav(get_woz_filename(year),
                                col_select = c("SOORTOBJECTNUMMER","RINOBJECTNUMMER", "VBOEIGENDOM",
                                               "WOZWAARDEOBJECT")) %>% 
      # filter to keep only records of the population of interest
      filter(SOORTOBJECTNUMMER %in% residence_dat$residence_type & RINOBJECTNUMMER %in% residence_dat$residence) %>%
      rename(ED_rentown = VBOEIGENDOM,
             ED_woz = WOZWAARDEOBJECT) %>%
      mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
             ED_rentown = as_factor(ED_rentown, levels = "values"),
             ED_woz = as.numeric(ED_woz),
             year = year) %>% 
      bind_rows(eigendomwoz_dat, .)
    
  }
}

# ED_rentown NA from 2015 on
eigendomwoz_dat <- eigendomwoz_dat %>% 
  mutate_at(c("ED_rentown"), ~na_if(., ''))

eigendomwoz_dat <- eigendomwoz_dat %>% 
  mutate(
    start_date = as.Date(paste(year, 1, 1, sep = "-")),
    end_date = as.Date(paste(year, 12, 31, sep = "-"))) %>% 
  select(-year)

saveRDS(eigendomwoz_dat, file.path(data_path, paste0("eigendomwoz_dat.rds")))

#### Residence and living data EIGENDOMTAB ####

eigendom_dat <- tibble(SOORTOBJECTNUMMER = factor(), RINOBJECTNUMMER = character(), ED_rentown = factor(),
                       ED_residents = integer(), ED_typeowner = factor())

# loop to bind the income data from the available years
for (year in seq.int(2012, 2022)) {
  if (year >= 2012 & year <= 2022) {
    eigendom_dat <- read_sav(get_eigendom_filename(year),
                             col_select = c("SOORTOBJECTNUMMER","RINOBJECTNUMMER", "TypeEigendom",
                                            "AantalBewoners", "TypeEigenaar")) %>%
      # filter to keep only records of the population of interest
      filter(SOORTOBJECTNUMMER %in% residence_dat$residence_type & RINOBJECTNUMMER %in% residence_dat$residence) %>%
      rename(ED_rentown = TypeEigendom,
             ED_residents = AantalBewoners,
             ED_typeowner = TypeEigenaar) %>%
      mutate(SOORTOBJECTNUMMER = as_factor(SOORTOBJECTNUMMER, levels = "values"),
             ED_rentown = as_factor(ED_rentown, levels = "values"),
             ED_residents = as.integer(ED_residents),
             ED_typeowner = as_factor(ED_typeowner, levels = "values"),
             year = year) %>%
      bind_rows(eigendom_dat, .)
  }
}

saveRDS(eigendom_dat, file.path(data_path, paste0("eigendom_dat.rds")))

#### Health care costs data ZVWZORGKOSTENTAB ####  

health_dat <- tibble(rins = factor(), rin = character(),ZVWK_GP_basic = double(), ZVWK_GP_regist = double(),
                     ZVWK_GP_consult = double(), ZVWK_GP_other = double(), ZVWK_pharmacy = double(), ZVWK_dentalcare = double(),
                     ZVWK_hospital = double(), ZVWK_physical_therapy = double(), ZVWK_physical_other = double(), 
                     ZVWK_appliances = double(), ZVWK_patient_transport_sit = double(), ZVWK_patient_transport_lie = double(),
                     ZVWK_birth_obstetrician = double(), ZVWK_birth_maternitycare = double(), ZVWK_abroad = double(),
                     ZVWK_abroad_sub1 = double(), ZVWK_abroad_sub2 = double(), ZVWK_mentalhealth_bas = double(), 
                     ZVWK_mentalhealth_spec = double(), ZVWK_mentalhealth_spec_stay = double(), 
                     ZVWK_mentalh_spec_nostay_inst = double(), ZVWK_mentalh_spec_nostay_ind = double(),
                     ZVWK_mentalh_spec_other = double(), ZVWK_mentalh_spec_long = double(), 
                     ZVWK_geriatric = double(), ZVWK_localnurse = double(), ZVWK_multidisc = double(),
                     ZVWK_sensory = double(), ZVWK_total = double(), ZVWK_deductible = double(), 
                     ZVWK_primarycare_residence = double(), ZVWK_other = double(), year = integer())


for (year in seq(format(dmy(cfg$health_year_min), "%Y"),
                 format(dmy(cfg$health_year_max), "%Y"))) {
  
  if (year >= 2018 & year <= 2020) {
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKEERSTELIJNSPSYCHO",
                                          -"ZVWKGGZ", -"NOPZVWKGGZOVERIG")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKGENBASGGZ, 
        ZVWK_mentalhealth_spec = ZVWKSPECGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_mentalh_spec_long = NOPZVWKLANGDGGZ,          
        ZVWK_geriatric = ZVWKGERIATRISCH,
        ZVWK_localnurse = ZVWKWYKVERPLEGING,          
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG,
        ZVWK_abroad_sub1 = NOPZVWKBUITENLGRENS, 
        ZVWK_abroad_sub2 = NOPZVWKBUITENLZINL, 
        ZVWK_multidisc = ZVWKMULTIDISC,
        ZVWK_sensory = ZVWKZINTUIGLIJK,
        ZVWK_total = ZVWKTOTAAL,
        ZVWK_deductible = NOPZVWKVRIJWILLIGEIGENRISICO,
        ZVWK_primarycare_residence = ZVWKEERSTELIJNSVERBLIJF) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  } else if (year == 2017) {
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKEERSTELIJNSPSYCHO",
                                          -"ZVWKGGZ", -"NOPZVWKGGZOVERIG")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKGENBASGGZ, 
        ZVWK_mentalhealth_spec = ZVWKSPECGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_mentalh_spec_long = NOPZVWKLANGDGGZ,          
        ZVWK_geriatric = ZVWKGERIATRISCH,
        ZVWK_localnurse = ZVWKWYKVERPLEGING,          
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG,
        ZVWK_abroad_sub1 = NOPZVWKBUITENLGRENS, 
        ZVWK_abroad_sub2 = NOPZVWKBUITENLZINL, 
        ZVWK_multidisc = ZVWKMULTIDISC,
        ZVWK_sensory = ZVWKZINTUIGLIJK,
        ZVWK_total = ZVWKTOTAAL,
        ZVWK_deductible = NOPZVWKVRIJWILLIGEIGENRISICO) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  } else if (year >= 2015 & year <= 2016) {
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKEERSTELIJNSPSYCHO",
                                          -"ZVWKGGZ", -"NOPZVWKGGZOVERIG")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKGENBASGGZ, 
        ZVWK_mentalhealth_spec = ZVWKSPECGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_mentalh_spec_long = NOPZVWKLANGDGGZ,          
        ZVWK_geriatric = ZVWKGERIATRISCH,
        ZVWK_localnurse = ZVWKWYKVERPLEGING,          
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG,
        ZVWK_abroad_sub1 = NOPZVWKBUITENLGRENS, 
        ZVWK_abroad_sub2 = NOPZVWKBUITENLZINL, 
        ZVWK_multidisc = ZVWKMULTIDISC,
        ZVWK_sensory = ZVWKZINTUIGLIJK) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  } else if (year == 2014) {
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKEERSTELIJNSPSYCHO",
                                          -"ZVWKGGZ", -"NOPZVWKGGZOVERIG", -"ZVWKWYKVERPLEGING")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKGENBASGGZ, 
        ZVWK_mentalhealth_spec = ZVWKSPECGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_geriatric = ZVWKGERIATRISCH,
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  } else if (year == 2013){
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKGENBASGGZ",
                                          -"ZVWKSPECGGZ", -"ZVWKWYKVERPLEGING")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKEERSTELIJNSPSYCHO, 
        ZVWK_mentalhealth_spec = ZVWKGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_geriatric = ZVWKGERIATRISCH,
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG,
        ZVWK_mentalh_spec_other = NOPZVWKGGZOVERIG) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  } else if (year >= 2009 & year <= 2012) {
    health_dat <- read_sav(get_zvwzorgkosten_filename(year), 
                           col_select = c(-"ZVWKOPHOOGFACTOR", -"ZVWKHUISARTS", -"ZVWKPARAMEDISCH",
                                          -"ZVWKZIEKENVERVOER", -"ZVWKGEBOORTEZORG", -"ZVWKGENBASGGZ",
                                          -"ZVWKSPECGGZ", -"ZVWKGERIATRISCH", -"ZVWKWYKVERPLEGING")) %>%
      # filter to keep only records of the population of interest
      filter(RINPERSOONS %in% id_parents$rins & RINPERSOON %in% id_parents$rin) %>%
      rename(
        rins = RINPERSOONS,
        rin = RINPERSOON,
        ZVWK_pharmacy = ZVWKFARMACIE, 
        ZVWK_dentalcare = ZVWKMONDZORG,
        ZVWK_hospital = ZVWKZIEKENHUIS, 
        ZVWK_appliances = ZVWKHULPMIDDEL,           
        ZVWK_abroad = ZVWKBUITENLAND,
        ZVWK_other = ZVWKOVERIG,
        ZVWK_mentalhealth_bas = ZVWKEERSTELIJNSPSYCHO, 
        ZVWK_mentalhealth_spec = ZVWKGGZ,
        ZVWK_mentalhealth_spec_stay = NOPZVWKGGZMV,
        ZVWK_mentalh_spec_nostay_inst = NOPZVWKGGZZVINST,
        ZVWK_mentalh_spec_nostay_ind = NOPZVWKGGZZVZELF,
        ZVWK_GP_basic = NOPZVWKEERSTELIJNSOND, 
        ZVWK_GP_regist = NOPZVWKHUISARTSINSCHRIJF,
        ZVWK_GP_consult = NOPZVWKHUISARTSCONSULT, 
        ZVWK_GP_other = NOPZVWKHUISARTSOVERIG, 
        ZVWK_physical_therapy = NOPZVWKFYSIOTHERAPIE, 
        ZVWK_physical_other = NOPZVWKPARAMEDISCHOV, 
        ZVWK_birth_obstetrician = NOPZVWKVERLOSKUNDE, 
        ZVWK_birth_maternitycare = NOPZVWKKRAAMZORG,
        ZVWK_patient_transport_sit = NOPZVWKZIEKENVERVOERZIT, 
        ZVWK_patient_transport_lie = NOPZVWKZIEKENVERVOERLIG,
        ZVWK_mentalh_spec_other = NOPZVWKGGZOVERIG) %>% 
      mutate(rins = as_factor(rins, levels = "values"),
             year = year) %>%
      bind_rows(health_dat, .)
    
  }
  
} 

health_dat <- health_dat %>% 
  mutate(
    start_date = as.Date(paste(year, 1, 1, sep = "-")),
    end_date = as.Date(paste(year, 12, 31, sep = "-"))) %>% 
  select(-c(year, NOPGENZORGSPECPATGROEP))

saveRDS(health_dat, file.path(data_path, paste0("health_dat.rds")))












