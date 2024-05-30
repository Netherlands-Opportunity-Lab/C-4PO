#### JGZ DATA PREPARATION ####
# In the scripts 02_jgz_preprocess_transform.R, 03_jgz_preprocess_filter.R, 04_jgz_preprocess_join_cbs.R
# and 05_jgz_preprocess_MI we prepare the JGZ data for analysis in 06_jgz_prediction.R

#### 02_jgz_preprocess_transform.R ####
# THE GOAL OF THIS SCRIPT IS TO CLARIFY THE VARIABLE AVAILABLE IN THE JGZ DATA, 
# TO ADD PARENT INFORMATION AND TRANSFORM THE VARIABLES IN THE JGZ DATA FOR ANALYSIS 

# load the necessary libraries 
library(tidyverse)
library(haven)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readxl)
library(AGD)
setwd("H:/")  

#### 1. Look at the JGZ data ####

# read the cleaned JGZ data (see 20220330 JGZ DATA - CLEAN AND MERGE.do for the cleaning steps)
jgz_data <- read_dta("JGZ for Helen and Mirthe/JGZ_panel.dta")
# look at the JGZ data 
glimpse(jgz_data)

#### 2. Specify class and labels of variables for clarity, ####
# ensure categorical variables are recognized as factors 
jgz_data <- jgz_data %>% 
  mutate(
    RINPERSOONS = as.factor(RINPERSOONS), 
    woonplaats = as.factor(woonplaats),
    gem = as.factor(gem),
    gebl1 = as.factor(gebl1),
    gebl2 = as.factor(gebl2),
    schoolnr = as.factor(schoolnr),
    visus_c = as.factor(visus_c),
    dat_a_str = ymd(dat_a_str),
    #dat_a2_str = ymd(dat_a2_str),  #.r ?? vanaf 2012
    org = as.factor(org),
    geslacht = as.factor(geslacht),
    i_cm = as.factor(i_cm),
    st_a = as.factor(st_a),
    so_a2 = as.factor(so_a2),
    jaartal = as.integer(jaartal),
    kindmogelijknietunieknr2 = as.factor(kindmogelijknietunieknr2),
    maanden = as.factor(maanden),
    bril = as.factor(bril),
    oog_o = as.factor(oog_o),
    across(starts_with("apk"), as.factor),
    lh_r = as.factor(lh_r),
    lh_l = as.factor(lh_l),
    lc_r = as.factor(lc_r),
    lc_l = as.factor(lc_l),
    scr_psp = as.factor(scr_psp),
    zin2w = as.factor(zin2w),
    pop6 = as.factor(pop6),
    noemt = as.factor(noemt),
    wijstaan = as.factor(wijstaan),
    zin3w = as.factor(zin3w),
    verstaan1 = as.factor(verstaan1),
    spontaan = as.factor(spontaan),
    vragen1 = as.factor(vragen1),
    verstaan2 = as.factor(verstaan2),
    vragen2 = as.factor(vragen2),
    r_gd_vve = as.factor(r_gd_vve),
    across(starts_with("d_so_a"), as.factor),
    across(starts_with("d_i_jgz"), as.factor),
    across(starts_with("d_indi"), as.factor),
    across(starts_with("d_inter"), as.factor),
    across(starts_with("d_verwijs"), as.factor),
    across(starts_with("d_opl"), as.factor),  
    across(starts_with("d_kmn"), as.factor),
    across(starts_with("d_dubbel"), as.factor),
    across(starts_with("d_bst_nl"), as.factor),
    across(starts_with("d_taalomg"), as.factor),
    across(starts_with("d_meertaal"), as.factor),
    across(starts_with("d_d_vvenum"), as.factor)
  )


# add labels to factor levels 
levels(jgz_data$geslacht) <- list(mannelijk = "1", vrouwelijk = "2", onbekend = "0")
levels(jgz_data$visus_c) <- list(voldoende = "1", twijfelachtig = "2", onvoldoende = "3", .r = ".r")
levels(jgz_data$i_cm) <- list("cm 9 maanden" = "9")
levels(jgz_data$st_a) <- list(uitgevoerd = "1") # uitgevoerd/ verschenen op afspraak # 2 niet uitgevoerd/ niet verschenen 
levels(jgz_data$so_a2) <- list("cm op indicatie" = "22")
levels(jgz_data$bril) <- list(bril = "1", lenzen = "2")
levels(jgz_data$oog_o) <- list(ja = "1", nee = "2")
levels(jgz_data$apk_r) <- list("5/30" = "1", "5/20" = "2", "5/15 = 3", "5/10" = "4", "5/6" = "5", "5/5" = "6", "niet gelukt" = "99")
levels(jgz_data$apk_l) <- list("5/30" = "1", "5/20" = "2", "5/15 = 3", "5/10" = "4", "5/6" = "5", "5/5" = "6", "niet gelukt" = "99")
levels(jgz_data$apkt_r3) <- list("3/30" = "1", "3/20" = "2", "3/15 = 3", "3/10" = "4", "3/6" = "5", "3/5" = "6", 
                                 "3/4" = "7", "3/3" = "8", "niet gelukt" = "99")
levels(jgz_data$apkt_l3) <- list("3/30" = "1", "3/20" = "2", "3/15 = 3", "3/10" = "4", "3/6" = "5", "3/5" = "6", 
                                 "3/4" = "7", "3/3" = "8", "niet gelukt" = "99")
levels(jgz_data$apkt_r4) <- list("4/30" = "1", "4/20" = "2", "4/15 = 3", "4/10" = "4", "4/6" = "5", "4/5" = "6", 
                                 "4/4" = "7", "4/3" = "8", "niet gelukt" = "99")
levels(jgz_data$apkt_l4) <- list("4/30" = "1", "4/20" = "2", "4/15 = 3", "4/10" = "4", "4/6" = "5", "4/5" = "6", 
                                 "4/4" = "7", "4/3" = "8", "niet gelukt" = "99")
levels(jgz_data$apkt_r5) <- list("5/30" = "1", "5/20" = "2", "5/15 = 3", "5/10" = "4", "5/6" = "5", "5/5" = "6", 
                                 "5/4" = "7", "5/3" = "8", "niet gelukt" = "99")
levels(jgz_data$apkt_l5) <- list("5/30" = "1", "5/20" = "2", "5/15 = 3", "5/10" = "4", "5/6" = "5", "5/5" = "6", 
                                 "5/4" = "7", "5/3" = "8", "niet gelukt" = "99")
levels(jgz_data$lh_r) <- list("0,10" = "1", "0,16" = "2", "0,25" = "3", "0,40" = "4", "0,50" = "5", "0,63" = "6",
                              "0,80" = "7", "1,00" = "8", "niet gelukt" = "99")
levels(jgz_data$lh_l) <- list("0,10" = "1", "0,16" = "2", "0,25" = "3", "0,40" = "4", "0,50" = "5", "0,63" = "6",
                              "0,80" = "7", "1,00" = "8", "niet gelukt" = "99")
levels(jgz_data$lc_r) <- list("0,1" = "1", "0,12" = "2", "0,15" = "3", "0,2" = "4", "0,25" = "5", "0,3" = "6",
                              "0,4" = "7", "0,5" = "8", "0,65" = "9","0,8" = "10", "1,0" = "11", "niet gelukt" = "99")
levels(jgz_data$lc_l) <- list("0,1" = "1", "0,12" = "2", "0,15" = "3", "0,2" = "4", "0,25" = "5", "0,3" = "6",
                              "0,4" = "7", "0,5" = "8", "0,65" = "9","0,8" = "10", "1,0" = "11", "niet gelukt" = "99")
levels(jgz_data$scr_psp) <- list("ASQ" = "1", "BITSEA" = "2", "DMO-protocol" = "3", "KIPPPI 0-1" = "4", "KIPPPI 1-4" = "5",
                                 "SDQ (vanaf 3 jaar)" = "6", "SPARK" = "7", "SDQ 5 jaar" = "8", "SDQ 7-12 jaar" = "9",
                                 "SDQ 13/14 jaar" = "10", "anders" = "98")
levels(jgz_data$zin2w) <- list( "+" = "1", "-" = "2", "M" = "3")   # wat is de M
levels(jgz_data$pop6) <- list( "+" = "1", "-" = "2", "M" = "3")   
levels(jgz_data$noemt) <- list( "+" = "1", "-" = "2", "M" = "3")   
levels(jgz_data$wijstaan) <- list( "+" = "1", "-" = "2")
levels(jgz_data$zin3w) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$verstaan1) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$spontaan) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$vragen1) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$verstaan2) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$vragen2) <- list( "+" = "1", "-" = "2", "M" = "3")
levels(jgz_data$r_gd_vve) <- list(kinderopvang = "1", financieel = "2", "geen belangstelling" = "3", 
                                  afstand = "4", wachtlijst = "5", anders = "98")


### 2. Provide variables with a label for clarification 
label(jgz_data$leeftijd) <- "leeftijd kind in dagen"
label(jgz_data$woonplaats) <- "woonplaats"
label(jgz_data$gem) <- "gemeente"
label(jgz_data$gebl1) <- "geboorteland biologische ouder 1"
label(jgz_data$gebl2) <- "geboorteland biologische ouder 2"
label(jgz_data$schoolnr) <- "schoolnummer"
label(jgz_data$visus_c) <- "conclusie visusbepaling"
label(jgz_data$dat_a_str) <- "datum activiteit (jaartal eerste vier posities, maand twee posities daarna), dag"
label(jgz_data$dat_a2_str) <- "datum indicatie contactmoment (jaartal eerste vier posities, maand twee posities"
label(jgz_data$org) <- "organisatienummer"
label(jgz_data$postcode) <- "postcode in vier cijfers"
label(jgz_data$geslacht) <- "geslacht"
label(jgz_data$i_cm) <- "indicatie contactmoment"
label(jgz_data$st_a) <- "status activiteit 1 uitgevoerd/verschenen op afspraak"
label(jgz_data$lengte) <- "lengte in millimeters"
label(jgz_data$gewicht) <- "gewicht in grammen"
label(jgz_data$lft2) <- "leeftijd indicatie contactmoment"
label(jgz_data$so_a2) <- "soort activiteit indicatie contactmoment"
label(jgz_data$jaartal) <- "jaartal van datum activiteit"
label(jgz_data$kindmogelijknietunieknr2) <- "zelfde kindnummers, maar verschillend geslacht"
label(jgz_data$maanden) <- "maand waarop de activiteit plaats vond"

label(jgz_data$bril) <- "bril/lenzen dragend"
label(jgz_data$oog_o) <- "oogonderzoek uitgevoerd"
label(jgz_data$apk_r) <- "APK-kaart uitslag rechts"
label(jgz_data$apk_l) <- "APK-kaart uitslag links"
label(jgz_data$apkt_r3) <- "APK-TOV kaart 3 meter uitslag rechts"
label(jgz_data$apkt_l3) <- "APK-TOV kaart 3 meter uitslag links"
label(jgz_data$apkt_r4) <- "APK-TOV kaart 4 meter uitslag rechts"
label(jgz_data$apkt_l4) <- "APK-TOV kaart 4 meter uitslag links"
label(jgz_data$apkt_r5) <- "APK-TOV kaart 5 meter uitslag rechts"
label(jgz_data$apkt_l5) <- "APK-TOV kaart 5 meter uitslag links"
label(jgz_data$lh_r) <- "LH-kaart uitslag rechts"
label(jgz_data$lh_l) <- "LH-kaart uitslag links"
label(jgz_data$lc_r) <- "Landolt-C kaart uitslag rechts"
label(jgz_data$lc_l) <- "Landolt-C kaart uitslag links"

label(jgz_data$scr_psp) <- "instrument psychosociale problemen"
label(jgz_data$sc_ep) <- "emotionele problemen"
label(jgz_data$sc_gp) <- "gedragsproblemen"
label(jgz_data$sc_pl) <- "problemen met leeftijdsgenoten"
label(jgz_data$sc_ha) <- "hyperactiviteit"
label(jgz_data$sdq_t) <- "sdq totaalscore"
label(jgz_data$sc_psg) <- "pro-sociaal gedrag"
label(jgz_data$sdq_i) <- "sdq impactscore"

label(jgz_data$srt_o) <- "soort onderzoek"
label(jgz_data$zin2w) <- "41. Zegt 'zinnen' van 2 woorden "
label(jgz_data$pop6) <- "42. Wijst 6 lichaamsdelen aan bij pop"
label(jgz_data$noemt) <- "43. Noemt zichzelf 'mij' of 'ik'"
label(jgz_data$wijstaan) <- "44. Wijst 5 plaatjes aan in boek"
label(jgz_data$zin3w) <- "45. Zegt 'zinnen' van 3 of meer woorden"
label(jgz_data$verstaan1) <- "46. Is verstaanbaar voor bekenden"
label(jgz_data$spontaan) <- "47. Praat spontaan over gebeurtenissen thuis/speelzaal"
label(jgz_data$vragen1) <- "48. Stelt vragen naar 'wie', 'wat', 'waar', 'hoe'"
label(jgz_data$verstaan2) <- "49. Is goed verstaanbaar voor onderzoeker"
label(jgz_data$vragen2) <- "50. Stelt vragen naar 'hoeveel', 'wanneer', 'waarom'"

label(jgz_data$zwduur) <- "Zwangerschapsduur in dagen"
label(jgz_data$r_gd_vve) <- "Reden geen deelname aan VVE"

label(jgz_data$d_so_a_miss) <- "soort activiteit contactmoment missing"
label(jgz_data$d_so_a_2) <- "huisbezoek 2 weken"
label(jgz_data$d_so_a_3) <- "contactmoment 4 weken"
label(jgz_data$d_so_a_4) <- "contactmoment 8 weken"
label(jgz_data$d_so_a_5) <- "contactmoment 3 maanden"
label(jgz_data$d_so_a_6) <- "contactmoment 4 maanden"
label(jgz_data$d_so_a_7) <- "contactmoment 6 maanden"
label(jgz_data$d_so_a_9) <- "contactmoment 9 maanden"
label(jgz_data$d_so_a_10) <- "contactmoment 11 maanden"
label(jgz_data$d_so_a_11) <- "contactmoment 14 maanden"
label(jgz_data$d_so_a_12) <- "contactmoment 18 maanden"
label(jgz_data$d_so_a_13) <- "contactmoment 2 jaar"
label(jgz_data$d_so_a_14) <- "contactmoment 3 jaar"
label(jgz_data$d_so_a_15) <- "contactmoment 3,9 jaar"
label(jgz_data$d_so_a_16) <- "contactmoment groep 2"
label(jgz_data$d_so_a_17) <- "contactmoment groep 7"
label(jgz_data$d_so_a_19) <- "contactmoment klas 2"
label(jgz_data$d_so_a_20) <- "contactmoment speciaal onderwijs"
label(jgz_data$d_so_a_22) <- "contactmoment op indicatie"
label(jgz_data$d_so_a_25) <- "spreekuur (0-19)"

label(jgz_data$d_i_jgz_miss) <- "indruk JGZ-professional gewicht/lengte missing"
label(jgz_data$d_i_jgz_1) <- "ondergewicht, indruk JGZ-professional gewicht/lengte"
label(jgz_data$d_i_jgz_2) <- "normaal gewicht, indruk JGZ-professional gewicht/lengte"
label(jgz_data$d_i_jgz_3) <- "overgewicht, indruk JGZ-professional gewicht/lengte"
label(jgz_data$d_indi_miss) <- "indicatie missing"
label(jgz_data$d_indi_9) <- "indicatie gewicht"
label(jgz_data$d_indi_10) <- "indicatie psychosociale/emotionele ontwikkeling"
label(jgz_data$d_indi_12) <- "indicatie spraak- taalontwikkeling"
label(jgz_data$d_indi_18) <- "indicatie amblyopie"
label(jgz_data$d_indi_19) <- "indicatie oogpathologie"
label(jgz_data$d_indi_20) <- "indicatie visusafwijking"
label(jgz_data$d_inter_miss) <- "interventie missing"
label(jgz_data$d_inter_1) <- "voorlichting interventie"
label(jgz_data$d_inter_2) <- "advies interventie"
label(jgz_data$d_inter_3) <- "consultatie/inlichtingen vragen interventie"
label(jgz_data$d_inter_4) <- "extra (medisch) onderzoek interventie"
label(jgz_data$d_inter_5) <- "melding interventie"
label(jgz_data$d_inter_6) <- "verwijzing interventie"
label(jgz_data$d_inter_7) <- "begeleiding interventie"
label(jgz_data$d_inter_98) <- "anders interventie"
label(jgz_data$d_verwijs_miss) <- "verwijzing missing"
label(jgz_data$d_verwijs_1) <- "huisarts verwijzing"
label(jgz_data$d_verwijs_2) <- "kinderarts verwijzing"
label(jgz_data$d_verwijs_3) <- "kinderfysiotherapeut/oefentherapeut verwijzing"
label(jgz_data$d_verwijs_4) <- "logopedist verwijzing"
label(jgz_data$d_verwijs_5) <- "maatschappelijk werk verwijzing"
label(jgz_data$d_verwijs_6) <- "veilig thuis verwijzing"
label(jgz_data$d_verwijs_7) <- "bureau jeugdzorg verwijzing"
label(jgz_data$d_verwijs_8) <- "GGZ verwijzing"
label(jgz_data$d_verwijs_9) <- "MEE/Integrale vroeghulp verwijzing"
label(jgz_data$d_verwijs_10) <- "opvoedbureau/pedagoog verwijzing"
label(jgz_data$d_verwijs_11) <- "VVE verwijzing"
label(jgz_data$d_verwijs_12) <- "Home Start (Humanitas) verwijzing"
label(jgz_data$d_verwijs_13) <- "audiologisch Centrum verwijzing"
label(jgz_data$d_verwijs_14) <- "peutergym/sportclub verwijzing"
label(jgz_data$d_verwijs_15) <- "KNO-arts verwijzing"
label(jgz_data$d_verwijs_16) <- "andere medisch specialist verwijzing"
label(jgz_data$d_verwijs_17) <- "lactatiekundige (extern) verwijzing"
label(jgz_data$d_verwijs_19) <- "cursus/groepsbehandeling verwijzing"
label(jgz_data$d_verwijs_20) <- "dermatoloog verwijzing"
label(jgz_data$d_verwijs_21) <- "di?tist verwijzing"
label(jgz_data$d_verwijs_23) <- "oogarts/optometrist/optici?n/orthopist verwijzing"
label(jgz_data$d_verwijs_24) <- "orthopeed verwijzing"
label(jgz_data$d_verwijs_25) <- "psycholoog verwijzing"
label(jgz_data$d_verwijs_26) <- "psz/KDV/BSO verwijzing"
label(jgz_data$d_verwijs_27) <- "radioloog verwijzing"
label(jgz_data$d_verwijs_28) <- "tandarts verwijzing"
label(jgz_data$d_verwijs_29) <- "video home training (extern) verwijzing"
label(jgz_data$d_verwijs_98) <- "anders verwijzing"

label(jgz_data$d_opl1_miss) <- "opleiding ouder/verzorger 1 missing"
label(jgz_data$d_opl1_0) <- "onbekend opleiding ouder/verzorger 1 "
label(jgz_data$d_opl1_1) <- "geen opleiding opleiding ouder/verzorger 1" #(lagere school niet afgemaakt)
label(jgz_data$d_opl1_2) <- "basisonderwijs opleiding ouder/verzorger 1" #(lagere school, basisonderwijs, speciaalbasisonderwijs) 
label(jgz_data$d_opl1_3) <- "VSO-MLK/I(V)BO/VMBO-LWOO/praktijkonderwijs opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_4) <- "LBO/VBO/VMBO-BBL&KBL opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_5) <- "MAVO/VMBO/GL&TL opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_6) <- "MBO opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_7) <- "HAVO/VWO opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_8) <- "HBO/HTS/HEAO opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_9) <- "WO opleiding ouder/verzorger 1"
label(jgz_data$d_opl1_98) <- "anders opleiding ouder/verzorger 1"
label(jgz_data$d_opl2_miss) <- "opleiding ouder/verzorger 2 missing"
label(jgz_data$d_opl2_0) <- "onbekend opleiding ouder/verzorger 2 "
label(jgz_data$d_opl2_1) <- "geen opleiding opleiding ouder/verzorger 2" #(lagere school niet afgemaakt)
label(jgz_data$d_opl2_2) <- "basisonderwijs opleiding ouder/verzorger 2" #(lagere school, basisonderwijs, speciaalbasisonderwijs) 
label(jgz_data$d_opl2_3) <- "VSO-MLK/I(V)BO/VMBO-LWOO/praktijkonderwijs opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_4) <- "LBO/VBO/VMBO-BBL&KBL opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_5) <- "MAVO/VMBO/GL&TL opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_6) <- "MBO opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_7) <- "HAVO/VWO opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_8) <- "HBO/HTS/HEAO opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_9) <- "WO opleiding ouder/verzorger 2"
label(jgz_data$d_opl2_98) <- "anders opleiding ouder/verzorger 2"

label(jgz_data$d_kmn_uniek_miss) <- "kindermogelijknummer dubbel"
label(jgz_data$d_kmn_uniek_0) <- "0 zelfde kindnummers, maar verschillende postcodes (kind kan verhuisd zijn)"
label(jgz_data$d_kmn_uniek_1) <- "1 zelfde kindnummers, maar verschillend postcodes (kind kan verhuisd zijn)"
label(jgz_data$d_dubbel_miss) <- "Resultaten van kindnummer komt meerdere keren voor op dezelfde leeftijd"
label(jgz_data$d_dubbel_0) <- "0 Resultaten van kindnummer komt meerdere keren voor op dezelfde leeftijd"
label(jgz_data$d_dubbel_1) <- "1 Resultaten van kindnummer komt meerdere keren voor op dezelfde leeftijd"

label(jgz_data$d_bst_nl_miss) <- "Beoordeling spraak- en taalontwikkeling NL missing"
label(jgz_data$d_bst_nl_1) <- "leeftijdsadequaat of sneller, beoordeling spraak- en taalontwikkeling NL"
label(jgz_data$d_bst_nl_2) <- "langzaam, Beoordeling spraak- en taalontwikkeling NL"
label(jgz_data$d_taalomg_miss) <- "taalomgeving stimulerend"
label(jgz_data$d_taalomg_1) <- "voldoende, taalomgeving stimulerend"
label(jgz_data$d_taalomg_2) <- "matig, taalomgeving stimulerend"
label(jgz_data$d_taalomg_3) <- "onvoldoende, taalomgeving stimulerend"
label(jgz_data$d_meertaal_miss) <- "meertaligheid"
label(jgz_data$d_meertaal_1) <- "geen meertaligheid"
label(jgz_data$d_meertaal_2) <- "simultane meertaligheid"
label(jgz_data$d_meertaal_3) <- "successieve meertaligheid"
label(jgz_data$d_d_vvenum_miss) <- "deelname VVE"
label(jgz_data$d_d_vvenum_1) <- "ja deelname VVE"
label(jgz_data$d_d_vvenum_2) <- "nee deelname VVE"

# save the data set with labels
saveRDS(jgz_data, "H:/Mirthe/case_jgz/repo/jgz_data_labels.rds") 


##################################################################################
#### 3. Transform the JGZ data ####
# now that we have specified the class and the labels of the variables for clarity,
# we can move to the next step and transform the JGZ data
setwd("H:/Mirthe")

# read in the JGZ data file with added labels created in 01_jgz_preprocess_labels.R (not saved due to limited storage)
jgz_data <- readRDS("case_jgz/repo/jgz_data_labels.rds")
# look at the JGZ data
glimpse(jgz_data)

# input the desired config file here: this file specifies the microdata filepaths and selected years
cfg_file <- "H:/Mirthe/data/data_scripts/preprocessing_set_V2.yml"

# load the configuration 
cfg <- config::get("data_preparation", file = cfg_file)
loc <- config::get("file_locations", file = cfg_file)

#### 3.1 Add parent information of the children in the JGZ data ####
### Identify the parents of the children in the JGZ data
jgz_data <- left_join(
  x = jgz_data,
  y = read_sav(file.path(loc$data_folder, loc$kindouder_data)) %>% 
    select(-(c("XKOPPELNUMMER"))) %>% 
    as_factor(only_labelled = TRUE, levels = "values"),
  by = c("RINPERSOONS" = "RINPERSOONS", "RINPERSOON" = "RINPERSOON")) %>% 
  rename(rins_mo = RINPERSOONSMa,
         rin_mo = RINPERSOONMa,
         rins_fa = RINPERSOONSpa,
         rin_fa = RINPERSOONpa) 
# change empty fields to NA 
jgz_data <- jgz_data %>% 
  mutate(rin_mo = ifelse(rin_mo == "---------", NA, rin_mo),
         rin_fa = ifelse(rin_fa == "---------", NA, rin_fa))


### Join the PRNL data for birth records of the child 
# rename the rins_mo and rin_mo to distinguish them from mothers recorded in GBA 
prnl_dat <- readRDS("data/prnl_dat.rds") %>% 
  rename(rins_mo_pr = rins_mo,
         rin_mo_pr = rin_mo)
# join the prnl data
jgz_data <- jgz_data %>% 
  left_join(y = prnl_dat,
            by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin"))

sum(is.na(jgz_data$rin_mo))
# several rin_mo's are missing in the perined dataset because this contains only data from 2004-2016 
sum(is.na(jgz_data$rin_mo_pr))
rm(prnl_dat)


### add information about parents' birthday, origin and generation as recorded in GBAPERSOONTAB
gba_dat <- readRDS("data/gba_dat.rds")
jgz_data <- jgz_data %>% 
  left_join(y = gba_dat,
            # rin_mo in jgz_data is rin in gba_dat
            by = c("rins_mo" = "rins", "rin_mo" = "rin")) %>% 
  rename_at(vars(c("GBA_origin", "GBA_generation", "birthday")), 
            function(x) paste0(x, "_mo")) %>% 
  left_join(y = gba_dat,
            # rin_fa in jgz_data is rin in gba_dat
            by = c("rins_fa" = "rins", "rin_fa" = "rin")) %>% 
  rename_at(vars(c("GBA_origin", "GBA_generation", "birthday")), 
            function(x) paste0(x, "_fa")) %>% 
  # add information about the JGZ kids as recorded in GBAPERSOONTAB
  left_join(y = gba_dat,
            by = c("RINPERSOONS" = "rins", "RINPERSOON" = "rin")) %>% 
  rename_at(vars(c("GBA_origin", "GBA_generation", "birthday")), 
            function(x) paste0(x, "_kid"))

sum(is.na(jgz_data$birthdate)) # birthday of the child from Perined
sum(is.na(jgz_data$birthday_kid)) # birtday from GBAPERSOONTAB
rm(gba_dat)

### check missingness in the data 
# check NAs in the data 
colSums(is.na(jgz_data))
# change empty observations to NA 
jgz_data <- jgz_data %>% 
  mutate(across(everything(), ~na_if(.x, " ")))

# check NAs again after adaptation 
colSums(is.na(jgz_data))


#### 3.2 Create dummies for the contactmomenten ####
# contactmoment is a consultation moment at the Preventive Youth Health Care centre to monitor the growth and
# development of the child during which the child is within a certain age range 
# we create these dummies based on the age of the child at the time of the consult 
summary(jgz_data$leeftijd)

# there are two negative values for age in the data, we assume the "-" is a registration mistake
jgz_data <- jgz_data %>% 
  mutate(leeftijd = ifelse(leeftijd < 0, abs(leeftijd), leeftijd))

# look at age (in days)
ggplot(data = jgz_data, mapping = aes(x = leeftijd)) +
  geom_bar() +
  xlim(0,2250) +
  ylim(0,3000) +
  xlab("Age (days)")+
  ylab("Observations (N)") 

table(jgz_data$jaartal)
# create variable for age of the child in months and years for each record
jgz_data <- jgz_data %>% 
  mutate(t_contact = make_date(year = jaartal, month = maanden),
         lft_maanden = ifelse(!is.na(birthday_kid), (interval(ymd(birthday_kid), ymd(t_contact)) %/% months(1)), round(leeftijd/30, digits = 0)),
         # age in whole years, 11 months is 0 years, 12 months is 1 year etc.
         lft_jaren2 = interval(ymd(birthday_kid), ymd(t_contact)) %/% years(1),
         # in this case age in years is missing for every kid not in GBA, therefore also create
         # age in days divided by 365 days, creating a more detailed indication of age in years
         lft_jaren = (leeftijd/365)) 

# create new dummy variables for the contact moments based on the age of the children 
jgz_data <- jgz_data %>% 
  mutate(cm_4wks = ifelse(leeftijd > 0 & leeftijd <= 46, 1, 0),
         cm_8wks = ifelse(leeftijd > 46 & leeftijd <= 79, 1, 0),
         cm_3months = ifelse(leeftijd > 79 & leeftijd <= 111, 1, 0),
         cm_4months = ifelse(leeftijd > 111 & leeftijd <= 169, 1, 0),
         cm_6months = ifelse(leeftijd > 169 & leeftijd <= 261, 1, 0),
         cm_9months = ifelse(leeftijd > 261 & leeftijd <= 320, 1, 0),
         cm_11months = ifelse(leeftijd > 320 & leeftijd <= 405, 1, 0),
         cm_14months = ifelse(leeftijd > 405 & leeftijd <= 513, 1, 0),
         cm_18months = ifelse(leeftijd > 513 & leeftijd <= 677, 1, 0),
         cm_2years = ifelse(leeftijd > 677 & leeftijd <= 1038, 1, 0),
         cm_3years = ifelse(leeftijd > 1038 & leeftijd <= 1319, 1, 0),
         cm_3years9m = ifelse(leeftijd > 1319 & lft_maanden <= 60, 1, 0),
         cm_groep2 = ifelse(lft_maanden > 60 & lft_maanden <= 93, 1, 0),
         cm_groep7 = ifelse(lft_maanden > 93 & lft_maanden <= 153, 1, 0),
         cm_klas2 = ifelse(lft_maanden > 153 & lft_maanden <= 902, 1, 0))

# create variable for age of parents at the time of birth 
jgz_data <- jgz_data %>% 
  mutate(age_at_birth_mo = trunc((birthday_mo %--% birthday_kid) / years(1)),
         age_at_birth_fa = trunc((birthday_fa %--% birthday_kid) / years(1)))

summary(jgz_data$age_at_birth_mo)
summary(jgz_data$age_at_birth_fa)

# add premature birth as a predictor variable
sum(is.na(jgz_data$zwduur)) 
# almost all observations for zwduur are NA so we use PRNL_gestational age 
sum(is.na(jgz_data$PRNL_gestational_age) & !is.na(jgz_data$zwduur))
# there are some individuals for who PRNL_gestational_age is missing but zwduur isn't, then we use zwduur
# we define a preterm birth as a birth of <37 weeks of gestation and >24 weeks of gestation (because of the registration policy)
jgz_data <- jgz_data %>% 
  mutate(premature_birth = ifelse(!is.na(PRNL_gestational_age) & PRNL_gestational_age_week > 24 & PRNL_gestational_age_week < 37, 1, 
                                  ifelse(is.na(PRNL_gestational_age) & (zwduur/7) > 24 & (zwduur/7) < 37, 1, 0)),
         premature_birth = as_factor(premature_birth))

table(jgz_data$premature_birth)

#### 3.3 Working with length and weight records ####
# there are some unrealistic values in the jgz_data for length and weight 
summary(jgz_data$lengte)
summary(jgz_data$gewicht)

# correct for commonly made recording mistakes (such as too many decimals)
# these corrections were suggested by youth health researchers who spoke to GGD practitioners about recording practices
jgz_data <- jgz_data %>%
  mutate(lengte = ifelse(lengte > 2500, lengte/10, lengte)) %>% 
  mutate(lengte = ifelse(lengte > 2500, lengte/10, lengte)) %>% 
  mutate(lengte = ifelse(lengte < 20, lengte*100, lengte)) %>%
  mutate(lengte = ifelse(lengte < 200, lengte*10, lengte)) %>%
  mutate(gewicht = ifelse(gewicht > 1000000, gewicht/1000, gewicht)) %>%
  mutate(gewicht = ifelse(gewicht > 1000000, gewicht/1000, gewicht)) %>%
  mutate(gewicht = ifelse(gewicht > 200000, gewicht/10, gewicht)) %>% 
  mutate(lengte = ifelse(lengte == 0, NA, lengte), 
         gewicht = ifelse(gewicht == 0, NA, gewicht))   

# the length and weight values looks more realistic 
summary(jgz_data$lengte)
summary(jgz_data$gewicht)
hist(jgz_data$lengte)
hist(jgz_data$gewicht)

# add variables length in centimeters, meters, and weight in kg for clarity and (potential) future use
jgz_data <- jgz_data %>% 
  mutate(
    lengte_m = (lengte*0.001),
    lengte_cm = (lengte*0.1),
    gewicht_kg = (gewicht*0.001))

# create the variable BMI 
jgz_data <- jgz_data %>% 
  mutate(BMI = gewicht_kg/(lengte_m^2))
# alternative; calculating BMI this way should give you the same results
# test <- jgz_data %>% 
#   mutate(BMI2 = (gewicht*0.001)/((lengte*0.001)^2))

# look at BMI 
summary(jgz_data$BMI)
jgz_data %>% ggplot(aes(BMI))+
  geom_histogram()+
  xlim(10,30)


### standardize length, weight and BMI with y2z from the AGD package
# y2z is conditional on ethnic group, sex and age, so we regroup the gebl variables in subpopulation categories: nederland N, marokko M, Turkijke T, and other N (as default)
jgz_data <- jgz_data %>% 
  mutate(sub_ethn = ifelse(gebl1 == "MAROKKO" | gebl2 == "MAROKKO", "M", 
                           ifelse(gebl1 == "TURKIJE" | gebl2 == "TURKIJE", "T",
                                  ifelse(gebl1 == "NEDERLAND" | gebl2 == "NEDERLAND", "N", "N")))) %>%
  # set default to "N", because otherwise z scores will be missing for all cases where sub is NA
  mutate(sub_ethn = ifelse(is.na(sub_ethn), "N", sub_ethn),
         sub_ethn = as_factor(sub_ethn),
         # relabel the levels of the variable sex_z (instead of using geslacht)
         sex_z = recode(geslacht, "mannelijk" = "M", "vrouwelijk" = "F", "onbekend" = "NA"),
         #  DO NOT USE LABELED NUMERIC VARIABLES or y2z won't work
         lengte_cm = as.numeric(lengte_cm),
         lft_jaren = as.numeric(lft_jaren),
         gewicht_kg = as.numeric(gewicht_kg),
         BMI = as.numeric(BMI))

# use to function y2z to transform length, weight or BMI to zscore conditional on age, sex and ethnic group
jgz_data <- jgz_data %>% 
  mutate(lengte_znl = y2z(y=lengte_cm, x=lft_jaren, sex = sex_z, sub = sub_ethn, ref = get("nl4.hgt")), # check if new references are available
         gewicht_znl = y2z(y=gewicht_kg, x=lft_jaren, sex = sex_z, sub = sub_ethn, ref = get("nl4.wgt")),
         bmi_znl = y2z(y=BMI, x=lft_jaren, sex = sex_z, sub = sub_ethn, ref = get("nl4.bmi")))

# the standardised scores can be used to exclude outliers from the dataset; look at potential outliers
sum(jgz_data$lengte_znl > 5, na.rm = TRUE)
sum(jgz_data$lengte_znl < -5, na.rm = TRUE)
sum(jgz_data$gewicht_znl > 5, na.rm = TRUE)
sum(jgz_data$gewicht_znl < -5, na.rm = TRUE)
sum(jgz_data$bmi_znl > 5, na.rm = TRUE)
sum(jgz_data$bmi_znl < -5, na.rm = TRUE)


#### 3.4 Transforming demographic variables ####

### classify JGZ birth countries and GBA origin and generation variables 
table(jgz_data$gebl1)
table(jgz_data$gebl2)

# we transform the categorical variable birth country because we don't want to include all countries as categories, there might not be enough records per country
# therefore we group birth countries and create broader categories
# use one of the reference files from CBS: in this case LANDAKTUEELREFV13. You can use different references for categorizing birth countries
origin_ref <- read_sav(loc$origin_ref, 
                       # we group using ethnic group or landaktueelachterstandbo categories 
                       col_select = c("LAND", "ETNGRP", "LANDAKTUEELACHTERSTANDBO")) %>%  
  mutate(LAND_code = as_factor(LAND),
         LAND_code = tolower(LAND_code),
         ETNGRP = ifelse(ETNGRP == "Onbekend", NA, ETNGRP),
         LANDAKTUEELACHTERSTANDBO = ifelse(LANDAKTUEELACHTERSTANDBO == "Onbekend", NA, LANDAKTUEELACHTERSTANDBO)) %>% 
  rename(LAND_ACHTS = LANDAKTUEELACHTERSTANDBO,
         LAND_ETNG = ETNGRP) %>% 
  add_row(LAND = "7011", LAND_code = "nederlandse antillen", LAND_ETNG = "4", LAND_ACHTS = "7") %>% 
  add_row(LAND = "5020", LAND_code = "ethiopi?", LAND_ETNG = "5", LAND_ACHTS = "6") %>% 
  add_row(LAND = "6039", LAND_code = "groot-brittanni?", LAND_ETNG = "6", LAND_ACHTS = "2") %>%
  add_row(LAND = "6013", LAND_code = "somali?", LAND_ETNG = "5", LAND_ACHTS = "6") %>%
  add_row(LAND = "5014", LAND_code = "zuid-afrika", LAND_ETNG = "5", LAND_ACHTS = "6") %>% 
  add_row(LAND = "9049", LAND_code = "sovjet-unie", LAND_ETNG = "6", LAND_ACHTS = "3") %>%
  mutate(LAND_code = as_factor(LAND_code),
         LAND_ETNG = as_factor(LAND_ETNG),
         LAND_ACHTS = as_factor(LAND_ACHTS)) 

# first for gebl1 and gebl2 from the JGZ data
jgz_data <- jgz_data %>% 
  mutate(gebl1 = tolower(gebl1),
         gebl2 = tolower(gebl2)) %>% 
  left_join(origin_ref, by = c("gebl1" = "LAND_code"), suffix = c("_gebl1", "_gebl2")) %>%
  rename(gebl1c = LAND) %>% 
  left_join(origin_ref, by = c("gebl2" = "LAND_code"), suffix = c("_gebl1", "_gebl2")) %>%
  rename(gebl2c = LAND) %>% 
  # set to factor because tolower makes it a character
  mutate(gebl1 = as_factor(gebl1),
         gebl2 = as_factor(gebl2))
# second for GBA origin from the CBS microdata
jgz_data <- jgz_data %>% 
  mutate(GBA_origin_mo = tolower(GBA_origin_mo),
         GBA_origin_fa = tolower(GBA_origin_fa)) %>% 
  left_join(origin_ref, by = c("GBA_origin_mo" = "LAND_code"), suffix = c("_mo", "_fa")) %>%
  left_join(origin_ref, by = c("GBA_origin_fa" = "LAND_code"), suffix = c("_mo", "_fa")) %>% 
  # set to factor because tolower makes it a character
  mutate(GBA_origin_mo = as_factor(GBA_origin_mo),
         GBA_origin_fa = as_factor(GBA_origin_fa))
# remove files you no longer need
rm(origin_ref)

### Add regional data 
# add classification of stedelijkheid as an predictor; use the CBS HULPbestanden for postal code 4  
pc4_sted <- read_excel("K:/Utilities/HULPbestanden/PC4/cbs_pc4_2020_v1.xlsx",
                       range = cell_rows(c(9, 11:4078)))
pc4_sted <- pc4_sted[-c(1),] %>% select(PC4, STED) %>% 
  mutate(STED = ifelse(STED == "-99997", NA, STED),
         PC4 = as.factor(PC4),
         STED = as.factor(STED))

# add municipality code (gem) to categories residences as part of a NL COROP region; use the CBS HULPbestanden for PWR  
pc4_gem <- read_sav("K:/Utilities/HULPbestanden/PWR/PWR2020.sav", 
                    col_select = c("postc", "gemc")) %>% # select all variables to join labels
  mutate(postc = as.factor(postc),
         gemc = as.factor(gemc)) # municipality code 
# use HULPbestanden Gemeenten (municipality) and COROP
gem_corop <- read_excel("K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx",
                        range = cell_cols("CA:CB")) %>% 
  mutate(GM2020 = as.factor(GM2020), 
         COROP2020 = as.factor(COROP2020)) %>% 
  unique()
# join all the regional data 
jgz_data <- jgz_data %>% 
  mutate(postcode = as.factor(postcode)) %>% 
  left_join(pc4_sted, by = c("postcode" = "PC4")) %>% 
  left_join(pc4_gem, by = c("postcode" = "postc")) %>% 
  left_join(gem_corop, by = c("gemc" = "GM2020"))
# remove files you no longer need
rm(pc4_sted, pc4_gem, gem_corop)

### reclassify education (opl) data from JGZ 
jgz_data <- jgz_data %>% 
  mutate_at(names(jgz_data %>% select(c(starts_with("d_opl1") | starts_with("d_opl2")))), 
            function(x) ifelse(is.na(x), 0, x)) %>% 
  mutate(opl1 = ifelse(d_opl1_9 == 1, "9_WO", 
                       ifelse(d_opl1_8 == 1, "8_HBO",
                              ifelse(d_opl1_7 == 1, "7_HAVOVWO",
                                     ifelse(d_opl1_6 == 1, "6_MBO",
                                            ifelse(d_opl1_5 == 1, "5_MAVO",
                                                   ifelse(d_opl1_4 == 1, "4_LBO",
                                                          ifelse(d_opl1_3 == 1, "3_praktijk",
                                                                 ifelse(d_opl1_2 == 1, "2_basis",
                                                                        ifelse(d_opl1_1 == 1, "1_geen",
                                                                               ifelse(d_opl1_98 == 1, "98_anders",
                                                                                      ifelse(d_opl1_0 == 1, "0_onbekend", 
                                                                                             ifelse(d_opl1_miss == 1, "miss", NA))))))))))))) %>% 
  mutate(opl1 = factor(opl1, ordered = TRUE,
                       levels = c("1_geen", "2_basis", "3_praktijk", "4_LBO", "5_MAVO", "6_MBO", "7_HAVOVWO", "8_HBO", "9_WO"))) %>% 
  mutate(opl2 = ifelse(d_opl2_9 == 1, "9_WO", 
                       ifelse(d_opl2_8 == 1, "8_HBO",
                              ifelse(d_opl2_7 == 1, "7_HAVOVWO",
                                     ifelse(d_opl2_6 == 1, "6_MBO",
                                            ifelse(d_opl2_5 == 1, "5_MAVO",
                                                   ifelse(d_opl2_4 == 1, "4_LBO",
                                                          ifelse(d_opl2_3 == 1, "3_praktijk",
                                                                 ifelse(d_opl2_2 == 1, "2_basis",
                                                                        ifelse(d_opl2_1 == 1, "1_geen",
                                                                               ifelse(d_opl2_98 == 1, "98_anders",
                                                                                      ifelse(d_opl2_0 == 1, "0_onbekend", 
                                                                                             ifelse(d_opl2_miss == 1, "miss", NA))))))))))))) %>% 
  mutate(opl2 = factor(opl2, ordered = TRUE,
                       levels = c("1_geen", "2_basis", "3_praktijk", "4_LBO", "5_MAVO", "6_MBO", "7_HAVOVWO", "8_HBO", "9_WO")))


# save the created dataset 
#saveRDS(jgz_data, "case_jgz/jgz_data.rds")

#### 3.5 Add birth records from Perined ####
# if the Perined data is available to you you can add it to the created data set 

# read the cleaned Perined data
perined_cleaned <- readRDS("Polina/perined_cleaned_2July2023.rds") 

### look at the data
glimpse(perined_cleaned)

# make sure empty Rinpersoon_Moeder fields are recognized as NA
perined_cleaned <- perined_cleaned %>% 
  mutate(Rinpersoon_Moeder = ifelse(Rinpersoon_Moeder == "" | Rinpersoon_Moeder == "---------" | 
                                      Rinpersoon_Moeder == "000000000", NA, Rinpersoon_Moeder), 
         Rinpersoons_Moeder = as_factor(Rinpersoons_Moeder)) 
#check the number of missings
sum(is.na(perined_cleaned$Rinpersoon_Moeder)) # 163

# we do not have perined data for 23748 kids
kid_missing_perined <- jgz_data %>% filter(!RINPERSOON %in% perined_cleaned$Rinpersoon_Kind)
n_distinct(c(kid_missing_perined$RINPERSOON))

# join the JGZ and perined data
jgz_data_p <- jgz_data %>% left_join(perined_cleaned, by = c("RINPERSOONS" = "Rinpersoons_Kind","RINPERSOON" = "Rinpersoon_Kind"))

# look at missing records for the mothers
sum(is.na(jgz_data_p$rin_mo_pr)) # 94242
sum(is.na(jgz_data_p$Rinpersoon_Moeder)) # 64663
sum(!is.na(jgz_data_p$rin_mo_pr) & is.na(jgz_data_p$Rinpersoon_Moeder)) # 20631

# look at cases in which Rinpersoon_Moeder from perined is missing but rin_mo_pr from CBS PRNL is not
mo_missing_perined <- jgz_data_p %>% filter(!is.na(jgz_data_p$rin_mo_pr) & is.na(jgz_data_p$Rinpersoon_Moeder)) %>% 
  select(c(RINPERSOONS, RINPERSOON, rins_mo, rin_mo, rins_mo_pr, rin_mo_pr, rins_fa, rin_fa, Rinpersoons_Moeder, Rinpersoon_Moeder, amww)) %>% 
  unique()

# we make a variable to combine rinpersoon(s) moeder, if 
jgz_data_p <- jgz_data_p %>% 
  mutate(rins_mo_pr_clean = if_else(!is.na(Rinpersoons_Moeder), Rinpersoons_Moeder, rins_mo_pr),
         rin_mo_pr_clean = if_else(!is.na(Rinpersoon_Moeder), Rinpersoon_Moeder, rin_mo_pr))
# check the number of missingness for the combined rin_mo variable
sum(is.na(jgz_data_p$rin_mo_pr_clean)) # 44032


### transform the birth record variables for analysis
jgz_data_p <- jgz_data_p %>% 
  mutate(interpreg_cat = ifelse(interpreg == -88, "nvt",
                                ifelse(interpreg == -99, NA, 
                                       ifelse(interpreg < 0, NA, 
                                              ifelse(interpreg < (30.436*6), "<6 months", 
                                                     ifelse(interpreg >= (30.436*6) & interpreg < (30.436*12), "6-12 months", 
                                                            ifelse(interpreg >= (30.436*12) & interpreg < (30.436*18), "12-18 months", 
                                                                   ifelse(interpreg >= (30.436*18) & interpreg < (30.436*24), "18-24 months",
                                                                          ifelse(interpreg >= (30.436*24) & interpreg < (30.436*30), "24-30 months",
                                                                                 ifelse(interpreg >= (30.436*30), ">30 months", NA))))))))),
         interpreg_cat = factor(interpreg_cat, ordered = T, 
                                levels = c("nvt", "<6 months", "6-12 months", "12-18 months", "18-24 months", "24-30 months", ">30 months")))

jgz_data_p <- jgz_data_p %>% 
  mutate(lft_concep = factor(lft_concep, ordered = TRUE),
         amww_f = factor(amww, ordered = TRUE), 
         amddd1ond_cat = ordered(amddd1ond_cat),
         par_cat = ordered(par_cat), 
         grav_cat = ordered(grav_cat),
         meerling = ifelse(omv_cat == "2" | omv_cat == "3", 1, 0),
         meerling = as_factor(meerling),
         congenafw_ja = as_factor(congenafw_ja),
         vroeg_geb_24_37 = as_factor(vroeg_geb_24_37),
         N_vroeg_24_37 = ordered(N_vroeg_24_37, levels = c("nvt", "0", "1", "2", "3", "geen data")),
         laag_geb_gewicht = as_factor(laag_geb_gewicht),
         N_vooraf_sga = ordered(N_vooraf_sga, levels = c("nvt", "0", "1", "2", "3", "4", "5", "geen data")))


# with the perined data available, use amww and PRNL_gestational_age to create the variable preterm birth
sum(is.na(jgz_data_p$amww))
sum(is.na(jgz_data_p$amww) & !is.na(jgz_data_p$PRNL_gestational_age))
jgz_data_p <- jgz_data_p %>% 
  mutate(premature_birth = ifelse(!is.na(amww) & amww > 24 & amww < 37, 1, 
                                  ifelse(is.na(amww) & PRNL_gestational_age_week > 24 & PRNL_gestational_age_week < 37, 1, 0)),
         premature_birth = as_factor(premature_birth))

# save the created data set 
saveRDS(jgz_data_p, "case_jgz/jgz_data_p.rds")

##################################################################################

#### 4 Specify the doe-moment based on the case ####

# create a variable for the doe-moment 4 months, the moment at which we want to observe our predictors 
jgz_data <- jgz_data %>% 
  mutate(doe_moment_4m = if_else(!is.na(birthdate), ymd(birthdate) %m+% months(4), 
                                 if_else(!is.na(birthday_kid), ymd(birthday_kid) %m+% months(4), NA)))

# create a variable for the doe-moment 2 years, the moment at which we want to observe our predictors 
jgz_data <- jgz_data %>% 
  mutate(doe_moment_2y = if_else(!is.na(birthdate), ymd(birthdate) %m+% months(24),
                                 if_else(!is.na(birthday_kid), ymd(birthday_kid) %m+% months(24), NA)))

# look at how often the doe-moment is missing due to missings in birthdate and birthday
sum(is.na(jgz_data$birthdate))
sum(is.na(jgz_data$birthday_kid))
sum(is.na(jgz_data$doe_moment_4m))
sum(is.na(jgz_data$doe_moment_2y))

# create a new variables, doe_moment 16 weeks and conception of the child, by substracting gestational age in days from the birthdate
sum(is.na(jgz_data$PRNL_gestational_age) & !is.na(jgz_data$zwduur))
sum(is.na(jgz_data$birthdate) & !is.na(jgz_data$birthday_kid))
sum(is.na(jgz_data$birthdate) & is.na(jgz_data$birthday_kid) & !is.na(jgz_data$leeftijd))
sum(is.na(jgz_data$PRNL_gestational_age) & !is.na(jgz_data$birthdate) & !is.na(jgz_data$zwduur))
sum(is.na(jgz_data$PRNL_gestational_age) & is.na(jgz_data$birthdate) & !is.na(jgz_data$birthday_kid) & !is.na(jgz_data$zwduur))

# almost all observations for zwduur are NA so we use amww (from perined)
# if amww is missing we use PRNL_gestational_age, if both are missing we use zwduur
sum(is.na(jgz_data$amww))
sum(is.na(jgz_data$amww) & !is.na(jgz_data$PRNL_gestational_age_week))
sum(is.na(jgz_data$amww) & is.na(jgz_data$PRNL_gestational_age_week) & !is.na(jgz_data$zwduur) & !is.na(jgz_data$birthday_kid))
# there are some individuals for who PRNL_gestational_age is missing but zwduur isn't, then we use zwduur
jgz_data <- jgz_data %>% 
  mutate(zwduur_clean = ifelse(zwduur > 2000, zwduur/10, 
                               ifelse(zwduur > 315 & zwduur < 450, zwduur/10*7, 
                                      ifelse(zwduur > 315, NA, 
                                             ifelse(zwduur < 22, NA, 
                                                    ifelse(zwduur < 45, zwduur*7, zwduur))))),
         zwduur_clean = round(zwduur_clean, digits = 0))

# create the variable conception and doe-moment at 16 weeks gestational age
jgz_data <- jgz_data %>% 
  mutate(conception = if_else((!is.na(birthdate) & !is.na(amww)), ymd(birthdate) %m-% weeks(amww),
                              if_else((is.na(birthdate) & !is.na(amww) & !is.na(birthday_kid)), ymd(birthday_kid) %m-% weeks(amww),
                                      if_else((!is.na(birthdate) & is.na(amww) & !is.na(PRNL_gestational_age_week)), ymd(birthdate) %m-% days(PRNL_gestational_age),
                                              if_else((is.na(birthdate) & is.na(amww) & !is.na(birthday_kid) & !is.na(PRNL_gestational_age_week)), ymd(birthday_kid) %m-% days(PRNL_gestational_age),
                                                      if_else((is.na(amww) & is.na(PRNL_gestational_age_week) & !is.na(zwduur_clean)), ymd(birthday_kid) %m-% days(zwduur_clean), NA))))),
         doe_moment_16w = ymd(conception) %m+% weeks(16))

# use conception to create a variable for age of parents at the time of conception 
jgz_data <- jgz_data %>% 
  mutate(lft_concept_mo = trunc((birthday_mo %--% conception) / years(1)),
         lft_concept_fa = trunc((birthday_fa %--% conception) / years(1)))


#### 5. Select the non-changing, i.e. demographic, records of the child ####
# The JGZ data contains multiple records per child (rows); at each visit with JGZ a new record is created for the child
# 1) there are variables that (mostly) do not change over time; parent info, birth related records and other demographic factors
# 2) there are variables that change at each visit with JGZ, those that monitor the growth and development of the child, e.g. length and weight

# Here we look at 1) the (mostly) non-changing information (see 03_jgz_preprocess_filter for part 2)
jgz_data_demo <- jgz_data %>% select(c(RINPERSOONS, RINPERSOON, woonplaats, gem, gebl1, gebl2, org, postcode, 
                                       geslacht, rins_fa, rin_fa, rins_mo, rin_mo, rins_mo_pr, rin_mo_pr,
                                       sex_at_birth, starts_with("PRNL_"), year, birthdate, birthday_mo, birthday_fa,
                                       birthday_kid, starts_with("GBA_"), age_at_birth_mo, age_at_birth_fa, premature_birth, 
                                       gebl1c, gebl2c, starts_with("LAND_"), STED, gemc, COROP2020, opl1, opl2, 
                                       doe_moment_4m, doe_moment_2y, zwduur_clean, conception, doe_moment_16w,
                                       Rinpersoons_Moeder, Rinpersoon_Moeder, jaar, pc4, pc6, lft_concep, par_cat, etnic,
                                       sectio_ia, amww, amddd1ond_cat, geboortegew, gesl, grav_cat, overdracht, pediater, 
                                       verantw_bb, verantw_eb, verantw_zw, partus, omv_cat, episiotomie, gebplaats,
                                       ligging, nicuopname, pijnbestrijding2, robson, ruptuur, congenafw_ja, fluxus, 
                                       vroeg_geb_24_37, vroeg_geb_24_28, vroeg_geb_28_34, vroeg_geb_34_37,
                                       vooraf_zw_vroeg_24_37, vooraf_zw_vroeg_24_28, vooraf_zw_vroeg_28_34, vooraf_zw_vroeg_34_37,
                                       N_vroeg_24_37, N_vroeg_24_28, N_vroeg_28_34, N_vroeg_34_37, 
                                       laag_geb_gewicht, vooraf_sga, N_vooraf_sga, interpreg, 
                                       rins_mo_pr_clean, rin_mo_pr_clean, interpreg_cat, amww_f, meerling, lft_concept_fa, lft_concept_mo)) %>% 
  unique()

# notice that in the data, for the same child, a record at a certain age contains info that is not present in the other records
# we fill the data set using the available information
jgz_data_demo <- jgz_data_demo %>% 
  group_by(RINPERSOONS, RINPERSOON) %>% 
  fill(names(jgz_data_demo), .direction = "updown") %>% 
  distinct() %>% 
  ungroup()

# if a child has a different woonplaats or postcode over time or if the child is registered at more than one organisation
# there are now multiple records in the data set for that child
# look at all the duplicate records
duplicates_kids <- jgz_data_demo$RINPERSOON[duplicated(jgz_data_demo$RINPERSOON)]
duplicates_kids_records <- jgz_data_demo %>% filter(RINPERSOON %in% duplicates_kids)

# save the created data set
saveRDS(jgz_data_demo, "case_jgz/jgz_data_demo_p.rds")


# now that we have transformed the variables in the JGZ data
# we can move to the next step: 03_jgz_preprocess_filter
