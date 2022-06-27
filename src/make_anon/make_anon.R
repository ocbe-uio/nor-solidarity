# The following Individual patient data from the “The (Norwegian) NOR Solidarity Multicenter Trial on the Efficacy of Different Anti-viral Drugs in SARS-CoV-2 Infected Patients”. The Data will be extracted such that the transferred Data are regarded anonymous. See full protocol for more information.
# Baseline data:
#   􏰀 Age
# 􏰀 Sex
# 􏰀 Ethnicity
# 􏰀 Country
# 􏰀 Date of enrolment
# 􏰀 days with symptoms prior to randomization
# 􏰀 ICU care at randomization
# 􏰀 severity of COVID-19 (ordinal scale: without need for oxygen
#                          therapy, need for low-flow oxygen therapy, need for high-flow oxygen therapy/non-invasive ventilation, 
#                          mechanical ventilation/ECMO)
# 􏰀 coexisting conditions
# 􏰀 C-reactive protein values
# 􏰀 concomitant therapies during study



# Data to assess the following outcomes:
#   􏰀 Mortality at 28 days
# 􏰀 Mortality at 60 days
# 
# 􏰀 Need for new mechanical ventilation or death within 28 days
# 􏰀 Number of mechanical ventilator-free days in the first 28 days
# 􏰀 Disease/clinical status at day 14 and day 28 (ordinal scale*)
# 􏰀 Days until cessation of oxygen therapy in those with
# oxygenation at baseline up to day 28
# 􏰀 Days until discharge/reaching discharge criteria up to day 28
# (defined as reaching stage 1 of ordinal scale*)
# 􏰀 Quality of life at day 28 (in addition, we will check for longer term quality of life data 
#                                 and pool data if comparable time points
#                               are available across included trials).
# 􏰀 Viral clearance (proportion of patients with absence of virus
#                     replication in PCR) at day 5, day 10, and day 15
# 􏰀 Total adverse events / serious adverse events by day 28 (using comparable entities across trials; e.g. 
#                                                              thromboembolic events; analyze number of patients – 
#                                                              also document number of events; in addition, we will 
#                                                              check for longer term advers events / serious adverse 
#                                                              events data and pool data if comparable time points
#                                                             are available across included trials)
# 


library(readr)
library(tidyverse)
library(labelled)

#Baseline information


#􏰀 Age
# 􏰀 Sex
# 􏰀 Ethnicity
# 􏰀 Country
# 􏰀 Date of enrolment
# 􏰀 days with symptoms prior to randomization
# 􏰀 ICU care at randomization
# 􏰀 severity of COVID-19 (ordinal scale: without need for oxygen
#                          therapy, need for low-flow oxygen therapy, 
#                           need for high-flow oxygen therapy/non-invasive 
#                           ventilation, mechanical ventilation/ECMO)
# 􏰀 coexisting conditions
# 􏰀 C-reactive protein values
# 􏰀 concomitant therapies during study

source("src/external/functions.R")

raw <- readr::read_rds("data/raw/raw.rds")
addm <- read_rds("data/ad/addm.rds")

anonid <- addm %>% 
  select(subjectid) %>% 
  mutate(anonid = as.vector(random::randomSequence(min = 1, max = 181, col = 1))) %>% 
  labelled::set_variable_labels(anonid = "Anonymised patient identifier")
  


anondm <- addm %>% 
  select(subjectid, rantrt:fas_hcq, dmage, dmicdat, bldt, randt, sex, sympdur, starts_with("me"), sq_admis, rcwhocps, 
         lbcrpres, starts_with("cc"), starts_with("ab")) %>% 
  mutate(randyear = lubridate::year(randt),
         randweek = lubridate::isoweek(randt),
         country = "Norway", 
         ethnicity = "Not known") %>%
  select(-dmicdat, -bldt, -randt, -rantrtcd) %>% 
  labelled::set_variable_labels(sympdur = "Symptom duration",
                                rcwhocps = "WHO clinical progression scale",
                                randyear = "Year of randomisation",
                                randweek = "Randomisation Week", 
                                country = "Country",
                                ethnicity = "Ethnicity")  
tdab <- read_rds("data/td/tdab.rds")
tdab_bl <- tdab %>% 
  filter(studyday == 1) %>% 
  mutate(abseroc = if_else(abrbd < 5, "RBD < 5", "RBD ≥ 5"),
         abseroc = factor(abseroc),
         abcapsidd = if_else(abcapsid < 10, "Capsid < 10", "Capsid ≥ 10"),
         abcapsidd = factor(abcapsidd)) %>% 
  select(subjectid, abrbd, abcapsid, abace2rbd, absumctr) %>% 
  set_variable_labels(absumctr = "Sum Controls",
                      abrbd = "Receptor-binding Domain",
                      abace2rbd = "Angiotensin converting enzyme 2 (ACE2)",
                      abcapsid = "Nucleocapsid (mammalian expr)")
anondm <- anondm %>% 
  left_join(tdab_bl, by = "subjectid") %>% 
  relocate(abrbd:absumctr, .after = cc_eversmoker)


# Efficacy
adev <- read_rds("data/ad/adev.rds")

# Data to assess the following outcomes:
#   􏰀 Mortality at 28 days
# 􏰀 Mortality at 60 days
# 􏰀 Days until discharge/reaching discharge criteria up to day 28
# (defined as reaching stage 1 of ordinal scale*)



mort <- adev %>% 
  select(subjectid, survtime_60:survcens_28, dischargeday_28, dischargecens_28)

# 




newmv28 <- adev %>%
  left_join(anondm %>% select(subjectid, rcwhocps_bl = rcwhocps), by = "subjectid") %>% 
  mutate(newmv28 = case_when(
    rcwhocps_bl %in% c(7, 8, 9) ~ "NA",
    mv28 == "Yes" ~ "Yes",
    survcens_28 == "No" ~ "Yes", 
    TRUE ~ "No"
    )) %>% 
  mutate(newmv28 = factor(newmv28, levels = c("No", "Yes", "NA"))) %>% 
  select(subjectid, newmv28) %>% 
  labelled::set_variable_labels(newmv28 = "Need for new mechanical ventilation or death within 28 days")
  

# 􏰀 Number of mechanical ventilator-free days in the first 28 days
mvfreedays28 <- read_rds("data/td/tdrc.rds") %>%
  group_by(subjectid) %>%
  arrange(subjectid, eventdate) %>%
  mutate(studyday = eventdate - eventdate[eventid == "V00"],
         rcwhocps = if_else(is.na(rcwhocps), 0, rcwhocps)) %>% 
  filter(studyday <= 28) %>% 
  mutate(mvfreedays28 = 28 - sum(rcwhocps >= 7)) %>%
  filter(eventid == "V00") %>% 
  select(subjectid, mvfreedays28) %>% 
  mutate(mvfreedays28 = max(0, mvfreedays28)) %>% 
  labelled::set_variable_labels(mvfreedays28 = "Number of mechanical ventilator-free days in the first 28 days")



# 􏰀 Disease/clinical status at day 14 and day 28 (ordinal scale*)

clinstat <- read_rds("data/td/tdrc.rds") %>% 
  group_by(subjectid) %>%
  arrange(subjectid, eventdate) %>%
  mutate(studyday = eventdate - eventdate[eventid == "V00"],
         rcwhocps = if_else(is.na(rcwhocps), 0, rcwhocps)) %>% 
  #ilter(studyday <= 28) %>% 
  filter(eventid != "V30") %>% 
  select(subjectid, rcwhocps, studyday, eventid) %>% 
  left_join(adev %>% select(subjectid, dischargeday_28, dischargecens_28, survtime_28, survcens_28), by = "subjectid") %>% 
  mutate(clinstat28 = case_when(
    survcens_28 == "No" & survtime_28 <= 28 ~ 10,
    dischargecens_28 == "No" ~ 3,
    survcens_28 == "Yes" & survtime_28 < 28 ~ NA_real_,
    studyday == 28 ~ rcwhocps,
    subjectid == "01-015" ~ 3,
    subjectid == "01-017" ~ 3,
    subjectid == "01-030" ~ 3,
    subjectid == "04-011" ~ 5,
    subjectid == "07-003" ~ 7,
    subjectid == "08-003" ~ 4,
    subjectid == "10-004" ~ 3,
    subjectid == "13-002" ~ 4,
    subjectid == "27-002" ~ 8,
    TRUE ~ 0
  )) %>% 
  mutate(clinstat28 = max(clinstat28)) %>% 
  mutate(clinstat14 = case_when(
    survcens_28 == "No" & survtime_28 <= 14 ~ 10,
    dischargecens_28 == "No" & dischargeday_28 <=14 ~ 3,
    survcens_28 == "Yes" & survtime_28 <= 14 ~ NA_real_,
    studyday == 14 ~ rcwhocps,
    studyday == 13 ~ rcwhocps,
    studyday == 15 ~ rcwhocps,
    # subjectid == "01-015" ~ 3,
    subjectid == "14-002" ~ 3,
    TRUE ~ 0
  )) %>% 
  mutate(clinstat14 = max(clinstat14)) %>% 
  labelled::set_variable_labels(clinstat14 = "WHO Clinical performace status (1-10) at day 14",
                                clinstat28 = "WHO Clinical performace status (1-10) at day 28") 
  

# 􏰀 Days until cessation of oxygen therapy in those with
# oxygenation at baseline up to day 28

oxdays <- clinstat %>% 
  select(-starts_with("clinstat")) %>% 
  mutate(oxygen = rcwhocps > 4 | rcwhocps == 0,
         change = cumsum(cumsum(!oxygen)),
         oxdays = ifelse(change == 1, studyday, -1),
         oxdays = max(oxdays)) %>% 
  mutate(oxdays = case_when(
    oxdays >= 0 ~ oxdays,
    survcens_28 == "No" ~ as.numeric(survtime_28),
    survcens_28 == "Yes" & dischargecens_28 == "No" ~ as.numeric(dischargeday_28),
    survcens_28 =="Yes" & survtime_28 < 28 ~ as.numeric(survtime_28),
    dischargeday_28 == 28 & dischargecens_28 == "Yes"~ 28,
    TRUE ~ NA_real_
  )) %>% 
  mutate(oxdayscens = case_when(
    max(change) > 0 & oxdays > 28 ~ "Censored on oxygen at day 28",
    max(change) > 0 & oxdays <= 28 ~ "Stoppet oxygen therapy before day 28",
    survcens_28 == "No" ~ "Died before day 28",
    survcens_28 =="Yes" & survtime_28 < 28 ~ "Withdrawn before day 28",
    survcens_28 == "Yes" & dischargecens_28 == "No" ~ "Stoppet oxygen therapy before day 28",
    max(change) == 0 & oxdays == 28 ~ "Censored on oxygen at day 28", 
    TRUE ~ NA_character_
  )) %>% 
  mutate(maxox = max(change)) %>% 
  filter(eventid == "V00") %>% 
  labelled::set_variable_labels(oxdays = "Days until cessation of oxygen therapy",
                                oxdayscens = "Censoring status on cessation of oxygen therapy") %>% 
  select(subjectid, starts_with("oxday"))
  
clinstat <- clinstat %>% 
  filter(eventid == "V00") %>% 
  select(subjectid, clinstat14, clinstat28)


  
  


# 􏰀 Quality of life at day 28 (in addition, we will check for longer term quality of life data 
#                                 and pool data if comparable time points
#                               are available across included trials).

items <- raw %>% pick("items")

copd <- readr::read_rds("data/raw/raw.rds") %>% 
  pick("cat") %>% 
  filter(eventid == "V28PROM") %>% 
  select(subjectid, ends_with("cd")) %>% 
  labeliser(codelist = items)


anondm <- anonid %>% 
  left_join(anondm, by = "subjectid") %>% 
  select(-subjectid) %>% 
  arrange(anonid)


anoneff <- anonid %>% 
  left_join(mort, by = "subjectid") %>% 
  left_join(newmv28, by = "subjectid") %>% 
  left_join(mvfreedays28, by = "subjectid") %>% 
  left_join(clinstat, by = "subjectid") %>% 
  left_join(oxdays, by = "subjectid") %>% 
  left_join(copd, by = "subjectid") %>% 
  select(-subjectid) %>% 
  arrange(anonid) %>% 
  labelled::set_variable_labels(survtime_60 = "Survival time, censored at day 60",
                                survcens_60 = "Censored? (No indicates death within 60 days)",
                                survtime_28 = "Survival time, censored at day 28",
                                survcens_28 = "Censored? (No indicates death within 28 days)", 
                                dischargeday_28 = "Days until discharge up to day 28",
                                dischargecens_28 = "Censored? (No indicates discharged within 28 days)") 





# 􏰀 Viral clearance (proportion of patients with absence of virus
#                     replication in PCR) at day 5, day 10, and day 15  

 

anonvl <- anonid %>% 
  left_join(readr::read_rds("data/ad/advl.rds"), by = "subjectid") %>% 
  select(anonid, studyday, vllog10cpkc, vldetect) %>% 
  filter(!is.na(studyday)) %>% 
  arrange(anonid)


# 􏰀 Total adverse events / serious adverse events by day 28 (using comparable entities across trials; e.g. 
#                                                              thromboembolic events; analyze number of patients – 
#                                                              also document number of events; in addition, we will 
#                                                              check for longer term advers events / serious adverse 
#                                                              events data and pool data if comparable time points
#                                                             are available across included trials)
# 




anonae <-  anonid %>% 
  left_join(readr::read_rds("data/ad/adae.rds"), by = "subjectid") %>% 
  filter(!is.na(aespid)) %>% 
  mutate(studyday_aest = aestdat - randt) %>% 
  filter(studyday_aest <= 28) %>% 
  select(anonid, rantrt:fas_hcq, aesevin, aesevext, aeser,aerel, aedis, soc_name, hlgt_name, hlt_name, pt_name) %>% 
  arrange(anonid)




haven::write_dta(anondm, "data/work/ipdma/anondm.dta")
haven::write_dta(anoneff, "data/work/ipdma/anoneff.dta")
haven::write_dta(anonvl, "data/work/ipdma/anonvl.dta")
haven::write_dta(anonae, "data/work/ipdma/anonae.dta")
write_rds(anondm, "data/work/ipdma/anondm.rds")
write_rds(anoneff, "data/work/ipdma/anoneff.rds")
write_rds(anonvl, "data/work/ipdma/anonvl.rds")
write_rds(anonae, "data/work/ipdma/anonae.rds")



  






