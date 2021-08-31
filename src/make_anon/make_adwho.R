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

whoid <- raw %>% 
  pick("dm") %>% 
  select(subjectid, dmwhoid)

anondm <- addm %>% 
  select(subjectid, rantrt:fas_hcq, dmage, dmicdat.x, bldt, randt, sex, sympdur, starts_with("me"), sq_admis, rcwhocps, 
         lbcrpres, starts_with("cc")) %>% 
  mutate(randyear = lubridate::year(randt),
         randweek = lubridate::isoweek(randt),
         country = "Norway", 
         ethnicity = "Not known") %>%
  select(-dmicdat.x, -bldt, -randt, -rantrtcd) %>% 
  labelled::set_variable_labels(sympdur = "Symptom duration",
                                rcwhocps = "WHO clinical progression scale",
                                randyear = "Year of randomisation",
                                randweek = "Randomisation Week", 
                                country = "Country",
                                ethnicity = "Ethnicity")
  



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



adev <- read_rds("data/ad/adev.rds")

whoeff <- adev 

whodm <- anondm %>% 
  left_join(whoid, by = "subjectid") %>% 
  left_join(addm %>% select(subjectid, randt, sitename), by = "subjectid") %>% 
  select(subjectid, dmwhoid, sitename, randt, everything())

whoex <- read_rds("data/ad/adex.rds")

haven::write_dta(whoeff, "data/work/who/whoeff.dta")
haven::write_dta(whodm, "data/work/who/whodm.dta")
haven::write_dta(whoex, "data/work/who/whoex.dta")
write_rds(whoeff, "data/work/who/whoeff.rds")
write_rds(whodm, "data/work/who/whodm.rds")
write_rds(whoex, "data/work/who/whoex.rds")




  

# adsl <- read_rds("data/ad/adsl.rds")


