###########################
# Make miscelaneous tabulation datasets
# Input from raw: sq, sc, dhp, oa, lbb, lbh
# Input from td:  tdran
# Output: tdsq, tdsc, tdex, tdds, tdcm, tdae
##########################


library(tidyverse)
library(lubridate)
library(glue)

source("src/external/functions.R")


raw <- readr::read_rds("data/raw/raw.rds")
tdran <- readr::read_rds("data/td/tdran.rds")
items <- raw %>% pick("items")

tdsq <- raw %>% 
  pick("sq") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sq")) %>% 
  select(-ends_with("cd"), -(sqegfr:sqaeyes1))  %>% 
  mutate(sq_mort = if_else(!is.na(sq_mort), sq_mort, ordered("Alive"))) %>% 
  arrange(subjectid, eventdate) 
write_rds(tdsq, "data/td/tdsq.rds")

tdsc <- raw %>% 
  pick("sc") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sc")) %>% 
  select(-ends_with("cd")) %>% 
  arrange(subjectid, eventdate) 
write_rds(tdsc, "data/td/tdsc.rds")


tddph <- raw %>% 
  pick("dph") %>% 
  labeliser(codelist = items) %>% 
  group_by(subjectid) %>% 
  summarise(dphstdt = min(dphad), dphendt = max(dphad), .groups = "drop_last") %>% 
  labelled::set_variable_labels(dphstdt = "First discharge date",
                                dphendt = "Last discharge date") %>% 
  select(subjectid, dphstdt, dphendt) %>% 
  ungroup


tdoa <- raw %>% 
  pick("oa") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid,  bldt = eventdate, starts_with("oa")) %>% 
  select(-ends_with("cd")) %>% 
  labelled::set_variable_labels(bldt = "Baseline date")
  


#########################################
# Make tdex for study treatment exposure
########################################


tdex <- raw %>% pick("da") %>% 
  full_join(pick(raw,"dr")) %>%   
  select(subjectid, starts_with("event"), starts_with("da"), starts_with("dr")) %>% 
  select(-ends_with("cd")) %>% 
  labeliser(codelist = items) %>% 
  arrange(subjectid, eventdate) 

write_rds(tdex, "data/td/tdex.rds")


tdds <- raw %>% 
  pick("eos") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("eos")) %>% 
  select( -(eosaechk:eosaeyes1cd), -eosyncd)

tdds <- raw %>%
  pick("dm") %>% 
  select(subjectid, sitename, dmicdat) %>% 
  left_join(tdoa, by = "subjectid") %>% 
  left_join(select(tdran, subjectid, randt), by="subjectid") %>% 
  left_join(tdds, by = "subjectid") %>% 
  left_join(tddph, by = "subjectid") %>% 
  arrange(subjectid) %>% 
  mutate(eosreas = fct_recode(eosreas, `Voluntary discontinuation by the patient` = "Voluntary discontinuation by the patient who is at any time free to discontinue his/her participation in the study, without prejudice to further treatment"))

write_rds(tdds, "data/td/tdds.rds")

tdcm <- raw %>% pick("cm") %>% 
  left_join(pick(raw,"atc_without_ddd")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  select(-ends_with("cd")) %>% 
  labeliser() 

tdcm <- tdds %>% 
  select(subjectid, randt) %>% 
  filter(!is.na(randt)) %>% 
  right_join(tdcm, by = "subjectid") %>% 
  mutate(cmstdat = ymd(str_replace_all(cmstdat, "-NK", "-01"))) %>% 
  mutate(cmbl = if_else(cmstdat <= randt, "Yes", "No", "Yes"),
         cmprior = if_else(cmstdat < randt, "Yes", "No", "Yes")) %>%  
  labeliser() %>% 
  labelled::set_variable_labels(cmbl = "Start on or prior to baseline",
                      cmprior = "Start prior to baseline") %>% 
  arrange(subjectid, cmspid) 


write_rds(tdcm, "data/td/tdcm.rds")

###################################################
# Make tdae
###################################################


tdae <- raw %>% pick("ae") %>% 
  left_join(pick(raw,"meddra")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser() %>% 
  arrange(subjectid, aespid) 
write_rds(tdae, "data/td/tdae.rds")


