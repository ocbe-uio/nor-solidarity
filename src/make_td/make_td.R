library(tidyverse)
library(lubridate)
library(glue)

source("src/external/functions.R")


raw <- readr::read_rds("data/raw/raw.rds")
items <- raw %>% pick("items")


tdsq <- raw %>% 
  pick("sq") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sq")) %>% 
  select(-ends_with("cd"), -(sqegfr:sqaeyes1))  
write_rds(tdsq, "data/td/tdsq.rds")

tdsc <- raw %>% 
  pick("sc") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sc")) %>% 
  select(-ends_with("cd"))
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



tdlbb <- raw %>% 
  pick("lbb") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventid, eventdate, starts_with("lb")) %>% 
  select(-ends_with("cd"))

tdlbh <- raw %>% 
  pick("lbh") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventid, eventdate, starts_with("lb")) %>% 
  select(-ends_with("cd"))

tdlb <-tdlbb %>% 
  full_join(tdlbh, by = c("subjectid", "eventid", "eventdate"))

write_rds(tdlb, "data/td/tdlb.rds")


#########################################
# Make tdex for study treatment exposure
########################################


tdex <- raw %>% pick("da") %>% 
  full_join(pick(raw,"dr")) %>%   
  select(subjectid, starts_with("event"), starts_with("da"), starts_with("dr")) %>% 
  select(-ends_with("cd")) %>% 
  labeliser(codelist = items) 

write_rds(tdex, "data/td/tdex.rds")


tdcm <- raw %>% pick("cm") %>% 
  left_join(pick(raw,"atc_without_ddd")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser() 





tdds <- raw %>% 
  pick("eos") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("eos")) %>% 
  select( -(eosaechk:eosaeyes1cd), -eosyncd)


tdds <- tddm %>% 
  select(subjectid, dmicdat) %>% 
  left_join(tdoa, by = "subjectid") %>% 
  left_join(select(tdran, subjectid, randt), by="subjectid") %>% 
  left_join(tdds, by = "subjectid") %>% 
  left_join(tddph, by = "subjectid")

write_rds(tdds, "data/td/tdds.rds")

###################################################
# Make tdae
###################################################


tdae <- raw %>% pick("ae") %>% 
  left_join(pick(raw,"meddra")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser()

write_rds(tdae, "data/td/tdae.rds")


