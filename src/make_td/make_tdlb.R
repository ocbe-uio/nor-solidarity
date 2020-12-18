
#################################
# Make tdlb for the laboratory values
#################################

library(tidyverse)
library(lubridate)
library(glue)
library(transplantr)

source("src/external/functions.R")


raw <- readr::read_rds("data/raw/raw.rds")
tddm <- readr::read_rds("data/td/tddm.rds")
items <- raw %>% pick("items")


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
  full_join(tdlbh, by = c("subjectid", "eventid", "eventdate")) %>% 
  left_join(tddm %>% select(sex, age_calc, subjectid), by = "subjectid") %>% 
  mutate(sex_tmp = recode_factor(sex, "Male" = "M", "Female" = "F"),
         ethnicity = "non-black") %>% 
  mutate(lbegfrc = ckd_epi(creat = lbcreres, age = age_calc, sex = sex_tmp, eth = ethnicity),
         lbegfrm = mdrd(creat = lbcreres, age = age_calc, sex = sex_tmp, eth = ethnicity)) %>% 
  mutate(across(starts_with("lbegfr"), ~round(.x, digits = 1))) %>% 
  labelled::set_variable_labels(lbegfrc = "eGFR values by the CKD-EPI equation", 
                      lbegfrm =  "eGFR values by the MDRD equation") %>% 
  arrange(subjectid, eventdate) %>%
  relocate(starts_with("lbegfr"), .after = lbcreu) %>% 
  select(-sex, -age_calc, -sex_tmp, -ethnicity)


write_rds(tdlb, "data/td/tdlb.rds")

