################################
# Make demographics and baseline characteristics analysis dataset
# Input: adsl, tddm, tdsq
###############################



library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)

source("src/external/functions.R")

tdsq <- readr::read_rds("data/td/tdsq.rds")
tdsc <- readr::read_rds("data/td/tdsc.rds")
tdlb <- readr::read_rds("data/td/tdlb.rds")
tddm <- readr::read_rds("data/td/tddm.rds")
tdcm <- readr::read_rds("data/td/tdcm.rds")


addm <- adsl %>% select(-age_calc,  -sex) %>% 
  left_join(tddm, by ="subjectid") %>% filter(fas == "Yes") %>% 
  # Add admittance at baseline
  left_join(tdsq %>% filter(eventid == "V00") %>% select(subjectid, sq_admis), by = "subjectid") %>% 
  labelled::set_variable_labels(sq_admis = "Admitted to (baseline)") %>%
  left_join(tdvs %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, vsweight, vsheight, vsbmi, vsobese, 
                     vssys, vsdia, vsmap, vstemp), 
            by = "subjectid") %>% 
  left_join(tdrc %>%
              filter(eventid == "V00") %>%
              select(subjectid, rcoxyter, rcratio),
            by = "subjectid") %>%  
  # Add baseline SOFA score
  left_join(tdsc %>% filter(eventid == "V00") %>% select(subjectid, scsumsc), by = "subjectid") %>% 
  labelled::set_variable_labels(scsumsc = "Baseline SOFA score") %>% 
  # Add lab-data
  left_join(tdlb %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, lbferres, lbdimres1, lbastres, lbaltres, lbldres, lbcrpres, lbprores,
                     lbhbres, lbpcres, lbneures, lblymres, lbwbcres), 
            by = "subjectid")  
