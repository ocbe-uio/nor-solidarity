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
adsl <- readr::read_rds("data/ad/adsl.rds")
tdvs <- readr::read_rds("data/td/tdvs.rds")
tdrc <- readr::read_rds("data/td/tdrc.rds")
tdvl <- readr::read_rds("data/td/tdvl.rds")
tdab <- readr::read_rds("data/td/tdab.rds")

tdvl_bl <- tdvl %>% 
  left_join(adsl %>% select(subjectid, bldt), by = "subjectid") %>% 
  mutate(studyday = vlsampledt - bldt) %>% 
  filter(studyday %in% c(-3:0) & 
           vlsource %in% c("Labfile only", "Both")) %>% 
  mutate(vllog10cpkc_imp = if_else(vldetect == "Detected", vllog10cpkc, 0)) %>% 
  group_by(subjectid) %>% 
  summarise(vllog10cpkc = mean(vllog10cpkc_imp, rm.na = TRUE), .groups = "drop_last") %>% 
  labelled::set_variable_labels(vllog10cpkc = "Log10 copies/1000 cells")

tdab_bl <- tdab %>% 
  filter(studyday == 1) %>% 
  mutate(abseroc = if_else(abrbd < 5, "RBD < 5", "RBD ≥ 5"),
         abseroc = factor(abseroc)) %>% 
  select(subjectid, abseroc) %>% 
  set_variable_labels(abseroc = "Seroconverted (RBD ≥ 5)")


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
              select(subjectid, rcoxyter, rcratio, rcwhocps, rcwhostate),
            by = "subjectid") %>%  
  mutate(rcratio40 = if_else(rcratio < 40, "Yes", "No")) %>% 
  labelled::set_variable_labels(rcratio40 = "Baseline PF-ratio < 40kPa?") %>% 
  # Add baseline SOFA score
  left_join(tdsc %>% filter(eventid == "V00") %>% select(subjectid, scsumsc), by = "subjectid") %>% 
  labelled::set_variable_labels(scsumsc = "Baseline SOFA score") %>% 
  # Add lab-data
  left_join(tdlb %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, lbferres, lbdimres1, lbastres, lbaltres, lbldres, lbcrpres, lbprores,
                     lbhbres, lbpcres, lbneures, lblymres, lbwbcres, lbtrores, lbtrotyp, lbbnpres, 
                     lbcreres, lbcrpres, lbegfrc, lbegfrm), 
            by = "subjectid") %>% 
  left_join(tdvl_bl, by = "subjectid") %>% 
  left_join(tdab_bl, by = "subjectid")



write_rds(addm, "data/ad/addm.rds")




