##################
# Make the events analysis data set
# Input: tdds, adsl, tdrc, tdsq
# Output: adev
##################



library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)

source("src/External/functions.R")

#######################
# Set this parameter to false in the Makefile to run the true allocation results.
#######################
# 
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args)==0) {
#   pseudorand <- TRUE #default to pseudorandom treatment
#   set.seed(42)
# } else if (length(args) != 0) {
#   pseudorand <- args[1]
#   set.seed(42)
# }



adsl <- readr::read_rds("data/ad/adsl.rds")
addm <- readr::read_rds("data/ad/addm.rds")
tdcat <-readr::read_rds("data/td/tdcat.rds")
tdqol <-readr::read_rds("data/td/tdqol.rds")
adev <- readr::read_rds("data/ad/adev.rds")

cat <- adsl %>% 
  select(subjectid, randt) %>% 
  left_join(tdcat, by = "subjectid", multiple = "all") %>% 
  rowwise() %>% 
  mutate(studyday = eventdate - randt,
         month = if_else(studyday <= 60, "m1", "m3"),
         mean = mean(c_across(copd1:copd8), na.rm = TRUE),
         nmiss = sum(is.na(across(copd1:copd8))),
         total = if_else(nmiss < 4, mean * 8, NA)) %>% 
  ungroup() %>% 
  filter(!is.na(eventname)) %>% 
  select(-randt, - eventname, -eventdate) %>% 
  select(subjectid, month, copd1:copd8, total) %>% 
  pivot_wider(id_cols = "subjectid", names_from = month, values_from = copd1:total, values_fn = mean) %>% 
  mutate(in_cat = TRUE)

qol <- tdqol %>% 
  select(subjectid, Eq5d6, starts_with("Score")) %>% 
  mutate(in_qol = TRUE)

subgroups <- addm %>% 
  select(subjectid, rcwhostate, abseroc, abcapsidd, sympdur, 
         dmage, vllog10cpkc, lbcrpres, lbferres, lblymres) %>% 
  mutate(age_cat = if_else(dmage < 60, "age < 60 years", "age ≥ 60 years"),
         age2_cat = if_else(dmage < 65, "age < 65 years", "age ≥ 65 years"),
         sympdur_cat = if_else(sympdur < 7, "Symptom duration < 7 days", "Symptom duration ≥ 7 days"),
         vl_median = median(vllog10cpkc, na.rm = TRUE),
         vl_cat = if_else(vllog10cpkc < median(vllog10cpkc, na.rm = TRUE), "Low viral load", "High viral load"),
         crp_median = median(lbcrpres, na.rm = TRUE),
         crp_cat = if_else(lbcrpres < median(lbcrpres, na.rm = TRUE),"Low CRP", "High CRP"),
         fer_median = median(lbferres, na.rm = TRUE),
         fer_cat = if_else(lbferres < median(lbferres, na.rm = TRUE), "Low Ferritin", "High Ferritin"),
         oxygen = case_match(rcwhostate, "Moderate" ~ "No or low flow ventilation", "Severe" ~ "Intensive ventilation or ECMO")) %>% 
  select(subjectid, rcwhostate, abseroc, abcapsidd, oxygen, ends_with("_cat")) %>% 
  mutate(across(ends_with("_cat"), factor)) %>% 
  mutate(oxygen = factor(oxygen)) %>% 
  set_variable_labels(oxygen = "Ventilation at baseline")


adprom <- adsl %>% 
  left_join(addm %>% select(subjectid, rcwhocps, sympdur), by = "subjectid") %>% 
  left_join(adev %>% select(subjectid, survtime, survcens, survcens_60, survcens_28), by = "subjectid") %>% 
  left_join(subgroups, by = "subjectid") %>% 
  mutate(hospdur = dphstdt - randt)


# ###############
# # Set up the pseudorandomisation list. 
# ##############
# 
# print(paste0("Pseudorandomisation is ", pseudorand))
# 
# if (pseudorand) {
#   
#   varlabels <- labelled::var_label(adprom, unlist = TRUE)
#   varorder <- names(adprom)
#    
#   print(paste0("Seed is ", set.seed))  
#   
#   set.seed
#   adprom2 <- adprom %>%
#     select(subjectid, rantrt:fas_hcq) %>% 
#     group_by(subjectid) %>% 
#     nest() %>% 
#     ungroup() %>% 
#     mutate(subjectid = sample(subjectid, n(), replace = FALSE)) %>% 
#     unnest(data) 
#   
#   adprom <- adprom %>% 
#     select(- (rantrt:fas_hcq)) %>% 
#     left_join(adprom2, by = "subjectid") %>% 
#     relocate(all_of(varorder))
#   
#   labelled::var_label(adprom) <- varlabels
#   
# }



adprom_noimp <- adprom %>% 
  left_join(cat, by = "subjectid") %>% 
  left_join(qol, by = "subjectid") %>% 
  mutate(in_cat = if_else(in_cat, TRUE, FALSE, missing = FALSE),
         in_qol = if_else(in_qol, TRUE, FALSE, missing = FALSE),
         in_prom = if_else(in_cat | in_qol, TRUE, FALSE)) %>% 
  mutate(rantrt2 = if_else(rantrtcd ==3, "Remdesivir", "SOC ± HCQ"),
         rantrt2 = factor(rantrt2, levels = c("Remdesivir", "SOC ± HCQ"), ordered = FALSE))

adprom <- adprom_noimp %>% 
  mutate(across(starts_with("copd") & ends_with("_m1"), ~replace(., is.na(.) & survcens_28 == "No", 5))) %>% #impute worst case outcome for death
  mutate(across(starts_with("copd") & ends_with("_m3"), ~replace(., is.na(.) & survcens_60 == "No", 5))) %>% #impute worst case outcome for death
  mutate(across(starts_with("total_m1"), ~replace(., is.na(.) & survcens_28 == "No", 40))) %>% #impute worst case outcome for death
  mutate(across(starts_with("total_m3"), ~replace(., is.na(.) & survcens_60 == "No", 40))) %>% #impute worst case outcome for death
  mutate(across(starts_with("Eq5d6"), ~replace(., is.na(.) & survcens_60 == "No", 0))) %>% #impute worst case outcome for death
  mutate(across(starts_with("Score_"), ~replace(., is.na(.) & survcens_60 == "No", 0))) #impute worst case outcome for death
  




readr::write_rds(adprom, "data/ad/adprom.rds")  
readr::write_rds(adprom_noimp, "data/ad/adprom_noimp.rds") 


adprom_anon <- adprom %>% 
  filter(ranavail_rem == "Yes") %>% 
  select(rantrt, rantrt2, starts_with("copd"), total_m1, total_m3, Eq5d6, starts_with("Score")) %>% 
  mutate(random = runif(n())) %>% 
  arrange(random) %>% 
  mutate(randomid = row_number()) %>%
  select(randomid, everything()) %>% 
  select(-random)


write_csv(adprom_anon, "data/ad/adprom_anon.csv", col_names = TRUE)

adprom_legend <- tribble(
  ~variable, ~label,
  "rantrt", "Treatment allocation",
  "rantrt2", "Treatment allocation SOC and HCQ combined",
  "copd1_m1", "CAT Cough at month 1",
  "copd1_m3", "Cat Cough at month 3",
  "copd2_m1", "CAT Phlegm at month 1",
  "copd2_m3", "CAT Phlegm at month 3",
  "copd3_m1", "CAT Chest tightness at month 1",
  "copd3_m3", "CAT Chest tightness at month 3",
  "copd4_m1", "CAT Breathlessness at month 1",
  "copd4_m3", "CAT Breathlessness at month 3",
  "copd5_m1", "CAT Activity at month 1",
  "copd5_m3", "CAT Activity at month 3",
  "copd6_m1", "CAT Confidence at month 1",
  "copd6_m3", "CAT Confidence at month 3",
  "copd7_m1", "CAT Sleep at month 1",
  "copd7_m3", "CAT Sleep at month 3",
  "copd8_m1", "CAT Energy at month 1",
  "copd8_m3", "CAT Energy at month 3",
  "total_m1", "Total CAT score at month 1",
  "total_m3", "Total CAT score at month 3",
  "Eq5d6", "EQ-5D Self-rated health (VAS)",
  "Score_EQ5D", "EQ-5D Index Value (UK TTO)",
  "Score_Pf", "RAND SF-36 Physical Functioning",
  "Score_Rp", "RAND SF-36 Role Physical",
  "Score_Re", "RAND SF-36 Role Emotional",
  "Score_Vt", "RAND SF-36 Vitality",
  "Score_Mh", "RAND SF-36 Mental Health",
  "Score_Sf", "RAND SF-36 Social Functioning",
  "Score_Bp", "RAND SF-36 Bodily Pain",
  "Score_Gh", "RAND SF-36 General Health",
  "Score_Ht", "RAND SF-36 Health Transition",
)

write_csv(adprom_legend, "data/ad/adprom_legend.csv", col_names = TRUE)

  

