##############################
# Make the demographics and baseline characteristics tabulation dataset
# Input from raw: cc, ef, ao, me, dm
# Input from other: tdds
# Output: tddm
###############################


library(tidyverse)
library(lubridate)

source("src/external/functions.R")

raw <- readr::read_rds("data/raw/raw.rds")
tdds <- readr::read_rds("data/td/tdds.rds")
items <- raw %>% pick("items")



tdcc <- raw %>% 
  pick("cc") %>% 
  select(subjectid,  starts_with("cc")) %>% 
  select(-ends_with("cd")) %>% 
  mutate(across(cc_card:cc_other, ~if_else(cc_known == "Yes", .x, factor("No", order = TRUE)))) %>%       
  labeliser(codelist = items) %>% 
  mutate(cc_eversmoker = case_when(
    cc_tobac == "Yes" ~ "Yes", 
    cc_tobac == "Former smoker" ~ "Yes",
    cc_tobac == "Never smoked" ~ "No",
    cc_tobac == "Unknown" ~ "Unknown", 
    is.na(cc_tobac) ~ NA_character_,
    TRUE ~ NA_character_),
    cc_eversmoker = factor(cc_eversmoker, levels = c("No", "Yes", "Unknown"))
  ) %>% 
  labelled::set_variable_labels(cc_eversmoker = "Ever smoker?") 


tdef <- raw %>% 
  pick("ef") %>% 
  labeliser(codelist = items) %>% 
  select( subjectid,  starts_with("ef")) %>% 
  select(-ends_with("cd"))



tdoa <- raw %>% 
  pick("oa") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid,  starts_with("oa")) %>% 
  select(-ends_with("cd"))



tdme <- raw %>% 
  pick("me") %>% 
  labeliser(codelist = items) %>% 
  select( subjectid,  starts_with("me")) %>% 
  select(-ends_with("cd"))

# Select out these from the CM dataset
# C03 - diuretika, 
# C10 - statiner, 
# C07 - betablokkere, 
# C09 - ACE-hemmere/ arb, 
# C01 - hjerteterapi: glykosider, kardilaterende, antiarytmika, 
# C08 - kalsiumantagonister, 
# A10 - antidiabetika

tdcm_bl <- tdcm %>% 
  filter(cmbl == "Yes" & l2code %in% c("C03", "C10", "C07", "C09",
                                      "C01", "C08", "A10")) %>% 
  select(subjectid, l2code) %>% 
  group_by(subjectid, l2code) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  pivot_wider(id_cols = subjectid, names_from = l2code, values_from = n) %>%
  arrange(subjectid)


tddm <- raw %>% 
  pick("dm") %>% 
  select( subjectid, eventdate, starts_with("dm"), sex, brthdat) %>% 
  select(-ends_with("cd"), -dmwhoyn,-dmsite, -dmransys, -dmwhoid, -dmcbp, -dm1) %>% 
  labeliser(codelist = items) %>% 
  mutate(age_calc = interval(brthdat, dmicdat) %/% months(1), 
         age_calc = age_calc / 12,
         age_calc = round(age_calc, 1)) %>% 
  labelled::set_variable_labels(age_calc = "Age (years)") %>% 
  # Add co-morbidity and smoking
  left_join(tdcc , by = "subjectid") %>% 
  # Add epidemiological factors
  left_join(tdef , by = "subjectid")  %>% 
  # Add duration of disease 
  left_join(tdds %>% select(subjectid, oaadm_h, oasympdt), by = "subjectid") %>% 
  mutate(sympdur = oaadm_h - oasympdt) %>% 
  labelled::set_variable_labels(sympdur = "Symptom duration at admission (days") %>% 
  select(-oaadm_h, -oasympdt) %>% 
  # Add baseline medications
  left_join(tdme, by = "subjectid") %>% 
  # Add baseline medication from the CM dataset
  left_join(tdcm_bl, by = "subjectid") %>% 
  mutate(across(C10:C01, ~ if_else(!is.na(.x), "Yes", "No"))) 
  


write_rds(tddm, "data/td/tddm.rds")
