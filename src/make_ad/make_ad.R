library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)

source("src/external/functions.R")

##################
#Make adae
#################

tdae <- read_rds("data/td/tdae.rds")

adae <- adsl %>% 
#  select(sitename, subjectid, dmicdat, age_calc, randt, rantrt) %>%
  left_join(tdae, by =  c("subjectid")) %>% 
  labeliser() %>% 
  mutate(anyae = if_else(is.na(aespid), 0, 1),
         sae = if_else(is.na(aespid), 0, aesercd)
  ) %>%  
  group_by(subjectid) %>% 
  mutate(n_ae = sum(anyae),
         one_ae = n_ae == 1,
         two_ae = n_ae == 2,
         three_plus_ae = n_ae > 2,
         anysae = max(sae)) %>% 
  ungroup

readr::write_rds(adae, "data/ad/adae.rds")

##################
# Make adeff
##################


adeff <- tdds %>% 
  mutate(survtime = if_else(!is.na(eosdtdat), eosdtdat - randt, eosdat - randt),
         survtime = if_else(!is.na(survtime), survtime, dphendt - randt),
         survcens = if_else(is.na(eosdtdat), "Yes", "No")
         ) #%>% 
  #mutate(survtime28 = if_else(survtime <= 28, survtime, 28),
         
  
# 
# 
#   mutate(outcome = case_when(
#     !is.na(dphdisc) ~ "Alive and discharged",
#     eosreas == "Death" ~ "Dead",
#     !is.na(eosreas) ~ "Withdrawn",
#     TRUE ~ "Ongoing"
#   )) %>% 
#   mutate(outcomedat = case_when(
#     !is.na(dphdisc) ~ dphad,
#     !is.na(eosreas) ~ eosdat,
#     TRUE ~ date(NA)
#   )) %>% 
#   mutate(outcome = factor(outcome, 
#                           levels = c("Alive and discharged",  "Withdrawn", "Ongoing","Dead"), 
#                           ordered = TRUE)) %>% 
#   group_by(subjectid) %>% 
#   arrange(subjectid, outcomedat) %>% 
#   filter(row_number() == 1) %>% 
#   ungroup
# 
# 

  

write_rds(adeff, "data/ad/adeff.rds")


# 
# lungimaging <- adsl %>%
#   left_join(pick(raw, "di") %>% select(subjectid, dixraydt, diinfil), by = "subjectid") %>%
#   arrange(subjectid, dixraydt) %>%
#   filter(dixraydt <= randt) %>%
#   group_by(subjectid) %>% mutate(n = row_number()) %>%
#   filter(n == 1) %>%
#   mutate(dipatchy = "Unknown") %>%
#   select(subjectid, diinfil, dipatchy)
# 
# 



# # adresp <- adsl %>% 
# #   left_join(adeff %>% select(subjectid, outcomedat), by = "subjectid") %>% 
# #   left_join(pick(raw, "vs") %>% select(subjectid, vsair, vshigh, eventid, eventdate), by="subjectid") %>% 
# #   left_join(pick(raw, "rc") %>% select(subjectid,eventid, rcimv, rcniv, rcoxyter, rchigh, eventdate), by = c("subjectid", "eventid", "eventdate")) %>% 
# #   filter(eventid != "V00") %>% 
# #   filter(eventdate <= outcomedat & eventdate > randt) %>% 
# #   group_by(subjectid) %>% 
# #   summarise(rcimv = if_else(any(rcimv == "Yes" | rcoxyter == "Invasive mechanical Ventilation"), "Yes", "No", "No"),
# #             rcniv = if_else(any(rcniv == "Yes" | rcoxyter == "Non-Invasive mechanical Ventilation (NIV)"), "Yes", "No", "No"),
# #             rchigh = if_else(any(rchigh == "Yes" | rcoxyter == "High-flow nasal cannula" | vshigh == "Yes"), "Yes", "No", "No"), 
# #             rcotherox = if_else(any(vsair == "Oxygen therapy" & vshigh != "Yes"), "Yes", "No", "No")) %>% 
# #   mutate(rcecmo = "") 
# #   
# 
# 
# adwho <- adeff %>% 
#   arrange(dmicdat) %>% 
#   mutate(country = "Norway") %>% 
#   left_join(pick(raw, "oa") %>% select(subjectid, oaadm_h), by = "subjectid") %>% 
#   left_join(adsl %>% select(subjectid, ranavail), by = "subjectid") %>% 
#   unnest(ranavail) %>%
#   filter(ranavail != "NA") %>% 
#   mutate(ranavail = case_when(
#     ranavail == "Standard of care (SOC)" ~ "SOC", 
#     ranavail == "Hydroxychloroquine + SOC" ~ "HCQ",
#     ranavail == "Remdesivir + SOC" ~ "Remdesivir"
#   )
#          ) %>% 
#   mutate(ranavail_val = "Yes") %>% 
#   pivot_wider(names_from = ranavail, values_from = ranavail_val) %>% 
#   mutate(Remdesivir = ifelse(is.na(Remdesivir), "No", Remdesivir),
#          Lopinavir = "No",
#          Interferon = "No") %>% 
#   left_join(pick(raw,"cc") %>% select(subjectid, cc_tobac, cc_diab, cc_card, cc_liver, cc_copd, cc_hiv, cc_tb), by = "subjectid") %>% 
#   mutate(cctobac = if_else(cc_tobac == "Yes" | cc_tobac == "Unknown", as.character(cc_tobac), "No")) %>% 
#   select(-cc_tobac) %>% 
#   left_join(pick(raw, "sig") %>%  filter(eventid == "V00") %>% select(subjectid, sig_brea), by = "subjectid") %>% 
#   left_join(pick(raw, "vs") %>% filter(eventid == "V00") %>%  select(subjectid, vsair), by = "subjectid") %>% 
#   left_join(pick(raw, "rc") %>% filter(eventid == "V00") %>%  select(subjectid, rcoxyter, rcimv), by = "subjectid") %>% 
#   mutate(vsair_bl = ifelse(vsair == "Oxygen therapy", "Yes", "No"),
#          rcimv_bl = if_else(rcoxyter == "Invasive mechanical Ventilation" | rcimv == "Yes", "Yes", "No", "No")) %>% 
#   select(-rcoxyter, -vsair, -rcimv) %>% 
#   left_join(lungimaging, by = "subjectid") %>% 
#   left_join(adex, by = "subjectid") %>% 
#   mutate(cm_antiviral = "",
#          cm_interferon = "", 
#          cm_tocilizumab = "",
#          cm_conplasma = "",
#          cm_corticosteroid = "") %>% 
#   left_join(adresp , by = "subjectid") %>% 
#   mutate(death_cause = "", 
#          death_cause_other = "") %>% 
#   labeliser(codelist = items)  %>% 
#   set_variable_labels(vsair_bl = "Already on oxygen", 
#                       rcimv_bl = "Already being ventilated", 
#                       dipatchy = "Patchy shadowing?", 
#                       country = "Country", 
#                       Remdesivir = "Remdesivir available?",
#                       HCQ = "HCQ available?",
#                       Lopinavir = "Lopinavir available?",
#                       Interferon = "Interferon available?",
#                       cctobac = "Current smoker?",
#                       exrem_yn = "Remdesivir given?",
#                       exrem_stdt = "Start date Remdesivir",
#                       exrem_endt = "Stop date Remdesivir",
#                       exhcq_yn = "Hydroxychloroquine given?",
#                       exhcq_stdt = "Start date Hydroxychloroquine",
#                       exhcq_endt = "Stop date Hydroxychloroquine",
#                       exlop_yn = "Lopinavir given?",
#                       exlop_stdt = "Start date Lopinavir",
#                       exlop_endt = "Stop date Lopinavir",
#                       exinf_yn = "Interferon given?",
#                       exinf_stdt = "Start date Interferon",
#                       exinf_endt = "Stop date Interferon",
#                       cm_antiviral = "Non-study antiviral given?",
#                       cm_interferon = "Non-study interferon given?",
#                       cm_tocilizumab = "Tocilizumab or other anti-IL6 treatment given?",
#                       cm_conplasma = "Convalescent plasma given?",
#                       cm_corticosteroid = "Corticosteroid given?", 
#                       rcecmo = "ECMO?",
#                       rcotherox = "Other oxygen?"
#                       ) %>% 
#   select(country, sitename, oaadm_h, randt, rantrt, Remdesivir, HCQ, Lopinavir, Interferon, dmage, sex, 
#          subjectid, dmicdat, cctobac, cc_card:dipatchy, exrem_yn:cm_corticosteroid, rcecmo, rcimv, rcniv, rchigh, rcotherox, 
#          outcomedat, outcome, death_cause, death_cause_other) %>% 
#   filter(subjectid != "07-001") %>% 
#   filter(subjectid != "03-003")
#   
# 
# write_dta(adwho, path = "data/ad/adwho.dta", version = 14)
#      
# 
#     
# 
# 
