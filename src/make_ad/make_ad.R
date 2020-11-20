library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)

source("src/external/functions.R")

#######################
# Set this parameter to false in the Makefile to run the true allocation results.
#######################

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  pseudorand <- TRUE #default to pseudorandom treatment
  set.seed = 42
} else if (length(args) != 0) {
  pseudorand <- args[1]
}

################
# Make adsl
################

raw <- read_rds("data/raw/raw.rds")

tddm <- read_rds("data/td/tddm.rds")
tdran <- read_rds("data/td/tdran.rds")
items <- raw %>% pick("items")
tdsq <- read_rds("data/td/tdsq.rds")
tdds <- read_rds("data/td/tdds.rds")


tmp <- tdsq %>% 
  filter(!(eventid %in% c("V00", "READM"))) %>% 
  group_by(subjectid) %>% 
  summarise(tmp2 = min(sq_mort), .groups = "drop_last") %>% 
  ungroup

adsl <- tdds %>% 
  left_join(tddm %>% select(subjectid, age_calc, sex, dmini), by = "subjectid") %>% 
  left_join(tmp, by = "subjectid") %>%
  left_join(tdran %>% select(-randt), by= "subjectid") %>% 
  mutate(enrolled= if_else(!is.na(dmicdat), "Yes", "No")) %>% 
  mutate(randomised = if_else(!is.na(randt), "Yes", "No")) %>% 
  mutate(fasex1 = if_else(is.na(tmp2), "Yes", "No"),
         fasex2 = if_else(eosreascd == 3, "Yes", "No", missing = "No"),
         fas = if_else(fasex1 == "No" & fasex2 == "No", "Yes", "No"),
         fas_rem = if_else(fas == "Yes" & 
                             ranavail_rem == "Yes" & 
                             (rantrtcd %in% c(1,3) ), "Yes", "No"),
         fas_hcq = if_else(fas == "Yes" & 
                             ranavail_hcq == "Yes" &
                             rantrtcd %in% c(1,2), "Yes", "No")) %>% 
  mutate(across(c(starts_with("fas"), enrolled, randomised), ~ factor(.x, levels = c("No", "Yes"), ordered = TRUE))) %>% 
  labelled::set_variable_labels(fasex1 = "Excluded from FAS, no post-randomisation evaluations?", 
                                fasex2 = "Excluded from FAS, incorrect inclusion?",
                                fas = "Included in FAS?", 
                                randomised = "Randomised?", 
                                enrolled = "All patients with informed consent",
                                fas_rem = "Included in FAS with remdesivir available?",
                                fas_hcq = "Included in FAS with HCQ available?") %>% 
  select(-tmp2, -(eosyn:eosdtdat)) %>% 
  arrange(subjectid)

###############
# Set up the pseudorandomisation list. 
##############

print(paste0("Pseudorandomisation is ", pseudorand))
if (pseudorand) {
  
  varlabels <- labelled::var_label(adsl, unlist = TRUE)
  
  adsl2 <- adsl %>%
    select(subjectid, rantrt:fas_hcq ) %>% 
    group_by(subjectid) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(subjectid = sample(subjectid,n(), replace = FALSE)) %>% 
    unnest(data) 
  
  adsl <- adsl %>% 
    select(-(rantrt:fas_hcq)) %>% 
    left_join(adsl2, by = "subjectid")
  
  labelled::var_label(adsl) <- varlabels
  
}

write_rds(adsl, "data/ad/adsl.rds")

################
# Make the adex dataset for exposure to study treatment
###############

tdex <- read_rds("data/td/tdex.rds")

adex <- tdex %>%  
  mutate(
    extrt = case_when(
      !is.na(dahcdt) ~ "HCQ",
      !is.na(drremdt) ~ "Remdesivir",
      TRUE ~ ""),
    exstdt =  case_when(
      extrt == "HCQ" ~ dahcdt, 
      extrt == "Remdesivir" ~ drremdt
    ),
    exendt = exstdt,
    exdose = case_when(
      extrt == "HCQ" ~ dabcno, 
      extrt == "Remdesivir" ~ drremds,
      TRUE ~ NA_real_
    ),
    exdisc = case_when(
      extrt == "HCQ" ~ dadiyn_c, 
      extrt == "Remdesivir" ~ dardisc
    )
  ) %>% 
  select(subjectid, extrt, exstdt, exendt, exdose, exdisc ) %>% 
  group_by(subjectid, extrt) %>% 
  summarise(
    exstdt = min(if_else(exdose>0, exstdt, Inf)), 
    exendt = max(if_else(exdose > 0, exendt, -Inf)),
    extrtdur = exendt - exstdt,
    exndose = sum(exdose > 0, na.rm = TRUE),
    exmindose = min(exdose, na.rm = TRUE),
    exmaxdose = max(exdose, na.rm = TRUE),
    extotdose = sum(exdose, na.rm = TRUE),
    exdisc = max(exdisc, na.rm = TRUE), .groups = "drop_last"
  ) %>% 
  labelled::set_variable_labels(
    extrt = "Treatment administered",
    exstdt = "Treatment start date",
    exendt = "Treatment end date",
    extrtdur = "Treatment duration (days)",
    exmindose = "Minimum dose registered", 
    exmaxdose = "Maximum dose registered",
    extotdose = "Total dose", 
    exdisc = "Any discrepances registered?"
  ) 


adex <- adsl %>%
  left_join(adex, by =  "subjectid")

write_rds(adex, "data/ad/adex.rds")



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
