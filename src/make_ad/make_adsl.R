##############################
# Make the subject level dataset
# Input: raw tdsq tdds tddm tdran
# Output: adsl
###############################


library(tidyverse)
library(lubridate)
library(glue)

source("src/external/functions.R")

#######################
# Set this parameter to false in the Makefile to run the true allocation results.
#######################

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  pseudorand <- FALSE #default to pseudorandom treatment
  set.seed = 42
} else if (length(args) != 0) {
  pseudorand <- args[1]
  set.seed = 42
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
  summarise(tmp2 = min(sq_mort), .groups = "drop") 

adsl_all <- tdds %>% select(-(oasympdt:oacodiag11)) %>% 
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

adsl <- adsl_all %>% 
  filter(fas == "Yes")

###############
# Set up the pseudorandomisation list. 
##############

print(paste0("Pseudorandomisation is ", pseudorand))
if (pseudorand) {
  
  varlabels <- labelled::var_label(adsl, unlist = TRUE)
  varorder <- names(adsl)
  
  adsl2 <- adsl %>%
    select(subjectid, rantrt:fas_hcq) %>% 
    group_by(subjectid) %>% 
    nest() %>% 
    ungroup() %>% 
    mutate(subjectid = sample(subjectid, n(), replace = FALSE)) %>% 
    unnest(data) 
  
  adsl <- adsl %>% 
    select(- (rantrt:fas_hcq)) %>% 
    left_join(adsl2, by = "subjectid") %>% 
    relocate(varorder)
  
  labelled::var_label(adsl) <- varlabels
  
}

readr::write_rds(adsl, "data/ad/adsl.rds")
readr::write_rds(adsl_all, "data/ad/adsl_all.rds")
