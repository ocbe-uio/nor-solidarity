###########################
# Make the exposure to treatment dataset
# Input: tdex adsl
# Output: adex
############################

library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)


################
# Make the adex dataset for exposure to study treatment
###############

tdex <- read_rds("data/td/tdex.rds")
adsl <- read_rds("data/ad/adsl.rds")

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

