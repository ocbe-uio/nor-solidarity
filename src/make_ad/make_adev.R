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

source("src/external/functions.R")
tdds <- readr::read_rds("data/td/tdds.rds")
tdrc <- readr::read_rds("data/td/tdrc.rds")
adsl <- readr::read_rds("data/ad/adsl.rds")
tdsq <- readr::read_rds("data/td/tdsq.rds")
#######################
# Set this parameter to false in the Makefile to run the true allocation results.
#######################

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  dblockdate <- as_date("2020-11-21")
  
} else if (length(args) != 0) {
 dblockdate <- as_date(args[1])
}

last_hosp <- tdsq %>% 
  group_by(subjectid) %>% 
  filter(row_number() == n()) %>% 
  select(subjectid, last_hosp_date = eventdate, last_hosp_status = sq_mort)


survtime <- tdds %>% 
  left_join(last_hosp, by = "subjectid") %>% 
  mutate(
    # Survtime is time of death or last contract point (3 month visit). If alive at discharge, 
    # but not 3 mnths followup, survtime is time from randomisation to db export date. 
    survtime = if_else(!is.na(eosdtdat), eosdtdat - randt, eosdat - randt),
    survtime = if_else(!is.na(survtime), survtime, 
                       if_else(!is.na(dphendt), dblockdate - randt, NA_real_)),
    survtime = if_else(!is.na(survtime), survtime, last_hosp_date - randt),
    survcens = if_else(is.na(eosdtdat), "Yes", "No"), 
    survtime_60 = if_else(survtime <= 60, survtime, 60),
    survcens_60 = if_else(survtime <= 60, survcens, "Yes"),
    #survtime_60 = if_else(survcens == "Yes", survtime_60, 60), 
    survtime_28 = if_else(survtime <= 28, survtime, 28),
    survcens_28 = if_else(survtime <= 28, survcens, "Yes"),
    #survtime_28 = if_else(survcens == "Yes", survtime_28, 28) 
  ) %>% 
  select(subjectid, survtime, survcens, survtime_60, survcens_60, survtime_28, survcens_28)

icustate <- tdsq %>% 
  group_by(subjectid) %>% 
  mutate(
    studyday = eventdate - eventdate[eventid == "V00"],
    sq_admis_max = max(sq_admis[eventid != "V00"], na.rm = TRUE),
    sq_admis_max28 = max(sq_admis[eventid != "V00"& studyday <= 28], na.rm = TRUE),
    sq_admis_max60 = max(sq_admis[eventid != "V00"& studyday <= 60], na.rm = TRUE),
    icutime =  sum(sq_admis == "ICU", na.rm = TRUE)
  ) %>% 
  mutate(across(starts_with("sq_admis_"), ~if_else(!is.na(.x), .x, ordered("Ward")))) %>% 
  filter(eventid == "V00") %>% 
  select(subjectid, starts_with("sq_admis_"), icutime)

mvday <- tdrc %>%
  group_by(subjectid) %>%
  arrange(subjectid, eventdate) %>%
  mutate(
    studyday = eventdate - eventdate[eventid == "V00"],
    rcwhocps = if_else(is.na(rcwhocps), 0, rcwhocps),
    cummvtime = cumsum(rcwhocps >= 7),
    mvday = if_else(cummvtime == 1, studyday, NA_real_)
  ) %>% 
  select(subjectid, mvday) %>% 
  filter(!is.na(mvday))



         
adev <- adsl %>%
  left_join(survtime, by = "subjectid") %>%
  left_join(icustate, by = "subjectid") %>% 
  left_join(mvday, by = "subjectid") %>% 
  left_join(tdrc %>% select(subjectid, eventid, eventdate, rcwhocps, rcwhostate), by = "subjectid") %>%
  filter(fas == "Yes") %>%
  group_by(subjectid) %>%
  mutate(
    studyday = eventdate - eventdate[eventid == "V00"],
    rcwhostate_bl = rcwhostate[eventid == "V00"],
    rcwhostate_max = max(rcwhostate[eventid != "V00"], na.rm = TRUE),
    rcwhostate_max28 = max(rcwhostate[eventid != "V00" & studyday <= 28], na.rm = TRUE),
    rcwhostate_max60 = max(rcwhostate[eventid != "V00" & studyday <= 60], na.rm = TRUE),   
    rcwhocps_max = max(rcwhocps[eventid != "V00"], na.rm = TRUE),
    rcwhocps_max28 = max(rcwhocps[eventid != "V00" & studyday <= 28], na.rm = TRUE),
    rcwhocps_max60 = max(rcwhocps[eventid != "V00" & studyday <= 60], na.rm = TRUE),   
    progression = case_when(
      survcens == "No" ~ "Yes",
      rcwhostate_bl == "Moderate" & rcwhostate_max == "Severe" ~ "Yes",
      TRUE ~ "No"
    ),
    progression28 = case_when(
      survcens_28 == "No" ~ "Yes",
      rcwhostate_bl == "Moderate" & rcwhostate_max28 == "Severe" ~ "Yes",
      TRUE ~ "No"
    ),
    progression60 = case_when(
      survcens_60 == "No" ~ "Yes",
      rcwhostate_bl == "Moderate" & rcwhostate_max60 == "Severe" ~ "Yes",
      TRUE ~ "No"
    ), 
    mv = if_else(rcwhocps_max >= 7, "Yes", "No"),
    mv28 = if_else(rcwhocps_max28 >= 7, "Yes", "No"),
    mv60 = if_else(rcwhocps_max60 >=7, "Yes", "No")
  ) %>%
  mutate(
    dischargeday = dphendt - randt,
    dischargecens = case_when(
    !is.na(dischargeday) ~ "No",
    is.na(dischargeday) & survcens == "Yes" ~ "Yes",
    is.na(dischargeday) & survcens == "No" ~ "No"
  ),
  dischargeday = case_when(
    !is.na(dischargeday) ~ dischargeday,
    is.na(dischargeday) & survcens == "Yes" ~ survtime,
    is.na(dischargeday) & survcens == "No" ~ dblockdate - randt
  ),
  dischargeday_28 = if_else(dischargeday <= 28, dischargeday, 28),
  dischargecens_28 = if_else(dischargeday <= 28, dischargecens, "Yes"),
  #dischargeday_28 = if_else(dischargecens == "No", dischargeday_28, if_else(dischargeday<=28, dischargeday, 28))
  ) %>% 
  mutate(
    mvdur = sum(rcwhocps >= 7, na.rm = TRUE),
    mvdaycens = is.na(mvday),
    mvtime = if_else(is.na(mvday), as.difftime(28, units = "days"), mvday)
  ) %>% 
  
  filter(eventid == "V00") %>%
  select(-studyday, -(eventid:rcwhostate), -starts_with("ranavail"), -(dmicdat:dmini), -(enrolled:fasex2)) %>% 
  ungroup()


write_rds(adev, "data/ad/adev.rds")

