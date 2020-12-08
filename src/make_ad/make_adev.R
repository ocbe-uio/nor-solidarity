##################
# Make the events analysis data set
# Input: tdds, adsl, tdrc
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
    survtime_60 = if_else(survcens == "No", survtime_60, 60), 
    survtime_28 = if_else(survtime <= 28, survtime, 28),
    survcens_28 = if_else(survtime <= 28, survcens, "Yes"),
    survtime_28 = if_else(survcens == "No", survtime_28, 28), 
  ) %>% 
  select(subjectid, survtime, survcens, survtime_60, survcens_60, survtime_28, survcens_28)



# adev <- adsl %>% 
#   left_join(tdrc %>% select(subjectid, eventid, eventdate, rcwhocps, rcwhostate), by = "subjectid") %>% 
#   left_join(survtime, by = "subjectid") %>% 
#   group_by(subjectid) %>% 
#   arrange(eventdate, .by_group = TRUE) %>% 
#   filter(eventid == "V00")
#            
adev <- adsl %>%
  left_join(tdrc %>% select(subjectid, eventid, eventdate, rcwhocps, rcwhostate), by = "subjectid") %>%
  left_join(survtime, by = "subjectid") %>%
  filter(fas == "Yes") %>%
  group_by(subjectid) %>%
  mutate(
    studyday = eventdate - eventdate[eventid == "V00"],
    rcwhostate_bl = rcwhostate[eventid == "V00"],
    rcwhostate_max14 = max(rcwhostate[eventid != "V00" & studyday < 14], na.rm = TRUE),
    progression = case_when(
      survcens == "No" ~ "Yes",
      rcwhostate_bl == "Moderate" & rcwhostate_max14 == "Severe" ~ "Yes",
      TRUE ~ "No"
    )
  ) %>%
  mutate(prog_time = if_else(rcwhostate_bl == "Moderate" & rcwhostate == "Severe", studyday, Inf),
         prog_time =  min(prog_time, na.rm = TRUE),
         prog_time = if_else(survcens == "No", min(survtime, prog_time), prog_time),
         prog_14 = case_when(
           progression == "Yes" & prog_time <= 14 ~ "Yes",
           survcens == "No" & progression == "No" & survtime <= 14 ~ "Yes",
           TRUE ~ "No"
         ),
         prog_time14 = if_else(prog_time <= 14, prog_time, 14)
  ) %>%
  filter(eventid == "V00") %>%
  select(-studyday, -(eventid:rcwhostate), -starts_with("ranavail"), -(dmicdat:dmini), -(enrolled:fasex2))


write_rds(adev, "data/ad/adev.rds")

