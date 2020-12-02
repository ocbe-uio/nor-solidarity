library(tidyverse)
library(lubridate)
library(glue)
library(readr)
library(labelled)

source("src/external/functions.R")
tdds <- readr::read_rds("data/td/tdds.rds")

##################
# Make the events analysis data set
# Input: tdds, adsl
# Output: adev
##################


survtime <- tdds %>% 
  mutate(
    survtime = if_else(!is.na(eosdtdat), eosdtdat - randt, eosdat - randt),
    survtime = if_else(!is.na(survtime), survtime, dphendt - randt),
    survcens = if_else(is.na(eosdtdat), "Yes", "No"), 
    survtime_60 = if_else(survtime <= 60, survtime, 60),
    survcens_60 = if_else(survtime <= 60, survcens, "Yes"),
    survtime_60 = if_else(survcens == "No", survtime_60, 60)
  ) %>% 
  select(subjectid, survtime, survcens, survtime_60, survcens_60)


  
           
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
  ) 


%>% 
  filter(eventid == "V00") %>% 
  select(-studyday, -(eventid:rcwhostate), -starts_with("ranavail"), -(dmicdat:dmini), -(enrolled:fasex2))
  



