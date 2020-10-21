library(tidyverse)
library(lubridate)
library(glue)

# 
# Make tddm
# 


tddm <- raw %>% 
  pick("dm") %>% 
  select(sitename, sitecode, subjectid, eventdate, starts_with("dm"), sex, brthdat) %>% 
  select(-ends_with("cd"), -dmwhoyn) %>% 
  labeliser(codelist = items) 

tdsq <- raw %>% 
  pick("sq") %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, sq_admis) %>%
  filter(eventid == "V00") %>% 
  select(subjectid, sq_admis) %>% 
  labeliser(codelist = items)

tddm <- tddm %>% 
  left_join(tdsq, by = "subjectid")



###############
# Make tdran
##############

tdran <- bind_rows(pick(raw,"ran"), pick(raw,"ran123"), pick(raw,"ran13"), pick(raw, "who")) %>% 
  mutate(rantrt = case_when(
    !is.na(whotrt) ~ whotrt,
    !is.na(rantrt) ~ rantrt,
    !is.na(rantrt123) ~ rantrt123,
    !is.na(rantrt13) ~ rantrt13
  )) %>% 
  mutate(randt = case_when(
    !is.na(whorandt) ~ whorandt,
    !is.na(randt) ~ randt,
    !is.na(ran123dt) ~ ran123dt,
    !is.na(ran13dt) ~ ymd(ran13dt)
  )) %>% 
  mutate(ranavail = case_when(
    !is.na(whorandt) ~ str_split(glue("Standard of care (SOC);{whoarms2};{whoarms3}"),";"),
    !is.na(randt) ~ list(c("Standard of care (SOC)", "Hydroxychloroquine + SOC")),
    !is.na(ran123dt) ~ list(c("Standard of care (SOC)", "Hydroxychloroquine + SOC", "Remdesivir + SOC")),
    !is.na(ran13dt) ~ list(c("Standard of care (SOC)", "Remdesivir + SOC"))
  )) %>% 
  select(subjectid, rantrt, randt, ranavail)


write_rds(tdran, "data/td/tdran.rds")


###################################################
# Make tdae
###################################################





tdae <- raw %>% pick("ae") %>% 
  left_join(pick(raw,"meddra")) %>% 
  select(-(eventid:designversion)) %>% 
  labeliser()

write_rds(tdae, "data/td/tdae.rds")


#########################################
# Make tdex for study treatment exposure
########################################


tdex <- raw %>% pick("da") %>% 
  full_join(pick(raw,"dr")) %>%   
  select(-(eventid:designversion), -siteseq, -subjectseq, -eventseq, -ends_with("cd")) %>% 
  labeliser() 
  
write_rds(tdex, "data/td/tdex.rds")



