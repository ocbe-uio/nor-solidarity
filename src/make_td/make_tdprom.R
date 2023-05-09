##############################
# Make the PROMs tabulation dataset
# Input from raw: cat
# Input from other: utlevering-nor-solidarity.csv
# Output: tdprom
###############################


library(tidyverse)
library(lubridate)

source("src/external/functions.R")

raw <- readr::read_rds("data/raw/raw.rds")
tdqol_ <- readr::read_csv2("data/raw/utlevering-nor-solidarity.csv")
items <- raw %>% pick("items")



tdcat <- raw %>% 
  pick("cat") %>% 
  select(subjectid, eventname, eventdate, starts_with("copd")) %>% 
  select(-ends_with("cd")) %>% 
  mutate(across(starts_with("copd"), ~as.numeric(.x)-1)) %>%       
  labeliser(codelist = items) 

tdqol <- tdqol_ %>% 
  rename(subjectid = PasientIDHoved) %>% 
  mutate(eventdate = as.Date(FormDate)) %>% # for all but one CreationDate == FormDate == LastUpdate
  select(-Skjematype, -Bestillingstidspunkt, -Varslingskanal, -CreationDate, -FormDate, -LastUpdate) %>% 
  select(subjectid, eventdate, everything())

write_rds(tdcat, "data/td/tdcat.rds")
write_rds(tdqol, "data/td/tdqol.rds")
