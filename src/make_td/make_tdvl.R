########################
# Make viral load tabulation dataset
# Input: rawvl.rds
# Output: tdvl.rds
########################

library(tidyverse)
library(readr)
library(labelled)
source("src/external/functions.R")

raw <- readr::read_rds("data/raw/raw.rds")
rawvl <- read_rds("data/raw/rawvl.rds")
tdran <- read_rds("data/td/tdran.rds")
items <- raw %>% pick("items")


tdvl_ <- rawvl %>% 
  rename(subjectid = viedoc, 
         vlsampledt = Prøvedato, 
         vlcpmcl = `c/µl`,  
         vlcpkcell = `copies/1000 celler`,
         vldetect = `log10 copies/1000 celler`) %>% 
  mutate(vldetect = case_when(
    vldetect == "ND" ~ "Not Detected",
    vldetect == "SP" ~ "Weakly Positive",
    TRUE ~ "Detected")
  ) %>% 
  mutate(vllog10cml = log10(vlcpmcl*1000)) %>% 

  mutate(vllog10cpkc = log10(suppressWarnings(as.numeric(vlcpkcell))))  %>% 
  set_variable_labels(subjectid = "Subject Id",
                      vlsampledt = "Event date",
                      vlcpmcl = "Copies per µL",
                      vlcpkcell = "copies/1000 cells",
                      vllog10cpkc = "Log10 copies/1000 cells",
                      vldetect = "Detectable copies?",
                      vllog10cml = "Log10 copies per mL")

tdvl <- raw %>% 
  pick("bb") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventid, eventdate, starts_with("bbnas"), bblocal) %>% 
  select(-ends_with("cd"),-bbnassp) %>% 
  filter(bbnasyn == "Yes") %>% 
  full_join(tdvl_, by = c("subjectid", "bbnasdt" = "vlsampledt")) %>% 
  arrange(subjectid, bbnasdt) %>% 
  mutate(vlsource = case_when(
    !is.na(eventid) & !is.na(vldetect) ~ "Both",
    is.na(eventid) & !is.na(vldetect) ~ "Labfile only",
    !is.na(eventid) & is.na(vldetect) ~ "Viedoc only",
    TRUE ~ "None"
  )) %>% 
  rename(vlnasyn = bbnasyn,  vlsampledt = bbnasdt, vllocal = bblocal ) %>% 
  mutate(vlsource = factor(vlsource)) %>% 
  mutate(vlsampledt = lubridate::as_date(vlsampledt)) %>% 
  set_variable_labels(vlsampledt = "Sample date", 
                      vllocal = "Sample localization", 
                      vlsource = "Source")


  
write_rds(tdvl, "data/td/tdvl.rds")
        
  
