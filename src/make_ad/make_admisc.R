##############################3
# Make miscellaneous analysis datasets
# Input: tdab tdvl
# Output adab advl
################################
library(tidyverse)
library(readr)

tdab <- read_rds("data/td/tdab.rds")
adsl<- read_rds("data/ad/adsl.rds")

adab <- adsl %>% 
  left_join(tdab %>% select(subjectid, studyday, absumctr, abrbd, abace2rbd, abcapsid ), by = "subjectid") %>% 
  mutate(abnormrbd = abrbd/absumctr*100,
         ablog10ace2rbd = log10(abace2rbd)) %>% 
  filter(studyday > 65) %>% 
  mutate(rantrt = factor(rantrt, ordered = FALSE)) %>% 
  mutate(abseroc = if_else(abrbd < 5, "RBD < 5", "RBD ≥ 5"),
         abseroc = factor(abseroc),
         abcapsidd = if_else(abcapsid < 10, "Capsid < 10", "Capsid ≥ 10"),
         abcapsidd = factor(abcapsidd)) %>% 
  labelled::set_variable_labels(abseroc = "Seroconverted (RBD ≥ 5)",
                      abcapsidd = "Seroconverted (Capsid ≥ 10)")

write_rds(adab, "data/ad/adab.rds")



tdvl<- readr::read_rds("data/td/tdvl.rds")

advl <- adsl %>% 
  left_join(tdvl, by = c("subjectid")) %>% 
  arrange(subjectid, vlsampledt) %>% 
  mutate(studyday = vlsampledt - bldt) %>% 
  mutate(vllog10cpkc_imp = if_else(vldetect == "Detected", vllog10cpkc, 0)) %>% 
  filter(studyday %in% c(-3:15) &
           vlsource %in% c("Labfile only", "Both")) %>% 
  mutate(epoch = case_when(
    studyday <= 1 ~ 0,
    studyday <= 5 ~ 4,
    studyday <= 10 ~ 8, 
    TRUE ~ 15
))

write_rds(advl, "data/ad/advl.rds")

tdrc <- readr::read_rds("data/td/tdrc.rds")

adrc <- adsl %>% 
  left_join(tdrc %>% select(subjectid, rcratio, eventdate, eventid), 
            by = "subjectid") %>% 
  mutate(studyday = eventdate - randt) %>% 
  mutate(epoch = case_when(
    studyday <= 0 ~ 0,
    studyday <= 5 ~ 4,
    studyday <= 10 ~ 8, 
    TRUE ~ 15
  )) %>% 
  mutate(rcratio = if_else(rcratio < 100, rcratio, NA_real_ ))

write_rds(adrc, "data/ad/adrc.rds")  
