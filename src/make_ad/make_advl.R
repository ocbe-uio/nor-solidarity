########################
# Make viral load analysis dataset
# Input: tdvl.rds adsl.rds
# Output: advl.rds
########################

library(tidyverse)

adsl <- readr::read_rds("data/ad/adsl.rds")
tdvl<- readr::read_rds("data/td/tdvl.rds")

advl <- adsl %>% 
  left_join(tdvl, by = c("subjectid")) %>% 
  group_by(subjectid) %>% 
  arrange(subjectid, vlsampledt) %>% 
  mutate(studyday = vlsampledt - randt) %>% 
  mutate(vllog10cpkc_imp = if_else(vldetect == "Detected", vllog10cpkc, 0))


haven::write_dta(advl, "data/work/advl.dta")
