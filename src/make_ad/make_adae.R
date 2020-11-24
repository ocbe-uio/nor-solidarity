library(tidyverse)
library(readr)

##################
# Make the adverse events analysis dataset
# Input: tdae, adsl
# Output: adae
#################

tdae <- read_rds("data/td/tdae.rds")
adsl <- read_rds("data/ad/adsl.rds")

adae <- adsl %>% 
  #  select(sitename, subjectid, dmicdat, age_calc, randt, rantrt) %>%
  left_join(tdae, by =  c("subjectid")) %>% 
  mutate(anyae = if_else(is.na(aespid), 0, 1),
         sae = if_else(is.na(aespid), 0, aesercd)
  ) %>%  
  group_by(subjectid) %>% 
  mutate(n_ae = sum(anyae),
         one_ae = n_ae == 1,
         two_ae = n_ae == 2,
         three_plus_ae = n_ae > 2,
         anysae = max(sae)) %>% 
  ungroup

readr::write_rds(adae, "data/ad/adae.rds")