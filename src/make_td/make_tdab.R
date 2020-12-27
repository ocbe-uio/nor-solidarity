#############################
# Make Antibodies tabulation dataset
# Input rawab.rds
# Output tdab.rds
##############################

library(tidyverse)
rawab <- readr::read_rds("data/raw/rawab.rds")

tdab <- rawab %>% 
  mutate(subjectid = str_trunc(`Sample Id`, 6, ellipsis = "")) %>% 
  select( -`Sample Id`, -hospital, -patient, -days...7,  -COHORT, -Cohortnumber) %>% 
  select(subjectid, studyday = days...4, absumctr = `sum controls`, abrbd = RBD, 
         abace2rbd = `ACE2 RBD`, everything()) 
  

write_rds(tdab, "data/td/tdab.rds")
