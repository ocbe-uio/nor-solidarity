##############################3
# Make miscellaneous analysis datasets
# Input: tdab
# Output adab
################################
library(tidyverse)
library(readr)

tdab <- read_rds("data/td/tdab.rds")
adsl<- read_rds("data/ad/adsl.rds")

adab <- adsl %>% 
  left_join(tdab %>% select(subjectid, studyday, absumctr, abrbd, abace2rbd ), by = "subjectid") %>% 
  mutate(abnormrbd = abrbd/absumctr*100,
         ablog10ace2rbd = log10(abace2rbd)) %>% 
  filter(studyday >65) %>% 
  mutate(rantrt = factor(rantrt, ordered = FALSE))

write_rds(adab, "data/ad/adab.rds")

