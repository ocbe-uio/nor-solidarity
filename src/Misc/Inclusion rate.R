library(tidyverse)



tdran <- read_rds("5.3 Analyser/Datasets/td/tdran.rds") 

tdran %>% 
  group_by(randt) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(cumulative = cumsum(n)) %>% 
  write_excel_csv("5.3 Analyser/Datasets/work/inclusion.csv", append = FALSE) %>% 
  write_excel_csv2("5.3 Analyser/Datasets/work/inclusion2.csv", append = FALSE) 
  
  
