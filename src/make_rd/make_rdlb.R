#######################
# Make Lab results
#######################

library(tidyverse)
library(lubridate)
source("src/make_rd/rdlb_functions.R")
source("src/make_rd/stata.R")

tdlb <- read_rds("data/td/tdlb.rds")
adsl <- read_rds("data/ad/adsl.rds")

adlb <- adsl %>% 
  left_join(tdlb, by = "subjectid") %>% 
  select(subjectid:eventdate, lbcrpres, lbprores, lbldres, lbferres, lblymres, lbneures) %>% 
  group_by(subjectid) %>% 
  arrange(subjectid, eventdate) %>% 
  mutate(studyday = eventdate - first(eventdate)) %>% 
  mutate(across(.cols = lbcrpres:lbneures, ~.x + 0.001 )) %>%
  #mutate(across(lbcrpres:lbneures, ~ first(.x), .names = "{.col}_bl")) %>% 
  ungroup %>% 
  mutate(studyday_fct = factor(studyday, ordered = TRUE)) 



efflab_vars <- adlb  %>% 
  select(starts_with("lb")) %>% 
  labelled::var_label(unlist = TRUE) %>% 
  enframe %>% 
  rename(var = name, label = value)


future::plan(future::multisession) 

efflab_results <- efflab_vars %>%
  mutate(order = row_number()) %>%
  crossing(population = c("fas_hcq", "fas_rem")) %>%
  mutate(digits = 2) %>%
  mutate(descriptives = pmap(list(data = list(adlb),
                                  var = var,
                                  population = population,
                                  digits = digits), efflab_descriptives),
         margins = furrr::future_pmap(list(data = list(adlb),
                                           var = var,
                                           population = population,
                                           model = "meglm",
                                           options = ", family(gamma)"), efflab_margins),
         diffs = furrr::future_pmap(list(data = list(adlb),
                                         var = var,
                                         population = population,
                                         model = "meglm",
                                         options = ", family(gamma)"), efflab_diffs)
  )

write_rds(efflab_results, "results/rds/rdlb.rds")

warnings()
