library(tidyverse)
library(lubridate)
library(survival)
library(survminer)
source("src/external/functions.R")
source("src/make_res/res_functions.R")
source("src/make_res/stata.R")

adev <- read_rds("data/ad/adev.rds")
tdlb <- read_rds("data/td/tdlb.rds")
adsl <- read_rds("data/ad/adsl.rds")


data <- tibble(
  data = list(adev, adev %>% filter(fas_hcq == "Yes"), adev %>% filter(fas_rem == "Yes")),
  dname = c("All", "Hydroxychloroquine only", "Remdesivir only")            
)
survres <- tibble(
  formula = list( Surv(survtime, survcens == "No") ~ rantrt,
                  Surv(survtime_60, survcens_60 == "No") ~ rantrt, 
                  Surv(survtime_28, survcens_28 == "No") ~ rantrt),
  fname = c("Full timeframe", "Censored at 60 days", "Censored at 28 days")
) %>% 
  crossing(data) %>% 
  mutate(fit = map2(formula, data, ~surv_fit(formula = .x, data = .y)),
         plot = map2(fit, data, ~ggsurvplot(fit = .x, data = .y, fun = "event", ylim = c(0, 0.2))),
         diff = map2(formula, data, ~survdiff(formula = .x, data = .y)),
         chisq = map_dbl(diff, ~.x$chisq),
         pval = map_dbl(diff, ~round(1-pchisq(.x$chisq, length(.x$obs)-1), digits = 3)),
         RR = map_chr(diff, ~RR_f(.x)))


write_rds(survres, "results/rds/mortres.rds")


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



#######################
# Lab results
#######################

library(tidyverse)

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

write_rds(efflab_results, "results/rds/efflbres.rds")

warnings()
