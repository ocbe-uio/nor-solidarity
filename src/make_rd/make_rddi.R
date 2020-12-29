##############################3
# Make Results Dataset for dichotomous variables. 
# Input: adev
# Output rddi
################################

library(modmarg)
library(tidyverse)
adev <- readr::read_rds("data/ad/adev.rds")
source("src/External/functions.R")

label <- tibble(var = c("survcens","survcens_28","survcens_60", "progression", 
                        "progression28", "progression60", "mv", "mv28", "mv60", 
                        "sq_admis_max", "sq_admis_max28", "sq_admis_max60"),
                label = c("Mortality during hospitalisation", 
                          "Mortality (censored at day 28)", 
                          "Mortality (censored at day 60)",
                          "WHO disease state progression",
                          "WHO disease state progression (censoreda at day 28)",
                          "WHO disease state progression (censoreda at day 60)",
                          "Mechanical ventilation during hospitalisation",
                          "Mechanical ventilation (censored at day 28)",
                          "Mechanical ventilation (censored at day 60)",
                          "Admission to ICU during hospitalisation",
                          "Admission to ICU (censored at day 28)",
                          "Admission to ICU (censored at day 60)"), 
                level = c(rep("No", 3), rep("Yes", 6), rep("ICU",3)),
                seq = 1:12)

filter_f <- function(data, filtervar) {
  data %>% filter(!!ensym(filtervar) == "Yes")
}

marginlevels_f <- function(data, formula){
  formula <- as.formula(formula)
  
  res <- glm(formula, data = data, family = binomial)
  levels <- modmarg::marg(mod = res, var_interest = "rantrt", type = "levels") %>% 
    deframe %>% 
    select(Label, Margin, lci = `Lower CI (95%)`, uci = `Upper CI (95%)`) %>% 
    mutate(Label = str_remove(Label, "rantrt = ")) %>%
    mutate(across(-Label, ~ round(.*100, digits = 1))) %>% 
    mutate(txt = paste0(Margin, "% (", lci, " to ", uci, ")")) %>% 
    select(Label, txt) %>% 
    deframe
  
  return(levels)
}

margineff_f <- function(data, formula){
  formula <- as.formula(formula)
  
  res <- glm(formula, data = data, family = binomial)
  effmarg <- modmarg::marg(mod = res, var_interest = "rantrt", type = "effects") %>% 
    deframe %>%  
    filter(!is.nan(Test.Stat)) %>% 
    select(Label, Margin, lci = `Lower CI (95%)`, uci = `Upper CI (95%)`) %>%
    mutate(Label = str_remove(Label, "rantrt = ")) %>%
    mutate(across(-Label, ~ round(.*100, digits = 1))) %>%
    mutate(txt = paste0(Margin, "% (", lci, " to ", uci, ")")) %>%
    select(Label, txt) %>%
    deframe
  
  return(effmarg)
}


rddi <- tibble(population = c("fas_hcq", "fas_rem"),
               pop_text = c("Hydroxychloroquine", "Remdesivir"),
) %>% 
  crossing(label) %>% 
  arrange(population, seq) %>% 
  mutate(data = list(adev),
         data = map2(data, population, filter_f),
         group = "rantrt",
         desc_text = pmap(list(data, var, group, level), n_pct),
         desc_id = map(desc_text, names), 
         formula = glue::glue("{var} == \"{level}\" ~ rantrt"),
         marglev = map2(data, formula, marginlevels_f), 
         marglev_id = map(marglev, names),
         margeff = map2(data, formula, margineff_f), 
         margeff_id = "Estimated marginal treatment effect"
  )

readr::write_rds(rddi, "results/rds/rddi.rds")

