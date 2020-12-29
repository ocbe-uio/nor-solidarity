##############################3
# Make Results Dataset for antibody variables. 
# Input: adab
# Output rdab
################################

library(modmarg)
library(tidyverse)
adab <- readr::read_rds("data/ad/adab.rds")

source("src/External/functions.R")

label <- tibble(var = c("abrbd","abnormrbd","ablog10ace2rbd"),
                label = c("Receptor-bindind domain", 
                          "Normalised receptor-binding domain", 
                          "log10 ACE2 receptor-binding domain"), 
                seq = 1:3, 
                digits = c(1, 1, 2))

filter_f <- function(data, filtervar) {
  data %>% filter(!!ensym(filtervar) == "Yes")
}

marginlevels_f <- function(data, formula, digits){
  formula <- as.formula(formula)
  
  res <- glm(formula, data = data)
  levels <- modmarg::marg(mod = res, var_interest = "rantrt", type = "levels") %>% 
    deframe %>% 
    select(Label, Margin, lci = `Lower CI (95%)`, uci = `Upper CI (95%)`) %>% 
    mutate(Label = str_remove(Label, "rantrt = ")) %>%
    mutate(across(-Label, ~ round(., digits = digits))) %>% 
    mutate(txt = paste0(Margin, " (", lci, " to ", uci, ")")) %>% 
    select(Label, txt) %>% 
    deframe
  
  return(levels)
}

margineff_f <- function(data, formula, digits) {
  formula <- as.formula(formula)
  
  res <- glm(formula, data = data)
  effmarg <- modmarg::marg(mod = res, var_interest = "rantrt", type = "effects") %>% 
    deframe %>%  
    filter(!is.nan(Test.Stat)) %>% 
    select(Label, Margin, lci = `Lower CI (95%)`, uci = `Upper CI (95%)`) %>%
    mutate(Label = str_remove(Label, "rantrt = ")) %>%
    mutate(across(-Label, ~ round(., digits = digits))) %>%
    mutate(txt = paste0(Margin, " (", lci, " to ", uci, ")")) %>%
    select(Label, txt) %>%
    deframe
  
  return(effmarg)
}


rdab <- tibble(population = c("fas_hcq", "fas_rem"),
               pop_text = c("Hydroxychloroquine", "Remdesivir"),
) %>% 
  crossing(label) %>% 
  arrange(population, seq) %>% 
  mutate(data = list(adab),
         data = map2(data, population, filter_f),
         group = "rantrt",
         mean_text = pmap(list(data, var, group, digits), mean_sd),
         median_text = pmap(list(data, var, group, digits), median_iqr),
         id = map(mean_text, names), 
         formula = glue::glue("{var} ~ rantrt"),
         marglev = pmap(list(data, formula, digits) ,marginlevels_f), 
         marglev_id = map(marglev, names),
         margeff = pmap(list(data, formula, digits), margineff_f), 
         margeff_id = "Estimated marginal treatment effect"
  )

readr::write_rds(rdab, "results/rds/rdab.rds")

