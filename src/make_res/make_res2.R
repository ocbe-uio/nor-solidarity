library(tidyverse)
library(lubridate)
source("src/external/functions.R")

adev <- read_rds("data/ad/adev.rds")
adev <- read_rds("data/ad/adlb.rds")
adsl <- read_rds("data/ad/adsl.rds")

RR_f <- function(diff){
  if(length(diff$n) != 2)
    return ("Not applicable")
  
  RR <- (diff$obs[2]-diff$exp[2])/diff$var[2,2]
  RR_l <- RR - qnorm(0.975)*sqrt(1/diff$var[2,2])
  RR_u <- RR + qnorm(0.975)*sqrt(1/diff$var[2,2])
  
  txt <- paste0(round(exp(RR), digits = 2), " (95% CI ", 
                round(exp(RR_l), digits = 2), " to ", round(exp(RR_u), digits = 2), ")")
  return(txt)
}

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

#######################
# Lab results
#######################

library(tidyverse)

future::plan(future::multisession) 



cont_descriptives <- function(data = adlb,
                              var = "lbcrpres",
                              population = "fas",
                              digits = 1) {
  var <- ensym(var)
  population <- ensym(population)
  
  data <- data %>%
    filter(!!population == "Yes" &
              studyday %in% c(0:14)) %>%
    mutate(rantrt = fct_drop(rantrt)) %>%
    select(!!var, subjectid, studyday, rantrt) %>%
    group_by(studyday, rantrt) %>%
    summarise(
      mean = mean(!!var, na.rm = TRUE),
      sd = sd(!!var, na.rm = TRUE),
      median = median(!!var, na.rm = TRUE),
      q1 = quantile(!!var, 1 / 4, na.rm = TRUE),
      q3 = quantile(!!var, 3 / 4, na.rm = TRUE),
      missing = sum(is.na(!!var)), 
      nonmissing = sum(!is.na(!!var)),
      .groups = "drop_last"
    ) %>%
    ungroup() %>%
    mutate(across(mean:q3, ~ round(., digits = digits))) %>%
    mutate(
      'Mean (SD)' = paste0(mean, " (", sd, ")"),
      'Median [IQR]' = paste0(median, " [", q1, " - ", q3, "]"),
      'Missing / Non-Missing'  = paste0(as.character(missing), " / ", as.character(nonmissing))
    ) %>%
    select(-(mean:nonmissing)) %>%
    pivot_longer('Mean (SD)':'Non-Missing / Non-Missing', names_to = "Statistic") %>%
    select(rantrt, studyday, Statistic, value) %>%
    pivot_wider(
      id_cols = studyday:Statistic,
      names_from = rantrt,
      values_from = value
    ) %>%
    rename('Days since randomisation' = studyday)
  
  return(data)
}
cont_descriptives()


desc_plot <- function(data = adlb, population = "fas", var = "lbcrpres", model = "mixed", options = ""){
  var <- ensym(var)
  population <- ensym(population)
  
  plot <- data %>% 
    filter(!!population == "Yes" & studyday < 15) %>% 
    ggplot(aes(x=studyday_fct, y=!!var, fill=rantrt)) +
    geom_boxplot()
  return(plot)
}


cont_margins <- function(data = adlb,
                         model = "mixed",
                         var = "lbcrpres",
                         options = "",
                         population = "fas") {
  var_ <- ensym(var)
  population <- ensym(population)
  
  data <- data %>%
    filter(!!population == "Yes" &
             studyday %in% c(0:14)) %>%
    mutate(rantrt = fct_drop(rantrt)) %>%
    group_by(subjectid) %>%
    mutate(outcome = !!var_)
  
  x <- glue::glue(
    "
tempfile tmp

quietly {model} {var} i.rantrt i.studyday i.rantrt#i.studyday || subjectid: {options}
quietly margins studyday#rantrt,  saving(`tmp')
use `tmp', clear

"
  )
  
  margins <-  stata(
    src = x,
    data.in = data,
    data.out = TRUE,
    stata.path = "/usr/local/bin/stata-se",
    stata.version = 15,
    stata.echo = FALSE
  )
  
  res <- margins %>%
    rename_all(~ str_replace(., "_", "")) %>%
    select(margin, ci_lb:m2) %>%
    select(rantrt = m2, studyday = m1, margin, ci_lb, ci_ub) %>%
    print()
  
  return(res)
  
}

cont_margins()


cont_diffs <- function(data = adlb,
                         model = "mixed",
                         var = "lbcrpres",
                         options = "",
                         population = "fas") {
  var_ <- ensym(var)
  population <- ensym(population)
  
  data <- data %>%
    filter(!!population == "Yes" &
             studyday %in% c(0:14)) %>%
    mutate(rantrt = fct_drop(rantrt)) %>%
    group_by(subjectid) %>%
    mutate(outcome = !!var_)
  
  x <- glue::glue(
    "
tempfile tmp

quietly {model} {var} i.rantrt i.studyday i.rantrt#i.studyday || subjectid: {options}
quietly margins studyday, dydx(rantrt)  saving(`tmp')
use `tmp', clear

"
  )
  
  margins <-  stata(
    src = x,
    data.in = data,
    data.out = TRUE,
    stata.path = "/usr/local/bin/stata-se",
    stata.version = 15,
    stata.echo = FALSE
  )
  
  res <- margins %>%
    rename_all(~ str_replace(., "_", "")) %>%
    select(deriv, margin, pvalue, ci_lb:m1) %>%
    mutate(rantrt = fct_recode(
      deriv, 
      "Hydroxychloroquine + SOC vs SOC" = "2.rantrt",
      "Remdesivir + SOC vs SOC" = "3.rantrt"
    )) %>% 
    select(rantrt, studyday = m1, margin, pvalue, ci_lb, ci_ub) %>%
    print()
  
  return(res)
  
}

cont_diffs()


adlb <- adsl %>% 
  left_join(tdlb, by = "subjectid") %>% 
  select(subjectid:eventdate, lbcrpres, lbprores, lbldres, lbferres, lblymres, lbneures) %>% 
  group_by(subjectid) %>% 
  arrange(subjectid, eventdate) %>% 
  mutate(studyday = eventdate - first(eventdate)) %>% 
  ungroup %>% 
  mutate(across(.cols = lbcrpres:lbneures, ~.x + 0.001 )) %>%
  mutate(studyday_fct = factor(studyday, ordered = TRUE))



efflab_vars <- adlb  %>% 
  select(starts_with("lb")) %>% 
  labelled::var_label(unlist = TRUE) %>% 
  enframe %>% 
  rename(var = name, label = value)





efflab_results2 <- efflab_vars %>%
  mutate(order = row_number()) %>%
  crossing(population = c("fas_hcq", "fas_rem")) %>%
  mutate(digits = 2) %>%
  #filter( str_ends(var, "_log")) %>% 
  mutate(descriptives = pmap(list(data = list(adlb),
                                  var = var,
                                  population = population,
                                  digits = digits), cont_descriptives), 
         desc_plot = pmap(list(data = list(adlb), 
                              var = var,
                              population = population), desc_plot), 
        margins = furrr::future_pmap(list(data = list(adlb),
                                           var = var,
                                           population = population,
                                           model = "meglm",
                                           options = ", family(gamma)"), cont_margins),
        diffs = furrr::future_pmap(list(data = list(adlb),
                                         var = var,
                                         population = population,
                                         model = "meglm",
                                         options = ", family(gamma)"), cont_diffs)
        )

write_rds(efflab_results2, "results/rds/efflbres2.rds")
