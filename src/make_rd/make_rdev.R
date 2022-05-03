#################################
# Make Events Result Datasets (evrd)
#################################

library(tidyverse)
library(survival)
library(survminer)

adev <- read_rds("data/ad/adev.rds")

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

cox_f <- function(tc){
  if(is.na(tc$p.value[1]))
    return ("Not applicable")
  
  tc <- slice(tc,1)
  HR <- exp(tc$estimate)
  HR_l <- exp(tc$conf.low)
  HR_u <- exp(tc$conf.high)
  
  txt <- paste0(round(HR, digits = 2), " (95% CI ", 
                round(HR_l, digits = 2), " to ", round(HR_u, digits = 2), ")")
  pval = tc$p.value
  ret = list(txt = txt, pval = pval)
  return(ret)
}

data <- tibble(
  data = list(adev, adev %>% filter(fas_hcq == "Yes"), adev %>% filter(fas_rem == "Yes")),
  dname = c("All", "Hydroxychloroquine only", "Remdesivir only")            
)
survres <- tibble(
  formula = list( Surv(survtime, survcens == "No") ~ rantrt,
                  Surv(survtime_60, survcens_60 == "No") ~ rantrt, 
                  Surv(survtime_28, survcens_28 == "No") ~ rantrt,
                  Surv(mvtime, mvdaycens == 0) ~ rantrt),
  fname = c("Full timeframe", "Censored at 60 days", "Censored at 28 days", "Time to first mechanical ventilation (cens 28 days)")
) %>% 
  crossing(data) %>% 
  mutate(fit = map2(formula, data, ~surv_fit(formula = .x, data = .y)),
         plot = map2(fit, data, ~ggsurvplot(fit = .x, data = .y, fun = "event", ylim = c(0, 0.2))),
         diff = map2(formula, data, ~survdiff(formula = .x, data = .y)),
         chisq = map_dbl(diff, ~.x$chisq),
         pval = map_dbl(diff, ~round(1-pchisq(.x$chisq, length(.x$obs)-1), digits = 3)),
         RR = map_chr(diff, ~RR_f(.x)),
         coxfit = map2(formula, data, ~coxph(formula = .x, data = .y)),
         tidycox = map(coxfit, broom::tidy, conf.int = "TRUE"), 
         cox_txt = map(tidycox, cox_f),
         HR = map_chr(cox_txt, ~.x$txt),
         cox_p = map_dbl(cox_txt, ~round(.x$pval, digits = 3)),
         HR = if_else(RR == "Not applicable", RR, HR)
         )


write_rds(survres, "results/rds/rdev.rds")

