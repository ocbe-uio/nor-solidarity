########################
# Make viral load analysis dataset
# Input: tdvl.rds adsl.rds
# Output: advl.rds
########################

library(tidyverse)

adsl <- readr::read_rds("data/ad/adsl.rds")
tdvl<- readr::read_rds("data/td/tdvl.rds")

rdlv_margins <- function(data = advl,
                           var = "vllog10cpkc_imp",
                           population = "fas") {
  var_ <- ensym(var)
  population <- ensym(population)
  
  data <- data %>%
    filter(!!population == "Yes" &
             studyday %in% c(-2:14) &
             vlsource %in% c("Labfile only", "Both")) %>%
    mutate(rantrt = fct_drop(rantrt),
           marginset = if_else(studyday %in% c(0, 7 ,14), studyday, NA_real_)) 
  
  x <- glue::glue(
    "
    
mkspline time_1 7 time_2  = studyday
gen tmp = studyday if studyday >= 0

tempfile tmp1
tempfile tmp2
tempfile tmp3

mixed {var} i.rantrt##c.(time_1 time_2)  || subjectid: studyday, covariance(unstructured)
*margins i.rantrt, at(time_1 = (0 7) time_2 = (0 7)) saving(`tmp1')
margins i.rantrt, over(marginset) saving(`tmp1')

margins i.rantrt, dydx(time_1) saving(`tmp2')
margins r.rantrt, dydx(time_1) saving(`tmp3')

use `tmp1', clear
append using `tmp2' `tmp3' , gen(analysis)

"
  )
  
  margins <-  stata(
    src = x,
    data.in = data,
    data.out = TRUE,
    stata.path = "/usr/local/bin/stata-se",
    stata.version = 16,
    stata.echo = FALSE
  )
  
  res <- margins #%>%
    # rename_all(~ str_replace(., "_", "")) %>%
    # select(rantrt = m1 , studyday = by1, margin, ci_lb, ci_ub, analysis)
  
  
  return(res)
  
}


advl <- adsl %>% 
  left_join(tdvl, by = c("subjectid")) %>% 
  group_by(subjectid) %>% 
  arrange(subjectid, vlsampledt) %>% 
  mutate(studyday = vlsampledt - randt) %>% 
  mutate(vllog10cpkc_imp = if_else(vldetect == "Detected", vllog10cpkc, 0)) %>% 
  ungroup()

rdlv_margins(population = "fas_rem")

rdvl <- tibble(pop = c("fas_hcq", "fas_rem")) %>%
   map( ~rdlv_margins(population = .x))


