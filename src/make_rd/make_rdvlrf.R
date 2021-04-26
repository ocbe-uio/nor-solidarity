####################
# Make viral load and respiratory failure results dataset
# Input: advl adrc 
# Output: rdvlrf
#################


library(modmarg)
library(tidyverse)
library(ggformula)
advl <- readr::read_rds("data/ad/advl.rds")
adrc <- readr::read_rds("data/ad/adrc.rds")
source("src/make_rd/stata.R")

margins_f <- function(data,
                         var = "vllog10cpkc_imp") {

  data <- data %>%
    mutate(rantrt = fct_drop(rantrt), 
           marginset = if_else(studyday %in% c(0, 4 , 8, 15), 
                               studyday, NA_real_)) 
  
  x <- glue::glue(
    "
gen studyday_ = studyday + 100
mkspline time_1 4 time_2 8 time_3 = studyday
mkspline time1 7 time2  = studyday
gen day10 = studyday == 10

tempfile tmp1
tempfile tmp2
tempfile tmp3
tempfile tmp4
tempfile tmp5
tempfile tmp6

mixed {var} i.rantrt##c.(time_1 time_2 time_3)  || subjectid: 
margins i.rantrt, over(marginset) saving(`tmp1')

mixed {var} i.rantrt##c.(time1 time2)  || subjectid: studyday, covariance(unstructured)
margins i.rantrt, dydx(time1) saving(`tmp2')
margins r.rantrt, dydx(time1) saving(`tmp3')
margins i.rantrt, at(time1 = 7 time2=3) saving(`tmp4')
margins r.rantrt, subpop(day10) saving(`tmp5')

mixed {var} i.rantrt##i.studyday_ || subjectid:
margins i.rantrt, over(studyday_) saving(`tmp6')

use `tmp6', clear
replace _by1 = _by1 - 100
save, replace

use `tmp1', clear
append using `tmp2' `tmp3' `tmp4' `tmp5' `tmp6' , gen(analysis)

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
  
  res <- margins %>%
    rename_all(~ str_replace(., "_", "")) %>%
    select(rantrt = m1 , studyday = by1, margin, ci_lb, ci_ub, pvalue, analysis) 
  
  return(res)
  
}


plot_cont_margins <- function(data, ytitle = "Value") {
  data %>%
    gf_line(
      margin ~ studyday,
      color = ~ rantrt,
      group = ~ rantrt,
      position = position_dodge(0.4),
      size = 1
    ) %>%
    gf_point(position = position_dodge(0.4)) %>%
    gf_errorbar(
      ci_lb + ci_ub ~ studyday,
      color = ~ rantrt,
      width = .8,
      position = position_dodge(0.4)
    ) %>%
    gf_labs(x = "Study day",
            y = str2expression(ytitle),
            color = "Treatment") %>%
    gf_theme(theme_classic())
  
}

filter_f <- function(data, filtervar) {
  data %>% filter(!!ensym(filtervar) == "Yes")
}

mk_table <- function(margin, pop_text) {
  margin %>% 
    filter(analysis %in% c(1, 2, 3, 4)) %>% 
    mutate(Explanation = case_when(
      analysis == 1 ~ "Slope 1st week",
      analysis == 2 ~ "Difference in slope",
      analysis == 3 ~ "Level at day 10",
      analysis == 4 ~ "Difference at day 10")) %>% 
    select(-analysis) %>%
    select(rantrt, Explanation, margin, ci_lb, ci_ub, pvalue) %>% 
    knitr::kable(col.names = c("Treatment",
                               "Explanation",
                               "Estimate",
                               "Lower 95% CL",
                               "Upper 95% CL",
                               "P-value"),
                 digits = 3, caption = glue::glue("Estimated treatment effect, {pop_text}"))
}

plot_cont_margins1 <- function(data, ytitle){
  
  data <- data %>% filter(analysis == 0)
  plot_cont_margins(data, ytitle)
}

plot_cont_margins2 <- function(data, ytitle){
  
  data <- data %>%
    filter(analysis == 5) %>%
    filter(studyday %in% c(0:14)) 
  plot_cont_margins(data, ytitle)
}

plot_cont_boxplot <- function(data, ytitle, var){
  var <- sym(var)
  
  ggplot(data = data, aes(x = factor(epoch), y = {{var}}, fill = rantrt) ) +
    geom_boxplot() +
    ggpubr::theme_classic2() + 
    labs(x = "Study day",
               y = str2expression(ytitle),
               fill = "Treatment") + 
  theme(legend.position = c(0.8, 0.8))
}


label <- tibble(var = c("vllog10cpkc_imp", "rcratio"),
                label = c("Viral load (log~10~ copies per 1000 cells)", 
                          "pO~2~/FiO~2~-ratio"), 
                ytitle = c("Viral~load~(log[10]~copies~per~1000~cells)",
                           "pO[2]/FiO[2]-ratio"),
                seq = 1:2)



adpf <- adrc %>%
  filter(studyday %in% c(-1:15))

rdvlrf <- tibble(population = c("fas_hcq", "fas_rem"),
               pop_text = c("Hydroxychloroquine", "Remdesivir"),
) %>%
  crossing(label) %>%
  arrange(population, seq) %>%
  mutate(data = if_else(seq == 1, list(advl), list(adpf)),
         data = map2(data, population, filter_f),
         margins = map2(data, var, margins_f),
         plot1 = map2(margins, ytitle, plot_cont_margins1),
         plot2 = map2(margins, ytitle, plot_cont_margins2),
         table = map2(margins, pop_text, mk_table),
         boxplots = pmap(list(data = data, ytitle = ytitle, var = var), plot_cont_boxplot)
         )



######################################
# Make ready for simple analysis with boxplots according to review by Annals of internal Medicine
#####################################






readr::write_rds(rdvlrf, "results/rds/rdvlrf.rds")

