##########################
# Subgroupanalyses for viral load
#########################

sg_margins_f <- function(data,
                         sg_var = "sex") {
  
  data <- data %>%
    mutate(rantrt = fct_drop(rantrt), 
           marginset = if_else(studyday %in% c(0, 4 , 8, 15), 
                                studyday, NA_real_)) 
  
  x <- glue::glue(
    "
gen studyday_ = studyday + 100
mkspline time_1 4 time_2 8 time_3 = studyday
mkspline time1 7 time2  = studyday

tempfile tmp1
tempfile tmp2
tempfile tmp3
tempfile tmp4
tempfile tmp5
tempfile tmp6

mixed vllog10cpkc_imp i.rantrt##c.(time_1 time_2 time_3)##i.{sg_var}  || subjectid: 
margins i.rantrt#i.{sg_var}, over(marginset) saving(`tmp1')

mixed vllog10cpkc_imp i.rantrt##c.(time1 time2)##i.{sg_var}  || subjectid:
margins i.rantrt#i.{sg_var}, dydx(time1) saving(`tmp2')
margins r.rantrt, dydx(time1) over(i.{sg_var}) saving(`tmp3')
margins r.rantrt#r.{sg_var}, dydx(time1) saving(`tmp4')

mixed vllog10cpkc_imp i.rantrt##i.studyday_##i.{sg_var} || subjectid:
margins i.rantrt#i.{sg_var}, over(studyday_) saving(`tmp5')

use `tmp3', clear
gen _m2 = _by1
save, replace

use `tmp5', clear
replace _by1 = _by1 - 100
save, replace

use `tmp1', clear
append using `tmp2' `tmp3' `tmp4' `tmp5' , gen(analysis)

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
    select(rantrt = m1, subgroup = m2, studyday = by1, subgroup = m2, margin, ci_lb, ci_ub, pvalue, analysis) 
  
  return(res)
  
}

#sg_margins_f(advl %>% filter(fas_rem == "Yes"))

subgroups <- addm %>% 
  select(subjectid, rcwhostate, abseroc, sympdur, 
         dmage, vllog10cpkc, lbcrpres, lbferres, lblymres) %>% 
  mutate(age_cat = if_else(dmage < 60, "age < 60 years", "age ≥ 60 years"),
         sympdur_cat = if_else(sympdur < 7, "Symptom duration < 7 days", "Symptom duration ≥ 7 days"),
         vl_median = median(vllog10cpkc, na.rm = TRUE),
         vl_cat = if_else(vllog10cpkc < median(vllog10cpkc, na.rm = TRUE), "Low viral load", "High viral load"),
         crp_median = median(lbcrpres, na.rm = TRUE),
         crp_cat = if_else(lbcrpres < median(lbcrpres, na.rm = TRUE),"Low CRP", "High CRP"),
         fer_median = median(lbferres, na.rm = TRUE),
         fer_cat = if_else(lbferres < median(lbferres, na.rm = TRUE), "Low Ferritin", "High Ferritin"),
         lym_median = median(lblymres, na.rm = TRUE),
         lym_cat = if_else(lblymres < 0.5, "Low Lymphocytes", "High Lymphocytes")) %>% 
  select(subjectid, rcwhostate, abseroc, ends_with("_cat"), ends_with("_median")) %>% 
  mutate(across(ends_with("_cat"), factor))

advl_sg <- advl %>% 
  left_join(subgroups, by = "subjectid")

sg_names <- tibble(sg_var = c("age_cat", "vl_cat", "crp_cat", "fer_cat", "lym_cat"),
                   sg_label = c("Age", "Viral load", "CRP", "Ferritin", "Lymphocytes"))


plot_sg_margins1 <- function(data){
  data <- data %>% filter(analysis == 0)
  plot_cont_margins(data, ytitle = "Viral~load~(log[10]~copies~per~1000~cells)") + 
    facet_wrap(~subgroup) + 
    theme(legend.position = "bottom")
}

plot_sg_margins2 <- function(data){
  data <- data %>% 
    filter(analysis == 4) %>% 
    filter(!is.na(margin))
  plot_cont_margins(data, ytitle = "Viral~load~(log[10]~copies~per~1000~cells)") + 
    facet_wrap(~subgroup) + 
    theme(legend.position = "bottom")
}

mk_table <- function(margin, pop_text) {
  margin %>% 
    filter(analysis %in% c(1, 2, 3, 4)) %>% 
    mutate(Explanation = case_when(
      analysis == 1 ~ "Slope 1st week by treatment and subgroup",
      analysis == 2 ~ "Difference in slope",
      analysis == 3 ~ "Treatment/Subgroup interaction") %>% 
    select(-analysis) %>%
    select(rantrt, subgroup, Explanation, margin, ci_lb, ci_ub, pvalue) %>% 
    knitr::kable(col.names = c("Treatment",
                               "Subgroup",
                               "Explanation",
                               "Estimate",
                               "Lower 95% CL",
                               "Upper 95% CL",
                               "P-value"),
                 digits = 3, caption = glue::glue("Estimated treatment effect, {pop_text}"))
}


rdvl_sg <-  tibble(population = c("fas_hcq", "fas_rem"),
                     pop_text = c("Hydroxychloroquine", "Remdesivir"))  %>% 
  crossing(sg_names) %>% 
  mutate(data = list(advl_sg),
         data = map2(data, population, filter_f),
         margins = map2(data, sg_var, sg_margins_f))
  
plot_sg_margins1(rdvl_sg$margins[[1]])

rdvl_sg1 <- rdvl_sg %>% 
  mutate(plot1 = map(margins, plot_sg_margins1),
         plot2 = map(margins, plot_sg_margins2))
  
plot(rdvl_sg1$plot2[[1]])





