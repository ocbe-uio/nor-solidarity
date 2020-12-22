


efflab_descriptives <- function(data = adlb,
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
    pivot_longer('Mean (SD)':'Missing / Non-Missing', names_to = "Statistic") %>%
    select(rantrt, studyday, Statistic, value) %>%
    pivot_wider(
      id_cols = studyday:Statistic,
      names_from = rantrt,
      values_from = value
    ) %>%
    rename('Days since randomisation' = studyday)
  
  return(data)
}


efflab_margins <- function(data = adlb,
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

quietly {model} {var} i.rantrt##i.studyday  || subjectid:  {options}
margins  i.rantrt, over(studyday) saving(`tmp')
use `tmp', clear

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
    select(margin, ci_lb:by1) %>%
    select(rantrt = m1 , studyday = by1, margin, ci_lb, ci_ub)


  return(res)
  
}

efflab_diffs <- function(data = adlb,
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
tempfile tmp1
tempfile tmp2
tempfile tmp3
tempfile tmp4


mkspline time_1 7 time_2 = studyday
gen day14 = studyday == 14

quietly {model} {var}  i.rantrt##c.(time_1 time_2)  || subjectid: {options}
margins i.rantrt, dydx(time_1) saving(`tmp1')
margins r.rantrt, dydx(time_1) saving(`tmp2')
margins i.rantrt, subpop(day14) saving(`tmp3')
margins r.rantrt, subpop(day14) saving(`tmp4')

use `tmp1', clear
append using `tmp2' `tmp3' `tmp4', gen(typec)

gen typen = typec
label def typec 0 \"Slope 1st week by treatment\" 1 \"Difference in 1st week slope\" 2 \"Day 14 level by treatment\" 3 \"Day 14 treatment difference\"
label val typec typec
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
    rename_with(~ str_replace(.x, "_", "")) %>%
    select( margin, pvalue, ci_lb:typen) %>%
    select( text = typec, rantrt = m1, margin,  ci_lb, ci_ub, pvalue) 
    
  return(res)
  
}




