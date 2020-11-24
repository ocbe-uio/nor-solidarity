factoriser <- function(data, codelist = items, delabel = TRUE) {
  x <- names(data)
  y <- codelist %>%
    filter(id %in% x) %>%
    filter(categorical == 1) %>%
    select(id,value_labels)
  
  if(length(y$id) == 0) {
    return(data)
  }
  
  for (i in 1:length(y$id)){
    ct <- y %>%
      slice(i) %>%
      unnest(cols = c(value_labels))  
    labs <- ct[["codevalue"]]
    names(labs) <- ct[["codetext"]]
    
    name1 <- y$id[[i]] 
    name2 <- paste0(y$id[[i]],"cd")
    
    data <- data %>%
      mutate_at(name2, as.numeric) %>%
      mutate_at(name2, haven::labelled, labels = labs) %>%
      mutate(!!name1 := as_factor(!!sym(name2), ordered = TRUE)) 
  }
  
  if (delabel == TRUE) mutate_if(data, haven::is.labelled, as.numeric)
  
  return(data)
}


labeliser <- function(data, codelist = items){
  x <- names(data)
  labels <- codelist %>%
    filter(id %in% x) %>%
    select(id,label) %>%
    spread(id, label) %>%
    as.list()
  
  labelled::var_label(data) <- labels
  
  return(data)
}

pick <- function(db, name) {
  db %>% dplyr::filter(id == name) %>% purrr::pluck("data",1)
}



###################
# Functions for tables
##################
mean_sd <- function(data, var, group, digits = 1) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(mean = mean(!!var, na.rm = TRUE), 
              sd = sd(!!var, na.rm = TRUE), 
              missing = sum(is.na(!!var))
              , .groups = "drop_last") %>% 
    mutate_at(vars(mean, sd), ~round(., digits = digits)) %>% 
    mutate(txt = paste0(mean, " (", sd, ")")) %>% 
    select(group, txt) %>% 
    deframe
}

median_iqr <- function(data, var, group, digits = 1) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(median = median(!!var, na.rm = TRUE), 
              q1 = quantile(!!var, probs = 0.25, na.rm = TRUE), 
              q3 = quantile(!!var, probs = 0.75, na.rm = TRUE), 
              missing = sum(is.na(!!var))
              , .groups = "drop_last") %>% 
    mutate(across(c(median, q1, q3), ~round(.x, digits = digits))) %>% 
    mutate(txt = paste0(median, " (", q1, " - ", q3, ")")) %>% 
    select(group, txt) %>% 
    deframe
}

n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    group_by(!!group, !!var, .drop = FALSE) %>% 
    summarise(n = n(),
              tot = n(), 
              .groups = "drop_last") %>% 
    group_by(!!group, .drop = TRUE) %>% 
    mutate(tot = sum(tot),
           pct = round(n/tot*100, digits = 1)) %>% 
    mutate(txt = paste0(n, " (", pct, "%)")) %>% 
    filter(!!var == !!level & tot>0) %>%
    select(group, txt) %>%
    deframe
}

empty <- function(data, var, group, ...){
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(n = n(), 
              .groups = "drop_last") %>% 
    mutate(txt = "") %>% 
    select(group,txt) %>% 
    deframe
}

missing_f <-  function(data, var, group, ...) {
  var <- ensym(var)
  group <- ensym(group)
  data %>% 
    group_by(!!group) %>% 
    summarise(tot = n(),
              non_miss = sum(!is.na(!!var)),
              miss = sum(is.na(!!var)),
              .groups = "drop_last") %>% 
    group_by(!!group) %>%
    mutate(pct = round(miss/tot*100,digits = 1)) %>%
    mutate(txt = paste0(miss, " (", pct, "%)")) %>%
    ungroup %>%
    select(group, txt) %>%
    deframe
}

ae_n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  
  data %>%
    group_by(subjectid, !!group, !!var) %>%
    summarise(n = sum(!!var),
              .groups = "drop_last") %>%
    group_by(!!group, !!var) %>%
    summarise(n_ae = sum(n),
              n_pat = n(),
              .groups = "drop_last") %>%
    group_by(!!group) %>%
    mutate(N_pat = sum(n_pat),
           pct = round(n_pat/N_pat*100,digits = 1),
           txt = paste0(n_pat, " (", pct, "%)")) %>%
    filter(!!var %in% !!level) %>%
    ungroup %>%
    select(!!group, txt) %>%
    deframe
}

ae_N_n_pct <-  function(data, var, group, level = 1) {
  var <- ensym(var)
  group <- ensym(group)
  
  data %>%
    group_by(subjectid, !!group) %>%
    summarise(n = sum(!!var),
              .groups = "drop_last") %>%
    mutate(!!var := if_else(n==0, 0, 1)) %>%
    group_by(!!group, !!var) %>%
    summarise(n_ae = sum(n),
              n_pat = n(),
              .groups = "drop_last") %>%
    group_by(!!group) %>%
    mutate(N_pat = sum(n_pat),
           pct = round(n_pat/N_pat*100,digits = 1),
           txt = paste0("[", n_ae,"] ", n_pat, " (", pct, "%)")) %>%
    mutate(txt = if_else(n_ae == 0, "[0] 0 (0%)", txt)) %>%
    filter(!!var %in% !!level) %>%
    ungroup %>%
    select(!!group, txt) %>%
    deframe
}

stats_exec <- function(f, data, var, group, ...){
  rlang::exec(f, data, var, group, !!!(...))
}
