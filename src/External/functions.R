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
  db %>% filter(id == name) %>% purrr::pluck("data",1)
}

