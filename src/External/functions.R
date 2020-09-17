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
      mutate_at(name2, labelled, labels = labs) %>%
      mutate(!!name1 := as_factor(!!sym(name2), ordered = TRUE)) 
  }
  
  if (delabel == TRUE) mutate_if(data, is.labelled, as.numeric)
  
  return(data)
}


labeliser <- function(data, codelist = items){
  x <- names(data)
  labels <- codelist %>%
    filter(id %in% x) %>%
    select(id,label) %>%
    spread(id, label) %>%
    as.list()
  
  var_label(data) <- labels
  
  return(data)
}

pick <- function(db, name) {
  db %>% filter(id == name) %>% pluck("data",1)
}

# 
# factoriser <- function(data, codelist) {
#   x <- names(data)
#   y <- codelist %>%
#     filter(ID %in% x) %>%
#     filter(categorical == 1) %>%
#     select(ID,value_labels)
#   
#   for (i in 1:length(y$ID)){
#     ct <- y %>%
#       slice(i) %>%
#       unnest() %>%
#       pull(CodeText)
#     col <- y$ID[[i]]
#     data <- data %>%
#       mutate_at(col, factor, levels = ct)
#   }
#   
#   return(data)
# }