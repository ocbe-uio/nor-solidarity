
library(tidyverse)
library(glue)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)==0) {
  export_name <- "ous_20201117_123041" #default export
} else if (length(args) != 0) {
  export_name <- args[1]
}



export_folder <- glue("data/raw/{export_name}")
                      
source("src/external/functions.R")


cl <- read_csv(glue("{export_folder}/{export_name}_CodeLists.csv"), skip = 1) %>%
  rename_all(tolower) %>%
  group_by(formatname) %>%
  nest(value_labels = c(datatype, codevalue, codetext))
items <- read_csv(glue("{export_folder}/{export_name}_Items.csv"), skip = 1) %>%
  rename_all(tolower) %>%
  mutate(id = tolower(id)) %>%
  mutate(categorical = if_else(!is.na(formatname), 2,
                               if_else(paste0(id, "cd") == lead(id), 1, 0)
  )) %>%
  mutate(formatname = if_else(categorical == 1, lead(formatname), formatname)) %>%
  left_join(cl, by = "formatname") %>%
  rename_all(tolower)



prefix <- str_sub(export_folder, -19)
raw <- tibble(files = list.files(export_folder)) %>%
  mutate(
    id = str_remove(files, paste0(prefix, "_")),
    id = str_remove(id, ".csv"),
    id = str_to_lower(id)
  ) %>%
  filter(!(id %in% c("items", "codelists", "readme.txt"))) %>%
  mutate(txt = map(paste0(export_folder, "/", files), read_csv, skip = 1)) %>%
  mutate(txt = map(txt, rename_all, tolower)) %>%
  mutate(txt = map(txt, labeliser, codelist = items)) %>%
  mutate(data = map(txt, factoriser, codelist = items)) %>%
  mutate(data = map(data, ~ mutate_if(., haven::is.labelled, as.numeric))) %>% 
  add_row(files= glue("{export_name}_CodeLists.csv"), id = "codelist", txt = list(cl), data = list(cl)) %>% 
  add_row(files= glue("{export_name}_Items.csv"), id = "items", txt = list(items), data = list(items))

write_rds(raw, "data/raw/raw.rds")
