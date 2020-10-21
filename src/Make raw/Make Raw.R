library(haven)
library(readxl)
library(tidyverse)
library(lme4)
library(emmeans)
library(margins)
library(broom)
library(labelled)
library(arsenal)
library(kableExtra)
#library(knitr)
library(ggformula)
library(stringr)
library(rlang)
library(glue)

export_name <- "ous_20201016_084109"
export_folder <- glue("data/raw/{export_name}")
                      
source("src/External/functions.R")


cl <- read_csv(glue("{export_folder}/{export_name}_CodeLists.csv"), skip = 1) %>%
  rename_all(tolower) %>%
  group_by(formatname) %>%
  #mutate(codetext = if_else(is.na(codetext), "Not Done", codetext)) %>%
  #mutate(codetext = if_else(formatname == "UNKFMT", "Unknown", codetext)) %>%
  #mutate(codetext = if_else(formatname == "AEFMT", "Confirmed", codetext)) %>%
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
  mutate(data = map(data, ~ mutate_if(., haven::is.labelled, as.numeric)))

write_rds(raw, "data/raw/raw.rds")
