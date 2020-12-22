library(readxl)
library(readr)

rawab <- read_excel("data/raw/misc/2011202_Remdesivir_study_Antibody_data.xlsx", skip = 1)

write_rds(rawab, "data/raw/rawab.rds")

