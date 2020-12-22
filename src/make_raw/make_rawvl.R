library(readxl)

rawvl <- read_excel("data/raw/misc/ORO_data_til_Inge.xlsx")

readr::write_rds(rawvl, "data/raw/rawvl.rds")