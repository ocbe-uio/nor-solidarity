library(tidyverse)
library(haven)

addm <- read_rds("data/ad/addm.rds")

pid <- tribble(
  ~subjectid,
  "08-017",
  "04-018",
  "04-019",
  "04-020",
  "04-021",
  "03-004",
  "01-032",
  "06-016",
  "04-022",
  "04-026",
  "06-015",
  "04-025",
  "03-005",
  "06-014",
  "04-023",
)

addm_ecm <- pid %>% 
  left_join(addm, by = "subjectid")

write_sav(addm_ecm, "data/work/addm_ecm.sav")
write_dta(addm_ecm, "data/work/addm_ecm.dta")