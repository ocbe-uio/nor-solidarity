library(tidyverse)

adev %>% select(subjectid, sq_admis_max) %>% write_csv2("data/work/misc/sq_admis_max.csv")

