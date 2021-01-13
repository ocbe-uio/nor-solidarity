
tdfiles <- tibble(filename =list.files("data/td/", pattern = ".rds$"), 
                  path = "data/td/")
adfiles <- tibble(filename =list.files("data/ad/", pattern = ".rds$"), 
                  path = "data/ad/")

datafiles <- tdfiles %>% 
  add_row(adfiles) %>% 
  mutate(file = paste0(path, filename),
         data = map(file, readr::read_rds),
         path_csv = paste0(path, "csv/", str_replace(filename, ".rds", ".csv")),
         path_dta = paste0(path, "dta/", str_replace(filename, ".rds", ".dta")),
         path_sav = paste0(path, "sav/", str_replace(filename, ".rds", ".sav")),
         path_sas = paste0(path, "sas/", str_replace(filename, ".rds", ".sas7bdat"))) 


datafiles %>%
  mutate(walk2(data, path_csv, readr::write_csv2),
         walk2(data, path_dta, haven::write_dta),
         walk2(data, path_sav, haven::write_sav),
         walk2(data, path_sas, haven::write_sas)) %>% 
  invisible()
