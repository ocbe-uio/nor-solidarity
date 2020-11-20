

###############
# Make tdran
# Input from raw: ran, ran123, ran13, who
##############

tdran <- bind_rows(pick(raw,"ran"), pick(raw,"ran123"), pick(raw,"ran13"), pick(raw, "who")) %>% 
  mutate(ranavail_rem = if_else( !is.na(whoarms3) | 
                                   !is.na(ran123dt) |
                                   !is.na(ran13dt), "Yes", "No")) %>% 
  mutate(ranavail_hcq = if_else( !is.na(whoarms2cd) | 
                                   !is.na(ran123dt) |
                                   !is.na(randt), "Yes", "No")) %>%
  mutate(rantrt = case_when(
    !is.na(whotrt) ~ whotrt,
    !is.na(rantrt) ~ rantrt,
    !is.na(rantrt123) ~ rantrt123,
    !is.na(rantrt13) ~ rantrt13
  )) %>% 
  mutate(randt = case_when(
    !is.na(whorandt) ~ whorandt,
    !is.na(randt) ~ randt,
    !is.na(ran123dt) ~ ran123dt,
    !is.na(ran13dt) ~ ymd(ran13dt)
  )) %>% 
  mutate(rantrtcd = as.numeric(rantrt)) %>%
  mutate(across(starts_with("ranavail_"), ~ factor(.x, levels = c("No", "Yes"), ordered = TRUE))) %>% 
  select(subjectid, rantrt, rantrtcd, randt, starts_with("ranavail")) %>% 
  labeliser(codelist = items) %>% 
  labelled::set_variable_labels(ranavail_rem = "Remdesivir available?",
                                ranavail_hcq = "Hydroxychloroquine available?") %>% 
  arrange(randt)



write_rds(tdran, "data/td/tdran.rds")
