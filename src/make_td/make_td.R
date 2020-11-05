library(tidyverse)
library(lubridate)
library(glue)

source("src/external/functions.R")


raw <- readr::read_rds("data/raw/raw.rds")
items <- raw %>% pick("items")


tdsq <- raw %>% 
  pick("sq") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sq")) %>% 
  select(-ends_with("cd"), -(sqegfr:sqaeyes1))  
write_rds(tdsq, "data/td/tdsq.rds")

tdsc <- raw %>% 
  pick("sc") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("sc")) %>% 
  select(-ends_with("cd"))
write_rds(tdsc, "data/td/tdsc.rds")

tdvs <- raw %>% 
  pick("vs") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid, eventdate, eventid, starts_with("vs")) %>% 
  select(-ends_with("cd")) %>% 
  mutate(vsobese = cut(vsbmi, breaks = c(-Inf, 30, Inf), label = c("Normal", "Obese"))) %>% 
  labelled::set_variable_labels(vsobese = "Obese (BMI > 30)?")
write_rds(tdvs, "data/td/tdvs.rds")

tdcc <- raw %>% 
  pick("cc") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid,  starts_with("cc")) %>% 
  select(-ends_with("cd")) 


tdef <- raw %>% 
  pick("ef") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid,  starts_with("ef")) %>% 
  select(-ends_with("cd"))


tdoa <- raw %>% 
  pick("oa") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid,  starts_with("oa")) %>% 
  select(-ends_with("cd"))

tdme <- raw %>% 
  pick("me") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid,  starts_with("me")) %>% 
  select(-ends_with("cd"))

tddph <- raw %>% 
  pick("dph") %>% 
  labeliser(codelist = items) %>% 
  group_by(subjectid) %>% 
  summarise(dphstdt = min(dphad), dphendt = max(dphad), .groups = "drop_last") %>% 
  labelled::set_variable_labels(dphstdt = "First discharge date",
                                dphendt = "Last discharge date") %>% 
  select(subjectid, dphstdt, dphendt) %>% 
  ungroup





###############
# Make tdran
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



# 
# Make tddm
# 

# •	Age
# •	Gender
# •	Obesity (Weight, hight/ BMI)
# •	SOFA 
# •	Blood pressure, at admission
# •	Temperature, at admission
# •	Oxygen requirements (number of liters of O2)
# •	Comorbidity: 
#   o	COPD 
# o	Diabetes 
# o	Hypertension
# o	Others? (autoimmune disases, known kidney- and liver disease)
# •	smoking (y/n)
# •	Descriptive data: Duration of SARS-CoV-2 illness prior to admittance, travel history, Health worker
# •	Medications: Immunosuppressive medication, anti-hypertensive medication



tddm <- raw %>% 
  pick("dm") %>% 
  select(sitename, subjectid, eventdate, starts_with("dm"), sex, brthdat) %>% 
  select(-ends_with("cd"), -dmwhoyn) %>% 
  labeliser(codelist = items) %>% 
  mutate(age_calc = interval(brthdat, dmicdat) %/% months(1), 
         age_calc = age_calc / 12,
         age_calc = round(age_calc, 1)) %>% 
  labelled::set_variable_labels(age_calc = "Age (years)") %>% 
  select(-dmsite, -dmransys, -dmwhoid, -dmcbp, -dm1) %>% 
  # Add admittance at baseline
  left_join(tdsq %>% filter(eventid == "V00") %>% select(subjectid, sq_admis), by = "subjectid") %>% 
  labelled::set_variable_labels(sq_admis = "Admitted to (baseline)") %>% 
  # Add baseline vital signs
  left_join(tdvs %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, vsweight, vsheight, vsbmi, vsobese, 
                     vssys, vsdia, vsmap, vstemp, vslitmin), 
            by = "subjectid") %>% 
  labelled::set_variable_labels(sq_admis = "Admitted to (baseline)") %>% 
  # Add baseline SOFA score
  left_join(tdsc %>% filter(eventid == "V00") %>% select(subjectid, scsumsc), by = "subjectid") %>% 
  labelled::set_variable_labels(scsumsc = "Baseline SOFA score") %>% 
  # Add co-morbidity and smoking
  left_join(tdcc %>%  select(-sitename, -sitecode), by = "subjectid") %>% 
  # Add epidemiological factors
  left_join(tdef %>%  select(-sitename, -sitecode), by = "subjectid")  %>% 
  # Add duration of disease etc
  left_join(tdoa %>%  select(-sitename, -sitecode), by = "subjectid") %>% 
  # Add baseline medications
  left_join(tdme %>%  select(-sitename, -sitecode), by = "subjectid")
  
  
write_rds(tddm, "data/td/tddm.rds")


tdds <- raw %>% 
  pick("eos") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("eos")) %>% 
  select( -(eosaechk:eosaeyes1cd), -eosyncd)


tdds <- tddm %>% 
  select(sitename, subjectid, dmicdat) %>% 
  left_join(select(tdran, subjectid, randt), by="subjectid") %>% 
  left_join(tdds, by = "subjectid") %>% 
  left_join(tddph, by = "subjectid")

write_rds(tdds, "data/td/tdds.rds")

###################################################
# Make tdae
###################################################





tdae <- raw %>% pick("ae") %>% 
  left_join(pick(raw,"meddra")) %>% 
  select(-(eventid:designversion)) %>% 
  labeliser()

write_rds(tdae, "data/td/tdae.rds")


#########################################
# Make tdex for study treatment exposure
########################################


tdex <- raw %>% pick("da") %>% 
  full_join(pick(raw,"dr")) %>%   
  select(-(eventid:designversion), -siteseq, -subjectseq, -eventseq, -ends_with("cd")) %>% 
  labeliser() 
  
write_rds(tdex, "data/td/tdex.rds")



