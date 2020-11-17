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



tdcc <- raw %>% 
  pick("cc") %>% 
  select(sitename, sitecode, subjectid,  starts_with("cc")) %>% 
  select(-ends_with("cd")) %>% 
  mutate(across(cc_card:cc_other, ~if_else(cc_known == "Yes", .x, factor("No", order = TRUE)))) %>%       labeliser(codelist = items) %>% 
  mutate(cc_eversmoker = case_when(
    cc_tobac == "Yes" ~ "Yes", 
    cc_tobac == "Former smoker" ~ "Yes",
    cc_tobac == "Never smoked" ~ "No",
    cc_tobac == "Unknown" ~ "Unknown", 
    is.na(cc_tobac) ~ NA_character_,
    TRUE ~ NA_character_),
    cc_eversmoker = factor(cc_eversmoker, levels = c("No", "Yes", "Unknown"))
  ) %>% 
  labelled::set_variable_labels(cc_eversmoker = "Ever smoker?") 
  
tdcm <- raw %>% pick("cm") %>% 
  left_join(pick(raw,"atc_without_ddd")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser() 

write_rds(tdae, "data/td/tdae.rds")

tdef <- raw %>% 
  pick("ef") %>% 
  labeliser(codelist = items) %>% 
  select(sitename, sitecode, subjectid,  starts_with("ef")) %>% 
  select(-ends_with("cd"))


tdoa <- raw %>% 
  pick("oa") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid,  starts_with("oa")) %>% 
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



tdlbb <- raw %>% 
  pick("lbb") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("event"), starts_with("lb")) %>% 
  select(-ends_with("cd"))

tdlbh <- raw %>% 
  pick("lbh") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("event"), starts_with("lb")) %>% 
  select(-ends_with("cd"))


#########################################
# Make tdex for study treatment exposure
########################################


tdex <- raw %>% pick("da") %>% 
  full_join(pick(raw,"dr")) %>%   
  select(subjectid, starts_with("event"), starts_with("da"), starts_with("dr")) %>% 
  select(-ends_with("cd")) %>% 
  labeliser(codelist = items) 

write_rds(tdex, "data/td/tdex.rds")



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

###########################################
# Make respiratory condition  and vital sign tabulation datasets
#########################################


tdrc <- raw %>% 
  pick("rc") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventdate, eventid, starts_with("rc")) %>% 
  select( -ends_with("_copy1"))

tdvs <- raw %>% 
  pick("vs") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventdate, eventid, starts_with("vs")) %>% 
  mutate(vsobese = cut(vsbmi, breaks = c(-Inf, 30, Inf), label = c("Normal", "Obese"))) %>% 
  labelled::set_variable_labels(vsobese = "Obese (BMI > 30)?")

tdrc_ <- tdrc %>%
  full_join(tdvs %>% select(-(vsweight:vscons), -vsobese), by = c("subjectid", "eventid", "eventdate")) %>% 
  mutate(rcair = if_else(!is.na(rcair), rcair, vsair),
         rcair = if_else(eventid!="V30", rcair, factor("Room air", ordered = TRUE))
  ) %>% 
  mutate(rcoxytercd = if_else(rcair == "Room air", 0, rcoxytercd), 
         rcoxytercd = if_else(!is.na(rcoxytercd), rcoxytercd, vsoxytercd),
         rcoxytercd = if_else(!is.na(rcoxytercd) & vshighcd == 1, 6, rcoxytercd, rcoxytercd),
         rcoxytercd = if_else(is.na(rcoxytercd) & rchighcd == 1, 6, rcoxytercd, rcoxytercd),
         rcoxytercd = if_else(rcnivcd == 1, 4, rcoxytercd, rcoxytercd),
         rcoxytercd = if_else(rcimvcd == 1, 7, rcoxytercd, rcoxytercd),
         rcoxytercd = if_else(is.na(rcoxytercd) & rchighcd == 0 & rcnivcd == 0 & rcimvcd == 0, 
                              2, rcoxytercd, rcoxytercd)
         ) %>%
  mutate(rcoxsat = if_else(!is.na(rcoxsat), rcoxsat, vsoxsat),
         rcoxsat = if_else(!is.na(vsox3mnt), vsox3mnt, rcoxsat)) %>%
  # Calculate pf-ratio based on oxygen saturation on room air. 
  # Quality checked against original computed value.
  mutate(rcrtroom = if_else(rcair == "Room air",
                                 case_when(
                                   rcoxsat < 80 ~ 5 /0.21, 
                                   rcoxsat == 80 ~ 5.87 /0.21, 
                                   rcoxsat == 81 ~ 6.0 /0.21, 
                                   rcoxsat == 82 ~ 6.13 /0.21, 
                                   rcoxsat == 83 ~ 6.27 /0.21, 
                                   rcoxsat == 84 ~ 6.53 /0.21, 
                                   rcoxsat == 85 ~ 6.67 /0.21, 
                                   rcoxsat == 86 ~ 6.93 /0.21, 
                                   rcoxsat == 87 ~ 7.07 /0.21, 
                                   rcoxsat == 88 ~ 7.33 /0.21, 
                                   rcoxsat == 89 ~ 7.60 /0.21, 
                                   rcoxsat == 90 ~ 8.0 /0.21, 
                                   rcoxsat == 91 ~ 8.27 /0.21, 
                                   rcoxsat == 92 ~ 8.67 /0.21, 
                                   rcoxsat == 93 ~ 9.20 /0.21, 
                                   rcoxsat == 94 ~ 9.73 /0.21, 
                                   rcoxsat == 95 ~ 10.53 /0.21, 
                                   rcoxsat == 96 ~ 11.47 /0.21, 
                                   rcoxsat == 97 ~ 12.8 /0.21, 
                                   rcoxsat == 98 ~ 14.93 /0.21, 
                                   rcoxsat == 99 ~ 15.0 /0.21, 
                                   rcoxsat == 100 ~ 15.1 /0.21, 
                                   is.na(rcoxsat) ~ NA_real_,
                                   TRUE ~ NA_real_
                                 ), NA_real_, NA_real_),
         rcrtroom = round(rcrtroom, 1)) %>% 
  arrange(subjectid, eventdate) 

# 
# %>% 
#   filter(rcair == "Room air") %>% 
#   select(subjectid, eventdate, rcair,rcrtroom, rcrtroom_calc, diff, rcoxsat)


tdvs <- tdvs %>% select(-(vsox3mnt:vsprohrs)) %>% 
  select(-ends_with("cd")) 

write_rds(tdvs, "data/td/tdvs.rds")




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
                     vssys, vsdia, vsmap, vstemp), 
            by = "subjectid") %>% 
  # left_join(tdrc %>% 
  #             filter(eventid == "V00") %>% 
  #             select(subjectid, rclmin), 
  #           by = "subjectid") %>% 
  # mutate(lito2min = if_else(is.na(vslitmin),rclmin, vslitmin),
  #        lito2min_reg = if_else(is.na(vslitmin) & is.na(rclmin),"No", "Yes")) %>% 
  # labelled::set_variable_labels(lito2min = "Oxygen requirement (litres O2 per min)",
  #                               lito2min_reg = "Litres O2 per min registered?") %>%
  # select(-vslitmin, -rclmin) %>% 
  # Add baseline SOFA score
  left_join(tdsc %>% filter(eventid == "V00") %>% select(subjectid, scsumsc), by = "subjectid") %>% 
  labelled::set_variable_labels(scsumsc = "Baseline SOFA score") %>% 
  # Add co-morbidity and smoking
  left_join(tdcc %>%  select(-sitename, -sitecode), by = "subjectid") %>% 
  # Add epidemiological factors
  left_join(tdef %>%  select(-sitename, -sitecode), by = "subjectid")  %>% 
  # Add duration of disease etc
  left_join(tdoa, by = "subjectid") %>% 
  mutate(sympdur = oaadm_h - oasympdt) %>% 
  labelled::set_variable_labels(sympdur = "Symptom duration at admission (days") %>% 
  # Add baseline medications
  left_join(tdme %>%  select(-sitename, -sitecode), by = "subjectid") %>% 
  left_join(tdlbb %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, lbferres, lbdimres1, lbastres, lbaltres, lbldres, lbcrpres, lbprores), 
            by = "subjectid") %>% 
  left_join(tdlbh %>% 
              filter(eventid == "V00") %>% 
              select(subjectid, lbhbres, lbpcres, lbneures, lblymres, lbwbcres), 
            by = "subjectid") 
  
  
write_rds(tddm, "data/td/tddm.rds")


tdds <- raw %>% 
  pick("eos") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, starts_with("eos")) %>% 
  select( -(eosaechk:eosaeyes1cd), -eosyncd)


tdds <- tddm %>% 
  select(subjectid, dmicdat) %>% 
  left_join(tdoa, by = "subjectid") %>% 
  left_join(select(tdran, subjectid, randt), by="subjectid") %>% 
  left_join(tdds, by = "subjectid") %>% 
  left_join(tddph, by = "subjectid")

write_rds(tdds, "data/td/tdds.rds")

###################################################
# Make tdae
###################################################


tdae <- raw %>% pick("ae") %>% 
  left_join(pick(raw,"meddra")) %>% 
  select(-(eventid:designversion), -(siteseq:subjectseq)) %>% 
  labeliser()

write_rds(tdae, "data/td/tdae.rds")


