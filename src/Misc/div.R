###############################3
## Fra Pål 25 Okt :De som skal gjøre noen avanserte analyser i Nederland ønsker bare 
## et ca tall på hvor mange som havnet på ICU + død. Dette er ikke i forhold til 
## statistikk etc men bare for å danne seg et bilde av populasjonen
################################


adsl %>% 
  select(subjectid, fas) %>% 
  left_join(tdds, by = "subjectid") %>% 
  filter(fas == "Yes") %>% 
  group_by(eosreascd) %>% 
  summarise(n=n())

tmp <- adsl %>% 
  select(subjectid, fas) %>% 
  left_join(tdsq, by = "subjectid") %>% 
  filter(fas == "Yes" & eventid != "V00") %>% 
  group_by(subjectid) %>% 
  summarise(max = max(sq_admis, na.rm = TRUE)) %>% 
  group_by(max) %>% 
  summarise(n = n())
tmp
  
##########################################
## Fra Dominique 24 Oct: 
## 14-day rate of progression for those in step 4 and 5 of the july WHO scale and
## 60-day mortality rate for those in sep 6 to 9 of the July WHO scale
##########################################

survtime <- tdds %>% 
  mutate(
    survtime = if_else(!is.na(eosdtdat), eosdtdat - randt, eosdat - randt),
    survtime = if_else(!is.na(survtime), survtime, dphendt - randt),
    survcens = if_else(is.na(eosdtdat), "Yes", "No"), 
    survtime_60 = if_else(survtime <= 60, survtime, 60),
    survcens_60 = if_else(survtime <= 60, survcens, "Yes"),
    survtime_60 = if_else(survcens == "No", survtime_60, 60)
  ) %>% 
  select(subjectid, survtime, survcens, survtime_60, survcens_60)

survtmp <-  adsl %>% 
  select(subjectid, fas, age_calc) %>% 
  left_join(tdrc, by = "subjectid") %>% 
  select(subjectid, fas, eventid, eventdate, rcwhocps, rcwhostate, age_calc) %>% 
  left_join(survtime, by = "subjectid") %>% 
  filter(fas == "Yes") %>% 
  group_by(subjectid) %>% 
  mutate(
    studyday = eventdate - eventdate[eventid == "V00"],
    rcwhostate_bl = rcwhostate[eventid == "V00"],
    rcwhostate_max14 = max(rcwhostate[eventid != "V00" & studyday < 14], na.rm = TRUE),
    progression = case_when(
      survcens == "No" ~ "Yes",
      rcwhostate_bl == "Moderate" & rcwhostate_max14 == "Severe" ~ "Yes",
      TRUE ~ "No"
      ) 
    ) %>% 
  mutate(prog_time = if_else(rcwhostate_bl == "Moderate" & rcwhostate == "Severe", studyday, Inf),
         prog_time =  min(prog_time, na.rm = TRUE),
         prog_time = if_else(survcens == "No", min(survtime, prog_time), prog_time), 
         prog_14 = case_when(
           progression == "Yes" & prog_time <= 14 ~ "Yes",
           survcens == "No" & progression == "No" & survtime <= 14 ~ "Yes",
           TRUE ~ "No"
         ),
         prog_time14 = if_else(prog_time <= 14, prog_time, 14)
  ) %>% 
  filter(eventid == "V00") %>% 
  mutate(early = eventdate < dmy("01-07-2020"),
         young = age_calc < 60)


library(survival)
library(survminer)

fit <- survfit(Surv(survtime_60, survcens_60 == "No") ~ early, data = survtmp)

ggsurvplot(fit, data = survtmp, risk.table = TRUE)

fit2 <- survfit(Surv(prog_time14, prog_14 == "Yes") ~ early , data = survtmp %>% filter(rcwhostate_bl=="Moderate"))

ggsurvplot(fit2, data = survtmp, risk.table = TRUE)

#############################
# Test av metodologien til Peto et al
#############################

test <- survdiff(Surv(survtime, survcens == "No") ~ early , data = survtmp)
test <- survdiff(Surv(survtime_60, survcens_60 == "No") ~ strata(young) + rcwhostate_bl, data = survtmp)

test
(test$obs[1]-test$exp[1])^2/test$var[1][1]

RR <- (test$obs[1]-test$exp[1])/test$var[1][1]
RR

addm %>% mutate(age_cat = cut(age_calc, breaks = c(-Inf, 50, 60, 70, 80, Inf))) %>% group_by(age_cat) %>% summarise(n=n())

############################
# Andreas vil ha oversikt over de som har vært innlagt på ICU
###########################


 tmp <- adsl %>% 
  select(subjectid, fas) %>% 
  left_join(tdsq, by = "subjectid") %>% 
  filter(fas == "Yes" & eventid != "V00") %>% 
  group_by(subjectid) %>% 
  summarise(max = max(sq_admis, na.rm = TRUE)) %>% 
  filter(max == "ICU") 

readr::write_excel_csv2(tmp, "data/work/icu.csv")  

##############################
# E-post 1 Dec 2020: Inge

# Det er noen prøver som skal sendes til en annen lab hvor det er viktig at vi vet hva som er gitt av 
# behandling, alder, kjønn og om pasienten har blitt a) innlagt intensiv under oppholdet 
# b) ble direkte innlagt intensiv. Prøvene sendes ut denne uken og det haster å få på plass en endelig oversikt.

# Jeg foreslår at du sender oss en oversikt over samtlige VIEDOC id, med ledsagende informasjon slik at 
# vi kan plukke aktuelle prøver etter den listen. Tuva som er ansatt i NOR-SOLIDARITY vil være ansvarlig 
# for dette oppsettet og fint om alle på listen får en kopi. 

###########################

adsl <- readr::read_rds("data/ad/adsl.rds")
addm <- readr::read_rds("data/ad/addm.rds")

tmp <- adsl %>% 
  select(subjectid, fas) %>% 
  left_join(tdsq, by = "subjectid") %>% 
  filter(fas == "Yes" & eventid != "V00") %>% 
  group_by(subjectid) %>% 
  summarise(max = max(sq_admis, na.rm = TRUE))

tmp2 <- addm %>% 
  select(subjectid, fas, rantrt, age_calc, sex, sq_admis) %>% 
  left_join(tmp, by = "subjectid")


readr::write_excel_csv2(tmp2, "data/work/external_lab_info.csv") 



