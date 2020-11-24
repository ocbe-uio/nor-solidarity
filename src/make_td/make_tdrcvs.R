
###########################################
# Make respiratory condition  and vital sign tabulation datasets
# Input from raw: rc, vs
# Output: tdrc, tdvs
#########################################

library(tidyverse)

source("src/external/functions.R")
raw <- readr::read_rds("data/raw/raw.rds")
items <- raw %>% pick("items")

tdrc <- raw %>% 
  pick("rc") %>% 
  labeliser(codelist = items) %>% 
  select(subjectid, eventdate, eventid, starts_with("rc")) %>% 
  select( -ends_with("_copy1")) %>% 
  arrange(subjectid, eventdate)

tdvs <- raw %>%
  pick("vs") %>%
  labeliser(codelist = items) %>%
  select(subjectid, eventdate, eventid, starts_with("vs")) %>%
  mutate(vsobese = cut(
    vsbmi,
    breaks = c(-Inf, 30, Inf),
    label = c("Normal", "Obese")
  )) %>%
  labelled::set_variable_labels(vsobese = "Obese (BMI > 30)?")


tdrc <- tdrc %>%
  full_join(tdvs %>% select(-(vsweight:vscons), -vsobese),
            by = c("subjectid", "eventid", "eventdate")) %>%
  mutate(
    rcair = if_else(!is.na(rcair), rcair, vsair),
    rcair = if_else(eventid != "V30", 
                    rcair, factor("Room air", ordered = TRUE))
  ) %>%
  mutate(
    rcoxytercd = if_else(rcair == "Room air",
                         0, rcoxytercd),
    rcoxytercd = if_else(!is.na(rcoxytercd),
                         rcoxytercd, vsoxytercd),
    rcoxytercd = if_else(!is.na(rcoxytercd) & vshighcd == 1,
                         6, rcoxytercd, rcoxytercd),
    rcoxytercd = if_else(is.na(rcoxytercd) & rchighcd == 1,
                         6, rcoxytercd, rcoxytercd),
    rcoxytercd = if_else(rcnivcd == 1,
                         4, rcoxytercd, rcoxytercd),
    rcoxytercd = if_else(rcimvcd == 1,
                         7, rcoxytercd, rcoxytercd),
    rcoxytercd = if_else(
      is.na(rcoxytercd) &
        rchighcd == 0 &
        rcnivcd == 0 &
        rcimvcd == 0,
      2,
      rcoxytercd,
      rcoxytercd
    )
  ) %>%
  mutate(rcoxyter = case_when(
    rcoxytercd == 0 ~ "Room air",
    rcoxytercd == 1 ~ "Face mask",
    rcoxytercd == 2 ~ "Nasal cannulas",
    rcoxytercd == 3 ~ "Face mask with reservoir" ,
    rcoxytercd == 4 ~ "Non-Invasive mechanical Ventilation (NIV)",
    rcoxytercd == 5 ~ "CPAP" ,
    rcoxytercd == 6 ~ "High-flow nasal cannula",
    rcoxytercd == 7 ~ "Invasive mechanical Ventilation",
    TRUE ~ "Unknown"
  ),
  rcoxyter = factor(rcoxyter, ordered = TRUE),
  rcair = as.character(rcair),
  rcair = if_else(!is.na(rcair) ,rcair, 
                  if_else(rcoxytercd > 0, "Oxygen therapy", rcair, rcair))) %>% 
  mutate(rcoxsat = if_else(!is.na(rcoxsat), rcoxsat, vsoxsat),
         rcoxsat = if_else(!is.na(vsox3mnt), vsox3mnt, rcoxsat)) %>%
  # Calculate pf-ratio based on oxygen saturation on room air. 
  # Quality checked against original computed value.
  mutate(
    rcpo2 = case_when(
      !is.na(rcpo2) ~ rcpo2,
      rcoxsat < 80  ~ 5 ,
      rcoxsat == 80 ~ 5.87 ,
      rcoxsat == 81 ~ 6.0 ,
      rcoxsat == 82 ~ 6.13 ,
      rcoxsat == 83 ~ 6.27 ,
      rcoxsat == 84 ~ 6.53 ,
      rcoxsat == 85 ~ 6.67 ,
      rcoxsat == 86 ~ 6.93 ,
      rcoxsat == 87 ~ 7.07 ,
      rcoxsat == 88 ~ 7.33 ,
      rcoxsat == 89 ~ 7.60 ,
      rcoxsat == 90 ~ 8.0 ,
      rcoxsat == 91 ~ 8.27 ,
      rcoxsat == 92 ~ 8.67 ,
      rcoxsat == 93 ~ 9.20 ,
      rcoxsat == 94 ~ 9.73 ,
      rcoxsat == 95 ~ 10.53 ,
      rcoxsat == 96 ~ 11.47 ,
      rcoxsat == 97 ~ 12.8 ,
      rcoxsat == 98 ~ 14.93 ,
      rcoxsat == 99 ~ 15.0 ,
      rcoxsat == 100 ~ 15.1 ,
      is.na(rcoxsat) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    rcpo2 = round(rcpo2, 1)
  ) %>% 
  mutate(rclmin = if_else(!is.na(rclmin), rclmin, vslitmin)) %>% 
  # CEstimated FiO2 based on hov many litres the patient gets and delivery method
  # Quality checked against eCRF-computed values
  mutate(rcfio2 = case_when(
    !is.na(rcfio2)                ~ rcfio2,
    rcoxytercd == 0               ~ 21, 
    
    rcoxytercd == 2  & rclmin <= 6 ~ 20 + rclmin*4, 
    rcoxytercd == 2  & rclmin > 6 ~ 44, 
    
    rcoxytercd == 1 & rclmin <= 5 ~ 20 + rclmin*4, 
    rcoxytercd == 1 & rclmin <= 6 ~ 40 + (rclmin-5)*10, 
    rcoxytercd == 1 & rclmin <= 8 ~ 50 + (rclmin - 6)*5, 
    rcoxytercd == 1 & rclmin > 8 ~ 60, 
    
    rcoxytercd == 3  & rclmin <= 8 ~ 40 + (rclmin - 4)*10, 
    rcoxytercd == 3 & rclmin > 8 ~ 85, 
    
    is.na(rcoxytercd)              ~ NA_real_,
    TRUE                           ~ NA_real_
  ),
  # some have entered the FiO2 as fraction not percentage
  rcfio2 =  if_else(rcfio2 <= 1.0, rcfio2*100, rcfio2, rcfio2),
  # two patients have entered FiO to zero, set to 21
  rcfio2 =  if_else(rcfio2 < 0.2, 21, rcfio2, rcfio2)
  ) %>% 
  mutate(rcratio = round(rcpo2 / rcfio2 * 100, 1) ) %>% 
  mutate(rcprone = if_else(!is.na(rcprone), rcprone, vsprone), 
         rcprohrs = if_else(!is.na(rcprohrs), rcprohrs, vsprohrs)) %>% 
  arrange(subjectid, eventdate) %>% 
  labeliser() %>% 
  rename(rcoxytercd_ = rcoxytercd) %>% 
  select( -ends_with("cd"), -starts_with("vs"), -rcrtroom, -(rcesfio2:rcrtoxy), -(rchigh:rcniv)) %>% 
  rename(rcoxytercd = rcoxytercd_) %>% 
  relocate(rcoxytercd, .after = rcoxytercd)

#######################
# Compute the WHO progression score (0-10) and corresponding disease state
######################

tdrc <-tdrc %>% 
  mutate(rcwhocps = case_when(
    rcoxytercd == 0           ~ 4,
    rcoxytercd %in% c(1, 2, 3) ~ 5,
    rcoxytercd %in% c(4, 5, 6) ~ 6,
    rcoxytercd == 7 & 
    
  )
  )

write_rds(tdrc, "data/td/tdrc.rds")


tdvs <- tdvs %>% select(-(vsox3mnt:vsprohrs)) %>% 
  select(-ends_with("cd")) 

write_rds(tdvs, "data/td/tdvs.rds")

