##################
# Make baseline dataset for the ECCO paper
#################
# 
# Gender
# Alder
# BMI
# Hjerterytme
# Sinusrytme (ja/nei)
# Røyker
# Systolisk blodtrykk
# Diastolisk blodtrykk
# Tidligere sykdommer/risikofaktorer
# Hypertensjon
# Diabetes
# koronarsykdom (Vi har chronich heart disease og man kan ta ut data på dette)
# Hjertesvikt (kardiologer har innhentet selv)
# Hyperlipedimi
# Medikamenter
# betablokker
# ACE-hemmer/ATII-blokkere
# Diuretika
# Antidiabetika
# Platehemmer
# Statiner
# Nitrater



eccosubj <- readxl::read_excel("data/raw/misc/20201120 Viedoc_ECCO_id.xlsx") %>% 
  filter(!is.na(eccoid)) %>% 
  select(subjectid)

eccobl <- eccosubj %>% 
  left_join(addm, by = "subjectid") %>% 
  select(subjectid, sex, age_calc, vsbmi, cc_eversmoker, vssys, vsdia,
         cc_ht, cc_diab, cc_card, me1, me2, diuretics = C03, antidiabetics = 
           A10, statins = C10, betablockers = C07, C01, C08)

haven::write_dta(eccobl, "data/work/eccobl.dta")  

