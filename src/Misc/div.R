adsl %>% 
  filter(fas == "Yes") %>% 
  group_by(rantrt, ranavail_rem) %>% 
  summarise(n=n())



tmp <- raw %>% pick("who") 


table(tdran$whoarms2, tdran$rantrt)
table(tdran$rantrt)
table(tdran$rantrt123)
