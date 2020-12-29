```{r}
rdab <- read_rds("results/rds/rdab.rds")

adab <- adsl %>% 
  left_join(tdab %>% select(subjectid, studyday, absumctr, abrbd, abace2rbd ), by = "subjectid") %>% 
  mutate(abnormrbd = abrbd/absumctr*100,
         log10abace2rbd = log10abace2rbd) %>% 
  filter(studyday >65) %>% 
  mutate(rantrt = factor(rantrt, ordered = FALSE))



```



### Descriptives

```{r}
tdab <- read_rds("data/td/tdab.rds")

adab <- adsl %>% 
  left_join(tdab %>% select(subjectid, studyday, absumctr, abrbd, abace2rbd ), by = "subjectid") %>% 
  mutate(abnormrbd = abrbd/absumctr*100,
         log10abace2rbd = log10abace2rbd) %>% 
  filter(studyday >65) %>% 
  mutate(rantrt = factor(rantrt, ordered = FALSE))





```


```{r antibodies, message = FALSE}
library(quantreg) 
library(broom)
tdab <- read_rds("data/td/tdab.rds")

adab <- adsl %>% 
  left_join(tdab %>% select(subjectid, studyday, absumctr, abrbd, abace2rbd ), by = "subjectid") %>% 
  mutate(abnormrbd = abrbd/absumctr*100) %>% 
  filter(studyday >65) %>% 
  mutate(rantrt = factor(rantrt, ordered = FALSE))

lreg_f <- function(var = "abrbd", population = "fas"){
  # var <- ensym(var)
  population <- ensym(population)
  
  f.str <- paste0(var, " ~ rantrt")
  data <- adab %>%
    filter(!!population == "Yes") %>%
    mutate(rantrt = fct_drop(rantrt))
  #res <- rlang::qqshow(quantreg::rq(!!var ~ rantrt, data = data))
  res <- lm(as.formula(f.str), data = data) %>%
    tidy(conf.int = TRUE)
  
  return(res)
}


mreg_f <- function(var = "abrbd", population = "fas"){
  # var <- ensym(var)
  population <- ensym(population)
  
  f.str <- paste0(var, " ~ rantrt")
  data <- adab %>%
    filter(!!population == "Yes") %>%
    mutate(rantrt = fct_drop(rantrt))
  #res <- rlang::qqshow(quantreg::rq(!!var ~ rantrt, data = data))
  res <- suppressWarnings(quantreg::rq(as.formula(f.str), data = data)) %>%
    tidy(se = "boot", conf.int = TRUE, R=20000)
  
  return(res)
}

rdab <- tibble(
  population = c("fas_hcq", "fas_rem")
) %>%
  crossing(var = c("abrbd", "abnormrbd", "abace2rbd")) %>%
  mutate(res_m = map2(var, population, mreg_f),
         res_l = map2(var, population, lreg_f)) 

rdab %>%
  select(-res_l) %>% 
  unnest(res_m) %>%  
  knitr::kable(digits = 3)

adab %>% ggplot(aes(abrbd)) +
  geom_histogram()

adab %>% ggplot(aes(abnormrbd)) +
  geom_histogram()

adab %>% ggplot(aes(log10(abace2rbd))) +
  geom_histogram()

tmp <- glm(abrbd ~ rantrt, data =  adab)
tmp
tmp_c <- sandwich::vcovHC(tmp)
modmarg::marg(tmp, var_interest = "rantrt", type = "levels")
modmarg::marg(tmp, var_interest = "rantrt", type = "levels", vcov_mat = tmp_c)

```


### HCQ results from Stata

```{r antibodies-hcq}


stata_qreg <-
  "
qreg abrbd i.rantrt
qreg abnormrbd i.rantrt
qreg abace2rbd i.rantrt

"

stata(
  src = stata_qreg,
  data.in = adab %>% filter(fas_hcq == "Yes"),
  data.out = FALSE,
  stata.path = "/usr/local/bin/stata-se",
  stata.version = 16,
  stata.echo = TRUE
)






```

### Remdesivir results by Stata

```{r antibodies-remdesivir}

stata(
  src = stata_qreg,
  data.in = adab %>% filter(fas_hcq == "Yes"),
  data.out = FALSE,
  stata.path = "/usr/local/bin/stata-se",
  stata.version = 16,
  stata.echo = TRUE
)


```


### Sensitivity results by ANOVA

```{r antibodies-sensitivity}


rdab %>%
  select(-res_m) %>% 
  unnest(res_l) %>%  
  knitr::kable(digits = 3)


```