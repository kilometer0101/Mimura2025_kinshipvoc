---
title: "Fig_2B"
output: 
  html_document:
    keep_md: TRUE
---




```r
source("source.R")

path_file <-
  "data/dat_all.csv"
```

- import data


```r
dat_raw <-
  path_file %>% 
  fread() %>% 
  as_tibble() 
```

- call N


```r
dat_allN <-
  dat_raw %>% 
  group_nest(Name, type, PW, PM, fm, parents) %>% 
  mutate(n = map_dbl(data, nrow))
```

- visualization


```r
dat_allN %>% 
  ggplot() +
  aes(PM, n, color = type) +
  geom_line(aes(group = Name), alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  labs(y = "Calls/30 min")
```

![](Fig_2B_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



- stat: ANOVA


```r
dat_allN %>% 
  rstatix::anova_test(
    n ~ PM * type
  )
```

```
## ANOVA Table (type II tests)
## 
##    Effect DFn DFd     F     p p<.05   ges
## 1      PM   1 105 4.550 0.035     * 0.042
## 2    type   1 105 0.164 0.686       0.002
## 3 PM:type   1 105 5.419 0.022     * 0.049
```

```r
dat_allN %>% 
  group_by(type) %>% 
  rstatix::anova_test(
    n ~ PM * Name
  )
```

```
## # A tibble: 6 × 8
##   type  Effect    DFn   DFd     F     p `p<.05`       ges
## * <chr> <chr>   <dbl> <dbl> <dbl> <dbl> <chr>       <dbl>
## 1 UE    PM          1    32 5     0.032 "*"     0.135    
## 2 UE    Name        6    32 4.54  0.002 "*"     0.46     
## 3 UE    PM:Name     6    32 2.14  0.075 ""      0.287    
## 4 VPA   PM          1    45 0.003 0.954 ""      0.0000763
## 5 VPA   Name        8    45 2.77  0.014 "*"     0.33     
## 6 VPA   PM:Name     8    45 1.98  0.072 ""      0.26
```

- models for LMM


```r
.list_model <-
  list(
    function(dat){ lm(n ~ 1, data = dat) }, #1
    function(dat){ lmer(n ~ 1 + (PM|fm), data = dat) }, #2
    function(dat){ lmer(n ~ 1 + (PM|Name), data = dat) }, #3
    function(dat){ lmer(n ~ 1 + (PM|parents), data = dat) }, #4
    function(dat){ lm(n ~ PM, data = dat) }, #5
    function(dat){ lmer(n ~ PM + (PM|fm), data = dat) }, #6
    function(dat){ lmer(n ~ PM + (PM|Name), data = dat) }, #7
    function(dat){ lmer(n ~ PM + (PM|parents), data = dat) } #8
  )
```

- model selection


```r
dat_fit <-
  dat_allN  %>%
  group_nest(type) %>% 
  mutate(model = list(.list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  group_by(type) %>% 
  mutate(modelid = modelid - min(modelid) + 1) %>% 
  ungroup() %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(type) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup()

dat_fit %>% 
  select(modelid, type, dAIC) %>% 
  pivot_wider(
    values_from = dAIC,
    names_from = type
  ) %>% 
  print(n = 100)
```

```
## # A tibble: 8 × 3
##   modelid    UE   VPA
##     <dbl> <dbl> <dbl>
## 1       1  31.9 26.2 
## 2       2  24.6 23.6 
## 3       3  10.7 10.0 
## 4       4  20.5  7.30
## 5       5  26.8 28.2 
## 6       6  14.5 17.3 
## 7       7   0    3.41
## 8       8  10.3  0
```

```r
dat_minAIC <-
  dat_fit %>% 
  filter(dAIC == 0) %>% 
  mutate(fixef = map(fit, fixef)) %>% 
  mutate(fixef = map(fixef, \(x){
    t(x) %>% 
      data.frame() %>% 
      set_names(c("a0", "a1"))
  })) %>% 
  unnest(fixef)

dat_minAIC
```

```
## # A tibble: 2 × 9
##   modelid type                data model  fit         AIC  dAIC    a0      a1
##     <dbl> <chr> <list<tibble[,7]>> <list> <list>    <dbl> <dbl> <dbl>   <dbl>
## 1       7 UE              [46 × 7] <fn>   <lmerMod>  635.     0  534. -81.3  
## 2       8 VPA             [63 × 7] <fn>   <lmerMod>  838.     0  254.  -0.186
```

- regression


```r
dat_pred <-
  dat_minAIC %>% 
  unnest(data) %>% 
  mutate(pred = a0 + PM * a1)
```

- visualization


```r
dat_pred %>% 
  ggplot() +
  aes(PM, n, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_line(aes(y = pred),
            linewidth = 2) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  labs(y = "Calls/30 min")
```

![](Fig_2B_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
