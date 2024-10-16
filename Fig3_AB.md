---
title: "Fig_3AB"
output:
  html_document:
    keep_md: yes
---




```r
source("source.R")

path_file <-
  "data/dat_all.csv"
```

- parameters


```r
.ici_max <- 30
.ici_threshold <- 5
```

- import data


```r
dat_raw <-
  path_file %>% 
  fread() %>% 
  as_tibble()
```

- bigram classification


```r
dat_bigram <-
  dat_raw %>%
  group_by(Name, PW) %>% 
  mutate(
    post_calltype = lead(calltype),
    post_start = lead(start),
    ici = post_start - end
  ) %>% 
  ungroup() %>% 
  filter(ici <= .ici_max) %>% 
  na.omit()

dat_bigram_call <-
  dat_bigram %>% 
  mutate(
    ICI = if_else(ici < .ici_threshold, "Short", "Long"),
    Repeat = if_else(calltype == post_calltype, "Repeated", "Different"),
    bigram = str_c(ICI, " ICI ", Repeat, " call")
  ) 
```

- count


```r
dat_bigram_callN <-
  dat_bigram_call %>%
  na.omit() %>% 
  group_nest(Name, type, PM, PW, Stage, fm, parents, bigram) %>% 
  mutate(n = map_dbl(data, nrow)) %>%
  group_by(Name, PW, PM) %>% 
  mutate(n = n / sum(n)) %>% 
  ungroup()  %>% 
  select(!data) %>% 
  pivot_wider(
    values_from = n,
    values_fill = 0,
    names_from = bigram
  ) %>% 
  pivot_longer(
    cols = contains("call"),
    names_to = "bigram",
    values_to = "n"
  )
```

- data visualization


```r
dat_g <-
  dat_bigram_callN %>%
  mutate(ICI = if_else(str_detect(bigram, "Long"), "Long ICI", "Short ICI"),
         Repeat = if_else(str_detect(bigram, "Repeated"), "Repeated call", "Different call")) %>% 
  mutate(ICI = factor(ICI, levels = c("Short ICI", "Long ICI")),
         Repeat = factor(Repeat, levels = c("Repeated call", "Different call"))) 


gg_bigram <- function(data){
  data %>% 
    ggplot() +
    aes(PM, n, color = type) +
    geom_line(aes(group = Name), alpha = 0.2) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = F, size = 2, alpha = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.25), limits = c(0, 1)) +
    scale_color_manual(values = c(UE = "black", VPA = "red")) +
    facet_wrap(~ICI, nrow = 1)+
    theme(legend.position = "none",
          axis.title.x = element_text(hjust = 1))
}


g1 <-
  dat_g %>% 
  filter(Repeat == "Repeated call") %>% 
  gg_bigram()

g2 <-
  dat_g %>% 
  filter(Repeat == "Different call") %>% 
  gg_bigram()

g <-
  wrap_plots(
    g1 +
      labs(y = "Population ratio", title = "Repeated call"), 
    g2 +
      labs(y = "Population ratio", title = "Different call")
    
  )


ggsave("fig/Fig_003AB.png", width = 8, height = 4)
ggsave("fig/Fig_003AB.svg", width = 8, height = 4)
```

- statistics: ANOVA


```r
dat_bigram_callN %>% 
  group_by(bigram) %>% 
  rstatix::anova_test(
    n ~ type * PM
  )
```

```
## # A tibble: 12 × 8
##    bigram                   Effect    DFn   DFd     F     p `p<.05`      ges
##  * <chr>                    <chr>   <dbl> <dbl> <dbl> <dbl> <chr>      <dbl>
##  1 Long ICI Different call  type        1   104 0.276 0.6   ""      0.003   
##  2 Long ICI Different call  PM          1   104 7.77  0.006 "*"     0.07    
##  3 Long ICI Different call  type:PM     1   104 5.91  0.017 "*"     0.054   
##  4 Long ICI Repeated call   type        1   104 0.635 0.427 ""      0.006   
##  5 Long ICI Repeated call   PM          1   104 3.36  0.069 ""      0.031   
##  6 Long ICI Repeated call   type:PM     1   104 1.24  0.268 ""      0.012   
##  7 Short ICI Different call type        1   104 1.72  0.192 ""      0.016   
##  8 Short ICI Different call PM          1   104 5.65  0.019 "*"     0.051   
##  9 Short ICI Different call type:PM     1   104 0.079 0.779 ""      0.000763
## 10 Short ICI Repeated call  type        1   104 3.42  0.067 ""      0.032   
## 11 Short ICI Repeated call  PM          1   104 4.04  0.047 "*"     0.037   
## 12 Short ICI Repeated call  type:PM     1   104 6.72  0.011 "*"     0.061
```

```r
dat_bigram_callN %>% 
  group_by(bigram, type) %>% 
  rstatix::anova_test(
    n ~ PW
  )
```

```
## # A tibble: 8 × 9
##   type  bigram                 Effect   DFn   DFd      F       p `p<.05`     ges
## * <chr> <chr>                  <chr>  <dbl> <dbl>  <dbl>   <dbl> <chr>     <dbl>
## 1 UE    Long ICI Different ca… PW         1    43  9.75  3   e-3 "*"     0.185  
## 2 VPA   Long ICI Different ca… PW         1    61  0.401 5.29e-1 ""      0.007  
## 3 UE    Long ICI Repeated call PW         1    43  3.09  8.6 e-2 ""      0.067  
## 4 VPA   Long ICI Repeated call PW         1    61  0.597 4.43e-1 ""      0.01   
## 5 UE    Short ICI Different c… PW         1    43  2.57  1.16e-1 ""      0.056  
## 6 VPA   Short ICI Different c… PW         1    61  3.07  8.5 e-2 ""      0.048  
## 7 UE    Short ICI Repeated ca… PW         1    43 15.8   2.69e-4 "*"     0.268  
## 8 VPA   Short ICI Repeated ca… PW         1    61  0.02  8.89e-1 ""      0.00032
```


- model selection


```r
.list_model <-
  list(
    function(dat){ lm(n ~ 1, data = dat) }, #1
    function(dat){ lmer(n ~ 1 + (PW|fm), data = dat) }, #2
    function(dat){ lmer(n ~ 1 + (PW|Name), data = dat) }, #3
    function(dat){ lmer(n ~ 1 + (PW|parents), data = dat) }, #4
    function(dat){ lm(n ~ PW, data = dat) }, #5
    function(dat){ lmer(n ~ PW + (PW|fm), data = dat) }, #6
    function(dat){ lmer(n ~ PW + (PW|Name), data = dat) }, #7
    function(dat){ lmer(n ~ PW + (PW|parents), data = dat) } #8
  )


dat_fit <-
  dat_bigram_callN  %>%
  group_nest(bigram, type) %>% 
  mutate(model = list(.list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  group_by(type, bigram) %>% 
  mutate(modelid = modelid - min(modelid) + 1) %>% 
  ungroup() %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(type, bigram) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup()

dat_fit %>% 
  print(n = 100)
```

```
## # A tibble: 64 × 8
##    modelid bigram                   type      data model fit          AIC   dAIC
##      <dbl> <chr>                    <chr> <list<t> <lis> <list>     <dbl>  <dbl>
##  1       1 Long ICI Different call  UE    [45 × 7] <fn>  <lm>       -27.9  7.20 
##  2       2 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -21.2 13.8  
##  3       3 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -21.4 13.7  
##  4       4 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -22.3 12.8  
##  5       5 Long ICI Different call  UE    [45 × 7] <fn>  <lm>       -35.1  0    
##  6       6 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -14.2 20.8  
##  7       7 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -16.0 19.0  
##  8       8 Long ICI Different call  UE    [45 × 7] <fn>  <lmerMod>  -15.4 19.6  
##  9       1 Long ICI Different call  VPA   [63 × 7] <fn>  <lm>       -91.4  0    
## 10       2 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -79.2 12.2  
## 11       3 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -83.6  7.88 
## 12       4 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -87.9  3.53 
## 13       5 Long ICI Different call  VPA   [63 × 7] <fn>  <lm>       -89.8  1.59 
## 14       6 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -70.3 21.2  
## 15       7 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -72.0 19.5  
## 16       8 Long ICI Different call  VPA   [63 × 7] <fn>  <lmerMod>  -76.6 14.8  
## 17       1 Long ICI Repeated call   UE    [45 × 7] <fn>  <lm>       -50.8  1.12 
## 18       2 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -40.4 11.5  
## 19       3 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -45.5  6.44 
## 20       4 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -48.8  3.05 
## 21       5 Long ICI Repeated call   UE    [45 × 7] <fn>  <lm>       -51.9  0    
## 22       6 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -30.6 21.2  
## 23       7 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -36.2 15.7  
## 24       8 Long ICI Repeated call   UE    [45 × 7] <fn>  <lmerMod>  -39.6 12.3  
## 25       1 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lm>      -112.   1.16 
## 26       2 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod>  -98.8 14.0  
## 27       3 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod> -113.   0    
## 28       4 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod> -108.   4.75 
## 29       5 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lm>      -110.   2.55 
## 30       6 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod>  -87.4 25.4  
## 31       7 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod> -101.  11.9  
## 32       8 Long ICI Repeated call   VPA   [63 × 7] <fn>  <lmerMod>  -96.4 16.4  
## 33       1 Short ICI Different call UE    [45 × 7] <fn>  <lm>       -60.9  0.614
## 34       2 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -49.5 12.1  
## 35       3 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -57.4  4.16 
## 36       4 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -53.8  7.79 
## 37       5 Short ICI Different call UE    [45 × 7] <fn>  <lm>       -61.6  0    
## 38       6 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -39.5 22.0  
## 39       7 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -46.8 14.8  
## 40       8 Short ICI Different call UE    [45 × 7] <fn>  <lmerMod>  -44.2 17.3  
## 41       1 Short ICI Different call VPA   [63 × 7] <fn>  <lm>      -108.   1.10 
## 42       2 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod>  -95.9 12.9  
## 43       3 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod>  -99.4  9.34 
## 44       4 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod> -101.   7.76 
## 45       5 Short ICI Different call VPA   [63 × 7] <fn>  <lm>      -109.   0    
## 46       6 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod>  -87.3 21.5  
## 47       7 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod>  -89.4 19.4  
## 48       8 Short ICI Different call VPA   [63 × 7] <fn>  <lmerMod>  -91.2 17.6  
## 49       1 Short ICI Repeated call  UE    [45 × 7] <fn>  <lm>       -37.3 12.1  
## 50       2 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -38.9 10.4  
## 51       3 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -48.5  0.866
## 52       4 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -47.8  1.56 
## 53       5 Short ICI Repeated call  UE    [45 × 7] <fn>  <lm>       -49.4  0    
## 54       6 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -31.7 17.7  
## 55       7 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -42.9  6.45 
## 56       8 Short ICI Repeated call  UE    [45 × 7] <fn>  <lmerMod>  -41.0  8.43 
## 57       1 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lm>       -35.7 15.9  
## 58       2 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -24.0 27.7  
## 59       3 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -42.9  8.79 
## 60       4 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -51.7  0    
## 61       5 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lm>       -33.8 17.9  
## 62       6 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -13.0 38.7  
## 63       7 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -32.3 19.4  
## 64       8 Short ICI Repeated call  VPA   [63 × 7] <fn>  <lmerMod>  -41.3 10.4
```

```r
dat_minAIC <-
  dat_fit %>%
  group_by(bigram, type) %>% 
  filter(dAIC == 0)

dat_minAIC %>% 
  arrange(type)
```

```
## # A tibble: 8 × 8
## # Groups:   bigram, type [8]
##   modelid bigram                   type        data model fit          AIC  dAIC
##     <dbl> <chr>                    <chr> <list<tib> <lis> <list>     <dbl> <dbl>
## 1       5 Long ICI Different call  UE      [45 × 7] <fn>  <lm>       -35.1     0
## 2       5 Long ICI Repeated call   UE      [45 × 7] <fn>  <lm>       -51.9     0
## 3       5 Short ICI Different call UE      [45 × 7] <fn>  <lm>       -61.6     0
## 4       5 Short ICI Repeated call  UE      [45 × 7] <fn>  <lm>       -49.4     0
## 5       1 Long ICI Different call  VPA     [63 × 7] <fn>  <lm>       -91.4     0
## 6       3 Long ICI Repeated call   VPA     [63 × 7] <fn>  <lmerMod> -113.      0
## 7       5 Short ICI Different call VPA     [63 × 7] <fn>  <lm>      -109.      0
## 8       4 Short ICI Repeated call  VPA     [63 × 7] <fn>  <lmerMod>  -51.7     0
```
