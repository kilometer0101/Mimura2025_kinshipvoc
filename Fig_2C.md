---
title: "Fig_2C"
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
dat_N <-
  dat_raw %>% 
  group_nest(Name, calltype, type, PW, PM, fm, parents) %>% 
  mutate(n = map_dbl(data, nrow))
```

- visualization


```r
dat_N %>% 
  ggplot() +
  aes(PM, n, color = type) +
  geom_line(aes(group = Name), alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  facet_wrap(~calltype, scales = "free_y", nrow = 2) +  
  labs(y = "Calls/30 min")
```

![](Fig_2C_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
.calls <-  c("phee", "trill", "ock")

dat_N %>% 
  filter(calltype %in% .calls) %>% 
  mutate(calltype = factor(calltype, levels = .calls)) %>% 
  ggplot() +
  aes(PM, n, color = type) +
  geom_line(aes(group = Name), alpha = 0.2) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  facet_wrap(~calltype, scales = "free_y", nrow = 1) +  
  labs(y = "Calls/30 min")
```

![](Fig_2C_files/figure-html/unnamed-chunk-5-1.png)<!-- -->




- two-way ANOVA


```r
dat_N %>% 
  group_by(calltype) %>% 
  rstatix::anova_test(
    n ~ PM * type
  )%>% 
  data.frame() %>% 
  filter(Effect == "type")
```

```
##     calltype Effect DFn DFd     F     p p..05   ges
## 1      chirp   type   1 101 0.392 0.533       0.004
## 2        egg   type   1  63 0.071 0.790       0.001
## 3        ock   type   1  98 5.709 0.019     * 0.055
## 4      other   type   1  25 1.898 0.181       0.071
## 5       peep   type   1 100 3.386 0.069       0.033
## 6       phee   type   1  68 7.504 0.008     * 0.099
## 7      trill   type   1 102 5.556 0.020     * 0.052
## 8  trillphee   type   1  61 0.325 0.571       0.005
## 9       tsik   type   1  42 1.355 0.251       0.031
## 10   twitter   type   1  15 0.092 0.766       0.006
```

- model selection


```r
dat_fit <-
  dat_N  %>%
  group_nest(calltype) %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  group_by(calltype) %>% 
  mutate(modelid = modelid - min(modelid) + 1) %>% 
  ungroup() %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(calltype) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup()

dat_fit %>% 
  select(modelid, calltype, dAIC) %>% 
  pivot_wider(
    values_from = dAIC,
    names_from = calltype
  ) %>% 
  print(n = 100)
```

```
## # A tibble: 21 × 11
##    modelid chirp    egg     ock other  peep  phee trill trillphee  tsik twitter
##      <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl> <dbl>   <dbl>
##  1       1 28.6  1.56   15.6    10.3  74.2  23.3  59.4      55.3  13.7    0.719
##  2       2 20.5  3.65   16.2    10.7  70.9  22.3  46.5      48.5  13.3    6.41 
##  3       3 23.4  3.65   16.7    10.9  73.2  25.3  53.1      50.0  13.7    6.40 
##  4       4 11.9  3.43    4.21    9.65 27.7  10.7  20.3      14.6  10.9    0    
##  5       5 16.1  3.35    3.03   10.6  12.7  17.1  39.5      17.2   9.73   4.83 
##  6       6 29.8  3.46   12.2    10.4  72.6  19.9  55.6      56.6  14.1    2.34 
##  7       7 26.2  0.189  11.2     4.32 63.2  18.8  45.2      53.8   6.74   4.75 
##  8       8 21.1  0.0447  3.03    1.82 36.6   8.78 17.8      21.8   4.78   4.69 
##  9       9 25.3  0       0.699   4.12 38.5   6.83 39.3      22.9   4.86   4.75 
## 10      10 19.0  3.34   16.0    12.2  75.3  23.7  52.7      45.1  14.7    1.62 
## 11      11 18.1  4.98   17.6    10.5  68.5  22.9  41.2      46.1  12.1   10.2  
## 12      12 20.5  4.98   18.8    10.8  70.7  26.7  47.4      46.9  12.3   10.2  
## 13      13  6.40 4.80    4.85    9.52 25.0  11.1  13.9      13.3   9.89   3.97 
## 14      14 11.5  4.63    3.72   10.6   8.60 17.5  33.5      16.6   8.09   8.78 
## 15      15 17.8  7.18   13.8    14.1  74.4  19.8  47.0      45.4  16.7    3.51 
## 16      16 11.6  1.40   13.6     2.42 56.4  18.7  26.1      41.3   3.17   8.74 
## 17      17  0    1.14    0.0876  1.68 13.7   6.44  0         9.01  1.35   2.40 
## 18      18  4.98 1.03    0       2.42  0     0    22.8       0     0      7.12 
## 19      19 11.2  1.40   13.0     2.42 56.4  18.7  26.6      41.3   3.17   8.74 
## 20      20  8.65 1.32    4.31    0    29.8   8.91  4.58     18.0   1.65   8.61 
## 21      21 11.5  1.27    1.76    2.33 30.3   7.46 21.9      14.8   1.56   8.74
```

```r
dat_minAIC <-
  dat_fit %>% 
  filter(dAIC == 0)

dat_minAIC
```

```
## # A tibble: 10 × 7
##    modelid calltype                data model  fit          AIC  dAIC
##      <dbl> <chr>     <list<tibble[,8]>> <list> <list>     <dbl> <dbl>
##  1      17 chirp              [105 × 8] <fn>   <lmerMod>  995.      0
##  2       9 egg                 [67 × 8] <fn>   <lmerMod>  612.      0
##  3      18 ock                [102 × 8] <fn>   <lmerMod>  795.      0
##  4      20 other               [29 × 8] <fn>   <lmerMod>  276.      0
##  5      18 peep               [104 × 8] <fn>   <lmerMod> 1246.      0
##  6      18 phee                [72 × 8] <fn>   <lmerMod>  562.      0
##  7      17 trill              [106 × 8] <fn>   <lmerMod> 1332.      0
##  8      18 trillphee           [65 × 8] <fn>   <lmerMod>  526.      0
##  9      18 tsik                [46 × 8] <fn>   <lmerMod>  489.      0
## 10       4 twitter             [18 × 8] <fn>   <lmerMod>   78.4     0
```
