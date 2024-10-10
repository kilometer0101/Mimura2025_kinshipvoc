
library(tidyverse)
library(data.table)
library(patchwork)
library(lme4)

theme_set(
  theme_classic() +
    theme(legend.title = element_blank(),
          strip.background = element_rect(color = NA))
)



sep_lowhigh <- function(dat, .key){
  
  dat_trill <-
    dat %>% 
    mutate(kHz = (top + bottom) / 2) %>% 
    filter(calltype == .key)
  
  mixmdl_trill <- normalmixEM(dat_trill$kHz, k = 2)
  
  dat_trill_p <-
    dat_trill %>% 
    bind_cols(mixmdl_trill$posterior) %>% 
    mutate(dist = if_else(comp.1 > comp.2, 
                          str_c("low-", .key), str_c("high-", .key)))
  
  dat_lowhigh <-
    dat_trill_p %>% 
    ungroup() %>% 
    select(callid, dist) %>% 
    left_join(dat, ., by = "callid") %>% 
    mutate(calltype = if_else(is.na(dist), calltype, dist)) %>% 
    select(!dist)
  
  return(dat_lowhigh)
}

lrt <- function (obj1, obj2) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}


list_model <-
  list(
    function(dat){ lm(n ~ 1, data = dat) }, #1
    function(dat){ lmer(n ~ 1 + (PW|type), data = dat) }, #2
    function(dat){ lmer(n ~ 1 + (PW|fm), data = dat) }, #3
    function(dat){ lmer(n ~ 1 + (PW|Name), data = dat) }, #4
    function(dat){ lmer(n ~ 1 + (PW|parents), data = dat) }, #5
    function(dat){ lm(n ~ type, data = dat) }, #6
    function(dat){ lmer(n ~ type + (type|fm), data = dat) }, #7
    function(dat){ lmer(n ~ type + (type|Name), data = dat) }, #8
    function(dat){ lmer(n ~ type + (type|parents), data = dat) }, #9
    function(dat){ lm(n ~ PW, data = dat) }, #10
    function(dat){ lmer(n ~ PW + (PW|type), data = dat) }, #11
    function(dat){ lmer(n ~ PW + (PW|fm), data = dat) }, #12
    function(dat){ lmer(n ~ PW + (PW|Name), data = dat) }, #13
    function(dat){ lmer(n ~ PW + (PW|parents), data = dat) }, #14
    function(dat){ lm(n ~ PW + type + PW:type, data = dat) }, #15
    function(dat){ lmer(n ~ PW + type + PW:type + (PW|fm), data = dat) }, #16
    function(dat){ lmer(n ~ PW + type + PW:type + (PW|Name), data = dat) }, #17
    function(dat){ lmer(n ~ PW + type + PW:type + (PW|parents), data = dat) }, #18
    function(dat){ lmer(n ~ PW + type + PW:type + (type|fm), data = dat) }, #19
    function(dat){ lmer(n ~ PW + type + PW:type + (type|Name), data = dat) }, #20
    function(dat){ lmer(n ~ PW + type + PW:type + (type|parents), data = dat) }# , #21
   )