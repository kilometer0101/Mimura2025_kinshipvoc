
library(tidyverse)
library(data.table)
library(patchwork)
library(lme4)

dir.create("data", showWarnings = FALSE)
dir.create("fig", showWarnings = FALSE)

theme_set(
  theme_classic() +
    theme(legend.title = element_blank(),
          strip.background = element_rect(color = NA))
)

.calls <-
  c("ock", "tsik", "egg", "phee", "peep", "trill", "trillphee", "chirp",
    "twitter",   "other")

arrange_ngram <- function(data, .bin = 30){
  data %>% 
    group_by(type, Name, Stage, Age, PW, PM) %>% 
    mutate(p1_calltype = lead(calltype),
           p1_start = lead(start),
           p1_end = lead(end),
           p1_ici = p1_start - end)  %>% 
    mutate(p2_calltype = lead(p1_calltype),
           p2_start = lead(p1_start),
           p2_end = lead(p1_end),
           p2_ici = p2_start - p1_end)   %>% 
    mutate(p3_calltype = lead(p2_calltype),
           p3_start = lead(p2_start),
           p3_end = lead(p2_end),
           p3_ici = p3_start - p2_end)   %>% 
    mutate(p4_calltype = lead(p3_calltype),
           p4_start = lead(p3_start),
           p4_end = lead(p3_end),
           p4_ici = p4_start - p3_end) %>% 
    mutate(p5_calltype = lead(p4_calltype),
           p5_start = lead(p4_start),
           p5_end = lead(p4_end),
           p5_ici = p5_start - p4_end) %>%   
    group_nest() %>% 
    mutate(gram1 = map(data, \(x){
      x %>% 
        group_nest(calltype) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data) %>% 
        rename(key = calltype)
    })) %>% 
    mutate(gram2 = map(data, \(x){
      x %>% 
        filter(p1_ici <= .bin) %>% 
        mutate(key = str_c(p1_calltype, "-", calltype)) %>% 
        group_nest(key) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data)
    })) %>% 
    mutate(gram3 = map(data, \(x){
      x %>% 
        filter(p1_ici <= .bin) %>% 
        filter(p2_ici <= .bin) %>% 
        mutate(key = str_c(p2_calltype, "-", p1_calltype, "-", calltype)) %>% 
        group_nest(key) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data)
    })) %>% 
    mutate(gram4 = map(data, \(x){
      x %>% 
        filter(p1_ici <= .bin) %>% 
        filter(p2_ici <= .bin) %>% 
        filter(p3_ici <= .bin) %>% 
        mutate(key = str_c(p3_calltype, "-", p2_calltype, "-", p1_calltype, "-", calltype)) %>% 
        group_nest(key) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data)
    })) %>% 
    mutate(gram5 = map(data, \(x){
      x %>% 
        filter(p1_ici <= .bin) %>% 
        filter(p2_ici <= .bin) %>% 
        filter(p3_ici <= .bin) %>% 
        filter(p4_ici <= .bin) %>% 
        mutate(key = str_c(p4_calltype, "-", p3_calltype, "-", p2_calltype, "-", p1_calltype, "-", calltype)) %>% 
        group_nest(key) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data)
    })) %>% 
    mutate(gram6 = map(data, \(x){
      x %>% 
        filter(p1_ici <= .bin) %>% 
        filter(p2_ici <= .bin) %>% 
        filter(p3_ici <= .bin) %>% 
        filter(p4_ici <= .bin) %>% 
        filter(p5_ici <= .bin) %>% 
        mutate(key = str_c(p5_calltype, "-",p4_calltype, "-", p3_calltype, "-", p2_calltype, "-", p1_calltype, "-", calltype)) %>% 
        group_nest(key) %>% 
        mutate(n = map_dbl(data, nrow)) %>% 
        select(!data)
    }))
}



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

lrt <- function (fit1, fit2) {
  L0 <- logLik(fit1)
  L1 <- logLik(fit2)
  DevRes <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = DevRes, df = df,
       "p-value" = pchisq(DevRes, df, lower.tail = FALSE))
}


list_model <-
  list(
    function(dat){ lm(n ~ 1, data = dat) }, #1
    function(dat){ lmer(n ~ 1 + (PM|type), data = dat) }, #2
    function(dat){ lmer(n ~ 1 + (PM|fm), data = dat) }, #3
    function(dat){ lmer(n ~ 1 + (PM|Name), data = dat) }, #4
    function(dat){ lmer(n ~ 1 + (PM|parents), data = dat) }, #5
    function(dat){ lm(n ~ type, data = dat) }, #6
    function(dat){ lmer(n ~ type + (type|fm), data = dat) }, #7
    function(dat){ lmer(n ~ type + (type|Name), data = dat) }, #8
    function(dat){ lmer(n ~ type + (type|parents), data = dat) }, #9
    function(dat){ lm(n ~ PM, data = dat) }, #10
    function(dat){ lmer(n ~ PM + (PM|type), data = dat) }, #11
    function(dat){ lmer(n ~ PM + (PM|fm), data = dat) }, #12
    function(dat){ lmer(n ~ PM + (PM|Name), data = dat) }, #13
    function(dat){ lmer(n ~ PM + (PM|parents), data = dat) }, #14
    function(dat){ lm(n ~ PM + type + PM:type, data = dat) }, #15
    function(dat){ lmer(n ~ PM + type + PM:type + (PM|fm), data = dat) }, #16
    function(dat){ lmer(n ~ PM + type + PM:type + (PM|Name), data = dat) }, #17
    function(dat){ lmer(n ~ PM + type + PM:type + (PM|parents), data = dat) }, #18
    function(dat){ lmer(n ~ PM + type + PM:type + (type|fm), data = dat) }, #19
    function(dat){ lmer(n ~ PM + type + PM:type + (type|Name), data = dat) }, #20
    function(dat){ lmer(n ~ PM + type + PM:type + (type|parents), data = dat) }# , #21
   )



gg_weight <- function(data){
  ggplot(data) +
    aes(PM, bw, color = type) +
    geom_vline(xintercept = -0.5, linetype = "dotted") +
    geom_point(alpha = 0.5, size = 0.2) +
    geom_line(aes(group = key), alpha = 0.2) +
    stat_smooth(method = "loess", span = 0.2, se = F) +
    scale_color_manual(values = c(UE = "black", VPA = "red")) +
    scale_x_continuous(breaks = -3:7) +
    scale_y_continuous(limits = c(0.8, 1.2)) +
    facet_wrap(~mf, ncol = 1, scales = "free_x") +
    theme(axis.title.x = element_text(hjust = 1)) +
    labs(y = "Scaled body weight")
}

gg_maha_bw <- function(data){
  data%>% 
    ggplot() +
    aes(bw, y, color = type) +
    geom_point(aes(shape = p_Name), alpha = 0.4) +
    scale_color_manual(values = c(UE = "black", VPA = "red")) +
    scale_x_continuous(limits = c(0.77, 1.2)) +
    facet_wrap(~mf, ncol = 1, scales = "free_x") +
    labs(x = "Scaled body weight",
         y = "log10(Mahalanobis D)")
}
