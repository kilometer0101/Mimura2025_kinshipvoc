
source("source.R")

path <- "data/dat_bigram_ici.csv"

dat_raw <- 
  path %>% 
  read_csv()


dat_bigram <-
  dat_raw %>% 
  unite(bigram, c(calltype, post_calltype), sep = "-")

dat_bigram_n <-
  dat_bigram %>% 
  select(!ici) %>% 
  group_nest(Name, type, fm, parents, PM, Repeat, ICI, bigram) %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  select(!data) %>% 
  group_by(Name, type, fm, parents, PM, Repeat, ICI) %>% 
  mutate(r = n / sum(n)) %>% 
  ungroup()

dat_bigram_n %>% 
  group_by(type, Repeat, ICI) %>% 
  summarise(n = sum(n)) %>% 
  pivot_wider(values_from = n,
              names_from = ICI) %>% 
  arrange(Repeat, type) %>% 
  mutate(r = Short / (Long + Short))

dat_bigram_n_sum <-
  dat_bigram %>% 
  select(!ici) %>% 
  group_nest(Name, type, fm, parents, PM, Repeat, ICI, bigram) %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  group_by(Name, type, fm, parents, PM, Repeat, ICI) %>% 
  summarise(n = sum(n))

dat_bigram_n_sum %>% 
  mutate(ICI = factor(ICI, levels = c("Short", "Long"))) %>% 
#  filter(Repeat == "Repeated") %>% 
# filter(ICI == "Short") %>% 
  ggplot() +
  aes(type, n, color = type) +
  geom_boxplot() +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  facet_grid(Repeat ~ ICI)


dat_bigram_H <-
  dat_bigram_n %>% 
  mutate(Repeat = factor(Repeat, levels = c("Repeated", "Different"))) %>% 
  mutate(ICI = factor(ICI, levels = c("Short", "Long"))) %>% 
  group_by(Name, type, fm, parents, PM, Repeat, ICI) %>% 
  summarise(H = -sum(r * log(r)))

g_H <-
  dat_bigram_H %>% 
  ggplot() +
  aes(PM, H, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point(size = 0.2) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(Repeat ~ ICI, nrow = 1) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  theme(axis.title.x = element_text(hjust = 1),
        legend.position = "none")

dat_bigram_r <-
  dat_bigram %>% 
  mutate(Repeat = factor(Repeat, levels = c("Repeated", "Different"))) %>% 
  mutate(ICI = factor(ICI, levels = c("Short", "Long"))) %>% 
  group_nest(Name, type, fm, parents, PM, ICI, Repeat) %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  group_by(Name, type, fm, parents, PM) %>% 
  mutate(r = n / sum(n)) %>% 
  ungroup()

g_r <-
  dat_bigram_r %>% 
  ggplot() +
  aes(PM, r, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point(size = 0.2) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(Repeat ~ ICI, nrow = 1) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  theme(axis.title.x = element_text(hjust = 1),
        legend.position = "none")

g <-
  patchwork::wrap_plots(
    g_r +
      theme(text = element_text(size = 7)), 
    g_H+
      theme(text = element_text(size = 7)), 
    nrow = 1
  )

g

ggsave("fig/fig_RepICI_RH.png",
       width = 174, height = 60, units = "mm")


ggsave("fig/fig_RepICI_RH.svg",
       width = 174, height = 60, units = "mm")



.levels <-
  c("ock", "tsik", "egg", "phee", "peep", "trill", "trillphee", "chirp", "twitter", "other") %>% 
  str_c(., "-", .)

dat_bigram_n %>% 
  filter(Repeat == "Repeated", ICI == "Short") %>% 
  mutate(bigram = factor(bigram, levels = .levels)) %>% 
  ggplot() +
  aes(PM, r, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point(size = 0.2) +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(~bigram, nrow =1) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  theme(legend.position = "none")


ggsave("fig/fig_RepICI_trilltrill.png",
       width = 174, height = 50, units = "mm")

ggsave("fig/fig_RepICI_trilltrill.svg",
       width = 174, height = 50, units = "mm")


dat_bigram_n %>% 
  filter(Repeat == "Repeated", ICI == "Short") %>% 
  group_by(bigram) %>% 
  rstatix::anova_test(
    r ~ type * PM
  ) %>% 
  print(n = 100)

fit1 <-
  dat_bigram_n %>% 
  filter(ICI == "Short") %>% 
  filter(Repeat == "Repeated") %>% 
  filter(bigram != "tsik-tsik") %>% 
  filter(bigram != "twitter-twitter") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(bigram, Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(bigram, Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


fit1 %>% 
  filter(dAIC == 0)


fit1 %>% 
  select(modelid, bigram, dAIC) %>% 
  pivot_wider(values_from = dAIC,
              names_from = bigram) %>% 
  print(n = 100)




dat_bigram_H %>% 
  ungroup() %>% 
  group_nest(Repeat, ICI) %>% 
  mutate(stat = map(data, \(x){
    x %>% 
      rstatix::anova_test(
        H ~ type*PM
      )
  })) %>% 
  .$stat

dat_fit <-
  dat_bigram_H %>% 
  rename(n = H) %>% 
  ungroup() %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)

dat_minAIC <-
  dat_fit %>% 
  filter(dAIC == 0)

dat_minAIC

dat_bigram_n %>% 
  filter(bigram == "trill-trill")%>% 
  ggplot() +
  aes(PM, r, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(Repeat ~ ICI) +
  scale_color_manual(values = c(UE = "black", VPA = "red"))

dat_fit_tt <-
  dat_bigram_n %>% 
  filter(bigram == "trill-trill") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


dat_minAIC_tt <-
  dat_fit_tt %>% 
  filter(dAIC == 0)


dat_bigram_n %>% 
  filter(Repeat == "Repeated", ICI == "Long") %>% 
  ggplot() +
  aes(PM, r, color = type) +
  geom_path(aes(group = Name), alpha = 0.2) +
  geom_point() +
  geom_smooth(se = F, method = "lm") +
  facet_wrap(~bigram, nrow =1) +
  scale_color_manual(values = c(UE = "black", VPA = "red"))



dat_fit_pp <-
  dat_bigram_n %>% 
  filter(bigram == "peep-peep") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


dat_minAIC_pp <-
  dat_fit_pp %>% 
  filter(dAIC == 0)

dat_fit_cc <-
  dat_bigram_n %>% 
  filter(bigram == "chirp-chirp") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


dat_minAIC_cc <-
  dat_fit_cc %>% 
  filter(dAIC == 0)




dat_fit_tptp <-
  dat_bigram_n %>% 
  filter(bigram == "trillphee-trillphee") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


dat_minAIC_tptp <-
  dat_fit_tptp %>% 
  filter(dAIC == 0)


dat_fit_phph <-
  dat_bigram_n %>% 
  filter(bigram == "phee-phee") %>% 
  select(!n) %>% 
  rename(n = r) %>% 
  group_nest(Repeat, ICI) %>%
  nest() %>% 
  mutate(model = list(list_model)) %>% 
  unnest(model) %>% 
  rowid_to_column("modelid") %>% 
  unnest(data) %>% 
  mutate(fit = map2(model, data, ~.x(.y))) %>% 
  mutate(AIC = map_dbl(fit, AIC)) %>% 
  group_by(Repeat, ICI) %>% 
  mutate(dAIC = AIC - min(AIC)) %>% 
  ungroup() %>% 
  arrange(Repeat, ICI, modelid)


dat_minAIC_phph <-
  dat_fit_phph %>% 
  filter(dAIC == 0)






dat_bigram_n %>% 
  filter(bigram == "peep-peep") %>% 
  rstatix::anova_test(
    r ~ type*PM
  )

dat_bigram_n %>% 
  filter(bigram == "phee-phee") %>% 
  rstatix::anova_test(
    r ~ type*PM
  )



dat_bigram_n <-
  dat_bigram %>% 
  select(!ici) %>% 
  group_nest(type, Repeat, ICI, bigram) %>% 
  mutate(n = map_dbl(data, nrow)) 

dat_sr <-
  dat_bigram_n %>% 
  arrange(desc(n)) %>% 
  filter(Repeat == "Repeated", ICI == "Short") %>% 
  select(!data) %>% 
  pivot_wider(values_from = n, values_fill = 0, names_from = type) %>% 
  mutate(total = VPA + UE) %>% 
  mutate(r_UE = UE / sum(UE),
         r_VPA = VPA / sum(VPA))

dat_sr %>% 
  select(UE, VPA) %>% 
  t() %>% 
  chisq.test()


dat_bigram_n %>% 
  arrange(desc(n)) %>% 
  filter(Repeat == "Different", ICI == "Long") %>% 
  select(!data) %>% 
  pivot_wider(values_from = n, values_fill = 0, names_from = type)%>% 
  mutate(total = VPA + UE) %>% 
  print(n = 100)




dat_repshort %>% 
  xtabs(~ bigram + type, data = .) %>% 
  vcd::assocstats()

dat_repshort %>% 
  xtabs(~ bigram + type, data = .) %>% 
  prop.test(correct = FALSE)

dat_repshort %>% 
  xtabs(~ bigram + type, data = .) %>% 
  prop.test(correct = TRUE)

library(magrittr)
dat_bigram %>% 
  filter(Repeat == "Repeated", ICI == "Short") %$% 
  table(bigram, type) %>% 
  prop.test(correct = FALSE)

dat_bigram %>% 
  filter(Repeat == "Repeated", ICI == "Long") %$% 
  table(bigram, type) %>% 
  prop.test(correct = FALSE)

dat_bigram %>% 
  filter(Repeat == "Repeated", type == "UE") %$% 
  table(bigram, ICI)  %>% 
  vcd::assocstats() %>% 
  summary()

dat_bigram %>% 
  filter(Repeat == "Repeated", ICI == "Short") %$% 
  table(bigram, type)  %>% 
  vcd::assocstats() %>% 
  summary()

dat_bigram %>% 
  filter(Repeat == "Repeated") %>% 
  filter(ICI == "Long") %>% 
  xtabs(~ bigram + type, data = .) %>% 
  vcd::assocstats() %>% 
  summary()

dat_bigram %>% 
  group_nest(Repeat, ICI) %>% 
  mutate(tab = map(data, \(x){
    xtabs(~bigram + type, data = x)
  })) %>% 
  mutate(stat = map(tab, vcd::assocstats),
         stat = map(stat, summary)) %>% 
  .$stata


dat_bigram %>% 
  group_nest(Repeat, ICI, Stage) %>% 
  mutate(tab = map(data, \(x){
    xtabs(~type + bigram, data = x)
  })) %>% 
  mutate(cv = map_dbl(tab, rstatix::cramer_v))



dat_bigram %>% 
  filter(Repeat == "Different", ICI == "Long") %$% 
  table(bigram, type) %>% 
  prop.test(correct = TRUE)



dat_bigram %>% 
  filter(Repeat == "Different", ICI == "Short") %$% 
  table(bigram, type) %>% 
  prop.test(correct = TRUE)
