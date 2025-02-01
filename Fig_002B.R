
source("source.R")

path_file <-
  "data/dat_all.csv"

dat_raw <-
  path_file %>% 
  fread() %>% 
  as_tibble() 

dat_allN <-
  dat_raw %>% 
  group_nest(Name, type, PW, PM, fm, parents) %>% 
  mutate(n = map_dbl(data, nrow))


dat_allN %>% 
  rstatix::anova_test(
    n ~ PM * type
  )

dat_allN %>% 
  group_by(type) %>% 
  rstatix::anova_test(
    n ~ PM * Name
  )



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

dat_pred <-
  dat_minAIC %>% 
  unnest(data) %>% 
  mutate(pred = a0 + PW * a1)
  
dat_pred %>% 
  ggplot() +
  aes(PM, n / 30, color = type) +
  geom_path(aes(group = Name), alpha = 0.2, linewidth = 0.2) +
  geom_point(alpha = 0.2, size = 0.2) +
  geom_line(aes(y = pred/30),
            linewidth = 0.5) +
  #  scale_x_continuous(limits = c(1, 6), breaks = 1:6) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  labs(y = "Calls/min") +
  theme(text = element_text(size = 7),
        line = element_line(linewidth = 0.2),
        legend.key.size = unit(2.5, "mm"))

ggsave("fig/freq_min_PM.png", width = 45, height = 50, units = "mm")

ggsave("fig/freq_min_PM.svg", width = 45, height = 50, units = "mm")
