
source("source.R")

path_file <-
  "data/dat_all.csv"

.calls <- c("ock", "tsik", "egg", "phee", "peep", "trill", "trillphee", "chirp", "twitter")


dat_raw <-
  path_file %>% 
  fread() %>% 
  as_tibble() 

dat_callN_all <-
  dat_raw %>% 
  group_nest(calltype) %>% 
  mutate(type = "all") %>% 
  bind_rows(dat_raw %>% group_nest(calltype, type)) %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  group_by(type) %>% 
  mutate(r = n / sum(n)) %>% 
  mutate(calltype = factor(calltype, levels = c(.calls, "other")))

dat_callN_all %>% 
  ggplot() +
  aes(type, r, fill = calltype) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Set3")+
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.5, 1),
                     expand = c(0, 0.01)) +
  theme(axis.title.x = element_blank(),
        text = element_text(size = 7),
        legend.key.size = unit(2.5, "mm"),
        line = element_line(linewidth = 0.2),
        axis.title.y = element_blank()) 

ggsave("fig/callfreq.png", width = 50, height = 45, units = "mm")
ggsave("fig/callfreq.svg", width = 50, height = 45, units = "mm")

dat_N <-
  dat_raw %>% 
  group_nest(Name, calltype, type, PW, PM, fm, parents) %>% 
  mutate(n = map_dbl(data, nrow))


g_callall <-
  dat_N %>% 
  filter(calltype != "other") %>% 
  mutate(calltype = factor(calltype, .calls)) %>% 
  ggplot() +
  aes(PM, n / 30, color = type) +
  geom_line(aes(group = Name), alpha = 0.2) +
  geom_point(alpha = 0.2, size = 0.5) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  facet_wrap(~calltype, scales = "free_y", nrow =  1) +  
  labs(y = "Calls/min") +
  theme(text = element_text(size = 7),
        line = element_line(linewidth = 0.5),
        legend.position = "none",
        axis.title.x = element_text(hjust = 1))

ggsave("fig/call_all.svg", g_callall,
       width = 178, height = 40, units = "mm")

ggsave("fig/call_all.png", g_callall,
       width = 178, height = 40, units = "mm")



.calls <-  c("phee", "trill", "ock", "twitter")

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

dat_N %>% 
  group_by(calltype) %>% 
  rstatix::anova_test(
    n ~ PM * type
  )%>% 
  data.frame() %>% 
  filter(Effect == "type")

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

dat_minAIC <-
  dat_fit %>% 
  filter(dAIC == 0)

dat_minAIC

