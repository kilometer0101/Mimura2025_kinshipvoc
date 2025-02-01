


path <- "/Users/km_pro/Documents/R/rawdata/kinship/kinship/locomotion_independency.csv"
id <- 
  "data/id.csv" %>% 
  read_csv() %>% 
  dplyr::select(Name, Pup, sex, birth, Mother, Father)


dat_raw <-
  path %>% 
  read_csv() 

dat <-
  dat_raw   %>% 
  left_join(
    id,
    by = c("Name")
  ) %>%
  mutate(birth = ymd(birth),
         date = ymd(date),
         age_d = date - birth) %>% 
  mutate(Treat = if_else(str_detect(Pup, "UE"), "UE", "VPA")) %>% 
  mutate(start = hms(start),
         end = hms(end),
         end = if_else(end > minutes(30), minutes(30), end),
         d = end - start) %>% 
  mutate(d_s =  seconds(d)) %>% 
  filter(Treat %in% c("UE", "VPA"))

dat_g <-
  dat %>% 
  mutate(parents = str_c(Mother, "-", Father)) %>% 
  group_by(Name, date, state, age_d, Treat, parents) %>% 
  summarise(d = sum(d_s)) %>% 
  ungroup() %>% 
  pivot_wider(values_from = d,
              names_from = state,
              values_fill = 0)  %>% 
  mutate(age_m = round(age_d / 7),
         age_m = as.numeric(age_m))


dat_g %>% 
  ggplot() +
  aes(age_m/ 4, independent/60, color = Treat) +
  geom_path(aes(group = Name), alpha = 0.3) +
  geom_point(
    #aes(shape = parents)
    ) +
  scale_color_manual(values = c(UE = "black", VPA = "red")) +
  scale_x_continuous(breaks = seq(0, 30, 1))+
  theme(text = element_text(size = 7)) +
  ggtitle("Independent locomotion (min)")

ggsave("fig/fig_independency.png",
       width = 110, height = 100, units = "mm")
ggsave("fig/fig_independency.svg",
       width = 110, height = 100, units = "mm")

dat_g %>% 
  filter(age_m <= 5) %>% 
  group_by(Name) %>% 
  filter(date == min(date)) %>% 
  ggplot() +
  aes(Treat, independent/60, color = Treat) +
  geom_boxplot(outliers = FALSE) +
  geom_point(aes(shape = parents), size = 5, alpha = 0.5) +
  scale_color_manual(values = c(UE = "black", VPA = "red"))
#+
#  ggtitle("Independent locomotion (min)")

ggsave("fig/fig_independency_box.png",
       width = 75, height = 100, units = "mm")
ggsave("fig/fig_independency_box.svg",
       width = 75, height = 100, units = "mm")

dat_g %>% 
  filter(age_m <= 5) %>% 
  group_by(Name) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>% 
  rstatix::anova_test(
    independent ~ Treat + parents
  )

dat_g %>% 
  filter(age_m <= 5) %>% 
  group_by(Name) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>% 
  group_nest(Treat) %>% 
  pivot_wider(
    values_from = data,
    names_from = Treat
  ) %>% 
  mutate(stat = map2(UE, VPA, \(x, y){
    lawstat::brunner.munzel.test(x$independent, y$independent)
  })) %>% 
  .$stat
