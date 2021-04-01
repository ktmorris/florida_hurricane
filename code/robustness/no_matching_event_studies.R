
fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate(id = row_number())


ttt <- fl_roll %>% 
  mutate(county = ifelse(treated, county, "whatever")) %>% 
  group_by(treated, county) %>% 
  summarize_at(vars(starts_with("v201")), ~ mean(. != 1)) %>% 
  pivot_longer(cols = starts_with("v"), names_to = "year",
               names_prefix = "v", values_to = "to",
               names_transform = list(year = as.integer))

ttt <- rbindlist(lapply(unique(filter(ttt, treated)$county), function(c){
  filter(ttt, county %in% c(c, "whatever")) %>% 
    mutate(county = c)
})) %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group"))

ttt$treated <- factor(ttt$treated, levels = c("Treated Group", "Control Group"))

ggplot(ttt, aes(x = year, y = to, linetype = treated)) + geom_line() +
  facet_wrap(~ county, nrow = 4) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)

#############################


ttt <- fl_roll %>% 
  group_by(treated) %>% 
  summarize_at(vars(starts_with("v201")), ~ mean(. != 1)) %>% 
  pivot_longer(cols = starts_with("v"), names_to = "year",
               names_prefix = "v", values_to = "to",
               names_transform = list(year = as.integer)) %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group"))

ttt$treated <- factor(ttt$treated, levels = c("Treated Group", "Control Group"))

ggplot(ttt, aes(x = year, y = to, linetype = treated)) + geom_line() +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)

#####################

long <- select(fl_roll, treated, white, black, latino, asian, female, male, dem, rep, age,
               median_income, some_college, starts_with("v201")) %>% 
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", values_to = "to",
               names_transform = list(year = as.integer))

m1 <- lm(I(to != 1) ~ treated * I(year == 2018), long)
m2 <- lm(I(to != 1) ~ treated * I(year == 2018) + white + black +
           latino + asian + female + male + dem + rep + age +
           median_income + some_college, long)
