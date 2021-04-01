

fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county)

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

load("./temp/mout_hurricane_full_full_exacts.RData")


matches <- data.table(group = c(mout$index.treated, mout$index.treated),
                      voter = c(mout$index.control, mout$index.treated),
                      weight = c(mout$weights, mout$weights))

matches <- left_join(matches, ids, by = c("voter" = "id")) %>% 
  select(-voter) %>% 
  rename(voter = LALVOTERID)

matches <- left_join(matches, ids, by = c("group" = "id")) %>% 
  select(-group) %>% 
  rename(group = LALVOTERID)

##################

history <- select(filter(fl_roll, LALVOTERID %in% matches$voter), LALVOTERID, starts_with("v201")) %>% 
  pivot_longer(cols = starts_with("v"),
               names_to = "year", values_to = "to") %>% 
  mutate(voted = to != 1,
         absentee = to == 2,
         early = to == 3,
         polls = to == 4,
         year = gsub("v", "", year)) %>% 
  select(-to)


###############
results <- fread("./raw_data/district_overall_2018.txt") %>% 
  filter(state_po == "FL",
         stage == "gen") %>% 
  group_by(district, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes) / 2) %>% 
  group_by(district) %>% 
  mutate(share = candidatevotes / sum(candidatevotes)) %>% 
  arrange(district, desc(share)) %>% 
  filter(row_number() <= 2) %>% 
  select(district, share) %>% 
  mutate(count = row_number())

results <- pivot_wider(results, id_cols = district, values_from = share,
                       names_from = count, names_prefix = "share") %>% 
  mutate(diff = 1 - (share1 - share2)) %>% 
  select(district, diff) %>% 
  ungroup() %>% 
  mutate(district = as.integer(gsub("District ", "", district)))
######

matches <- full_join(matches, history, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, fl_roll, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))

matches <- left_join(matches,
                     fl_roll %>% 
                       select(county, LALVOTERID) %>% 
                       rename(treated_county = county),
                     by = c("group" = "LALVOTERID")) %>% 
  mutate(treatment = treated,
         d18 = year == "2018",
         midterm = year %in% c("2010", "2014", "2018"),
         midterm_treated = treated * midterm)

saveRDS(matches, "./temp/full_reg_data_exacts.rds")

matches <- readRDS("./temp/full_reg_data_exacts.rds")

f1 <- voted ~ treatment*d18 +
  midterm + midterm_treated

f2 <- voted ~ treatment*d18 +
  midterm + midterm_treated +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f3 <- voted ~ treatment*d18 +
  midterm + midterm_treated +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + diff


models <- lapply(c(f1, f2, f3), function(f){
  m <- lm(f, matches, weights = weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches, weights = matches$weight, cluster = matches$group))[ , 2]
)

save(models, ses_cl, file = "./temp/full_dind_reg_exact.rdata")
# source("./code/misc/make_full_latex.R")
# 
# m1 <- glm(voted ~ treatment + d18,
#           data = filter(matches, !treated | !d18), weights = weight)
# m2 <- glm(voted ~ treatment + d18 +
#             white + black + latino + asian +
#             female + male + dem + rep + age +
#             median_income + some_college,
#           data = filter(matches, !treated | !d18), weights = weight)
# m3 <- glm(voted ~ treatment + d18 +
#             white + black + latino + asian +
#             female + male + dem + rep + age +
#             median_income + some_college + diff,
#           data = filter(matches, !treated | !d18), weights = weight)
# 
# matches$pred1 <- predict(m1, matches, type = "response")
# matches$pred2 <- predict(m2, matches, type = "response")
# matches$pred3 <- predict(m3, matches, type = "response")
# 
# observed <- mean(filter(matches, treated, d18)$voted)
# to1 <- mean(filter(matches, treated, d18)$pred1)
# to2 <- mean(filter(matches, treated, d18)$pred2)
# to3 <- mean(filter(matches, treated, d18)$pred3)
# 
# save(observed, to1, to2, to3, file = "./temp/predicted_turnout_full.rdata")

####

ll <- matches %>% 
  group_by(treated, year) %>% 
  summarize_at(vars(absentee, polls, early, voted), ~ weighted.mean(., weight)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group"))

ll$treated <- factor(ll$treated, levels = c("Treated Group", "Control Group"))

p <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(p, "./temp/full_to_fig_exact.rds")

######

lll <- pivot_longer(ll, cols = c(polls, early, absentee)) %>% 
  mutate(name = ifelse(name == "polls", "At Polling Place",
                       ifelse(name == "absentee", "Absentee",
                              "Early In Person")))

lll$name <- factor(lll$name, levels = c("At Polling Place", "Early In Person", "Absentee"))

p2 <- ggplot(lll, aes(x = as.integer(year), y = value, linetype = treated)) +
  geom_line() + geom_point() +
  facet_grid(. ~ name) +
  labs(y = "Votes Cast as Share of Registered Voters in 2018", x = NULL, linetype = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, 
                                    colour = "grey20"),
        strip.background = element_rect(fill = NA, 
                                        colour = "grey20"))

saveRDS(p2, "./temp/vote_mode_exact.rds")
