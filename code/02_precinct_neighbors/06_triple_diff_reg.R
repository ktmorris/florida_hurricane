first_stage <- readRDS("./temp/neighbor_matches_weights.rds") %>% 
  mutate(treated = group_id == voter,
         panhandle = T,
         secondary_control_1 = F)
######
second_stage <- readRDS("./temp/second_stage_matches.rds") %>% 
  filter(group_id != voter) %>% 
  rename(weight2 = weight)

second_stage <- left_join(second_stage,
                          select(first_stage, voter, weight, treated, gi = group_id), by = c("group_id" = "voter")) %>% 
  mutate(weight = weight * weight2,
         secondary_control_1 = treated,
         panhandle = F,
         treated = F) %>% 
  select(-weight2,
         -group_id) %>% 
  rename(group_id = gi)
######
combine <- bind_rows(first_stage, second_stage)

saveRDS(combine, "./temp/voters_weights_tripdif.rds")
######
history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")
fl_history <- filter(fl_history, LALVOTERID %in% c(combine$voter))

fl_history <- reshape2::melt(fl_history, id.vars = "LALVOTERID") %>% 
  mutate(year = substring(variable, 9, 12),
         voted = ifelse(value == "Y", 1, 0)) %>% 
  dplyr::select(-variable, -value)
#######
fl_history2 <- dbGetQuery(history, "select LALVOTERID,
                                   BallotType_General_2018_11_06,
                                   BallotType_General_2016_11_08,
                                   BallotType_General_2014_11_04,
                                   BallotType_General_2012_11_06,
                                   BallotType_General_2010_11_02
                                   from florida_history_type")
fl_history2 <- filter(fl_history2, LALVOTERID %in% c(combine$voter))

fl_history2 <- reshape2::melt(fl_history2, id.vars = "LALVOTERID") %>%  
  mutate(year = substring(variable, 20, 23),
         absentee = value == "Absentee",
         early = value == "Early",
         polls = value == "Poll Vote") %>% 
  dplyr::select(-variable, -value)


fl_history <- full_join(fl_history, fl_history2, by = c("LALVOTERID", "year")) %>% 
  filter(year != "")
#####
fl_roll <- readRDS("./temp/pre_match_full_voters.rds")
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
# #######
combine <- left_join(combine,
                     select(fl_roll, LALVOTERID, county),
                     by = c("group_id" = "LALVOTERID"))

combine <- filter(combine, county != "FRA")

w2 <- readRDS("./temp/weights_for_treated.rds") %>%
  rename(tr_weight = weight)

combine <- left_join(combine, w2, by = "county") %>%
  mutate(weight2 = weight * tr_weight) %>%
  select(-county, -tr_weight)
######
combine <- left_join(combine, fl_history, by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, -treated), by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))
combine$d18 <- combine$year == "2018"
combine$d18_panhandle <- combine$d18 * combine$panhandle
combine$d18_treated <- combine$d18 * combine$treated
combine$panhandle_midterm <- combine$panhandle * (combine$year %in% c(2010, 2014, 2018))
combine$treated_midterm <- combine$treated * (combine$year %in% c(2010, 2014, 2018))
combine$midterm <- combine$year %in% c(2010, 2014, 2018)

f1 <- voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated + secondary_control_1 +
           midterm + panhandle_midterm + treated_midterm

f2 <- voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated + secondary_control_1  +
           midterm + panhandle_midterm + treated_midterm + 
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college

f3 <- voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated + secondary_control_1 +
           midterm + panhandle_midterm + treated_midterm + 
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + diff

models <- lapply(c(f1, f2, f3), function(f){
  m <- lm(f, combine, weights = weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = combine, weights = combine$weight, cluster = combine$group))[ , 2],
  summary(lm.cluster(formula = f2, data = combine, weights = combine$weight, cluster = combine$group))[ , 2],
  summary(lm.cluster(formula = f3, data = combine, weights = combine$weight, cluster = combine$group))[ , 2]
)

save(models, ses_cl, file = "./temp/triple_diff_regs.rdata")

stargazer(models,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Panhandle", "2018", "Panhandle $\\times$ 2018",
                               "Treated", "Treated $\\times$ 2018",
                               "Secondary Control Group 1",
                               "Midterm",
                               "Panhandle $\\times$ Midterm",
                               "Treated $\\times$ Midterm"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:trip-diff} Turnout, 2010 --- 2018",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          omit = c("white", "black", "latino", "asian", "female", "male",
                   "dem", "rep", "age", "median_income", "some_college",
                   "diff"),
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = ses_cl,
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "X"),
                         c("Includes control for CD competitiveness", "", "", "X")))
# 
# m1 <- glm(voted ~ panhandle + d18 + d18_panhandle + treated + secondary_control_1,
#           data = filter(combine, !treated | !d18), weights = weight,
#           family = "binomial")
# 
# m2 <- glm(voted ~ panhandle + d18 + d18_panhandle + treated + secondary_control_1 +
#             white + black + latino + asian +
#             female + male + dem + rep + age +
#             median_income + some_college,
#           data = filter(combine, !treated | !d18), weights = weight,
#           family = "binomial")
# 
# m3 <- glm(voted ~ panhandle + d18 + d18_panhandle + treated + secondary_control_1 +
#             white + black + latino + asian +
#             female + male + dem + rep + age +
#             median_income + some_college + diff,
#           data = filter(combine, !treated | !d18), weights = weight,
#           family = "binomial")
# 
# combine$pred1 <- predict(m1, combine, type = "response")
# combine$pred2 <- predict(m2, combine, type = "response")
# combine$pred3 <- predict(m3, combine, type = "response")
# 
# observed <- mean(filter(combine, treated, d18)$voted)
# to1 <-      mean(filter(combine, treated, d18)$pred1)
# to2 <-      mean(filter(combine, treated, d18)$pred2)
# to3 <-      mean(filter(combine, treated, d18)$pred3)
# 
# save(observed, to1, to2, to3, file = "./temp/predicted_turnout_tripdiff.rdata")
########
ll <- combine %>%
  group_by(panhandle, year) %>%
  summarize(voted = weighted.mean(voted, weight2)) %>%
  ungroup() %>%
  mutate(panhandle = ifelse(panhandle, "Panhandle Voters",
                            "Secondary Control Voters"))

ll$panhandle <- factor(ll$panhandle, levels = c("Panhandle Voters",
                                                  "Secondary Control Voters"))

plot_pan <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = panhandle)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(plot_pan, "./temp/plot_pan.rds")


##########

ll2 <- combine %>%
  filter(panhandle) %>%
  group_by(treated, year) %>%
  summarize(voted = weighted.mean(voted, weight2)) %>%
  ungroup() %>%
  mutate(treated = ifelse(treated, "Treated Voters",
                            "Primary Control Voters"))

ll2$treated <- factor(ll2$treated, levels = c("Treated Voters",
                                                "Primary Control Voters"))

plot_neighbors <- ggplot(ll2, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(plot_neighbors, "./temp/ll_to.rds")

#######

ll3 <- combine %>%
  mutate(group = ifelse(treated, "Treated",
                        ifelse(panhandle, "Primary Control",
                               ifelse(secondary_control_1, "Secondary Control1", "Secondary Control2")))) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight))

ggplot(ll3, aes(x = as.integer(year), y = voted, color = group)) +
  geom_line() + geom_point() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)

##########
ll <- combine %>% 
  mutate(group = ifelse(treated, "Treated",
                        ifelse(panhandle, "Primary Control",
                               ifelse(secondary_control_1, "Secondary Control1", "Secondary Control2")))) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  ungroup()

ll2 <- combine %>% 
  filter(voted == 1) %>% 
  mutate(group = ifelse(treated, "Treated",
                        ifelse(panhandle, "Primary Control",
                               ifelse(secondary_control_1, "Secondary Control1", "Secondary Control2")))) %>%
  group_by(group, year) %>%
  summarize_at(vars(absentee, polls, early), ~ weighted.mean(., weight)) %>% 
  ungroup()

ll <- full_join(ll, ll2)

p <- ggplot(filter(ll, group %in% c("Treated", "Primary Control")),
            aes(x = as.integer(year),y = early, linetype = group)) +
  geom_line() + geom_point() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)

p
##########

ll7 <- combine %>%
  filter((!treated & panhandle) | (!panhandle & !secondary_control_1)) %>%
  group_by(treated = panhandle, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>%
  ungroup() %>%
  mutate(treated = ifelse(treated, "Treated Voters",
                          "Primary Control Voters"))

ll7$treated <- factor(ll7$treated, levels = c("Treated Voters",
                                              "Primary Control Voters"))

ggplot(ll7, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
