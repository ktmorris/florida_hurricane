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

fl_roll <- readRDS("./temp/pre_match_full_voters.rds")


history <- select(filter(fl_roll, LALVOTERID %in% combine$voter), LALVOTERID, starts_with("v201")) %>% 
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
pps <- rbindlist(lapply(list.files("raw_data/actual_expected_polling", full.names = T), function(f){
  k <- fread(f) %>% 
    mutate(m = gsub("raw_data/actual_expected_polling/|.csv", "", f))
  
  k <- cSplit(k, "m", sep = "_") %>% 
    dplyr::select(type = m_1, county = m_2) %>% 
    group_by(county, type) %>% 
    tally()
})) %>% 
  pivot_wider(id_cols = "county", names_from = "type", values_from = "n") %>% 
  mutate(c = toupper(substring(county, 1, 3)),
         share_open = actual / expected)

######
combine <- left_join(combine, history, by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, -treated), by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, LALVOTERID, treated_county = county, treated_rel = rel),
                     by = c("group_id" = "LALVOTERID"))
combine <- left_join(combine, select(pps, treated_county = c, share_open))
combine <- left_join(combine, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))


combine$d18 <- combine$year == "2018"
combine$treated_18 <- combine$treated * combine$d18
combine$share_open_18 <- combine$treated * combine$d18 * combine$share_open
combine$rel_18 <- combine$treated * combine$d18 * combine$treated_rel

combine$panhandle_18 <- combine$panhandle * combine$d18
combine$share_open_18_pan <- combine$panhandle * combine$d18 * combine$share_open
combine$rel_18_pan <- combine$panhandle * combine$d18 * combine$treated_rel
combine$rain <- combine$treated_rel

f1 <- voted ~ panhandle*d18 + treated*d18

f2 <- voted ~ panhandle*d18 + treated*d18  +
           white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college

f3 <- voted ~ panhandle*d18 + treated*d18 +
           white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + diff

f4 <- voted ~ panhandle*d18*rain + treated*d18*rain +
  panhandle*d18*share_open + treated*d18*share_open

models <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  m <- lm(f, combine, weights = weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = combine, weights = combine$weight, cluster = combine$group))[ , 2],
  summary(lm.cluster(formula = f2, data = combine, weights = combine$weight, cluster = combine$group))[ , 2],
  summary(lm.cluster(formula = f3, data = combine, weights = combine$weight, cluster = combine$group))[ , 2],
  summary(lm.cluster(formula = f4, data = combine, weights = combine$weight, cluster = combine$group))[ , 2]
)

stargazer(models,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Weather Treatment",
                               "Administrative Treatment",
                               "2018",
                               "Weather Treatment $\\times$ 2018",
                               "Administrative Treatment $\\times$ 2018",
                               "Administrative Treatment $\\times$ 2018 $\\times$ Relative Rainfall in 2018",
                               "Administrative Treatment $\\times$ 2018 $\\times$ Share of Expected Polling Places Open in 2018"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:trip-diff} Turnout, 2010 --- 2018",
          table.placement = "h",
          omit.stat = c("f", "ser", "aic"),
          # omit = c("white", "black", "latino", "asian", "female", "male",
          #          "dem", "rep", "age", "median_income", "some_college",
          #          "diff"),
          table.layout = "-cmd#-t-a-s-n",
          
          order = c(1, 4, 2, 18, 21, 27, 29),
          # order = c(1, 4, 2, 18, 21, 27, 29),
          out = "./temp/trip_dif.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = ses_cl,
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "X"),
                         c("Includes control for CD competitiveness", "", "", "X"),
                         c("Includes rainfall and its interactions", "", "", "", "X"),
                         c("Includes share of polling places open and its interactions", "", "", "", "X")))
#######################################


models <- lapply(c(unique(combine$treated_county)), function(c){
  m <- lm(f1, filter(combine, treated_county == c), weights = weight)
})

ses_cl <- list(
  summary(lm.cluster(formula = f1, data = filter(combine, treated_county == "BAY"),
                     weights = filter(combine, treated_county == "BAY")$weight,
                     cluster = filter(combine, treated_county == "BAY")$group))[ , 2],
  summary(lm.cluster(formula = f1, data = filter(combine, treated_county == "GAD"),
                     weights = filter(combine, treated_county == "GAD")$weight,
                     cluster = filter(combine, treated_county == "GAD")$group))[ , 2],
  summary(lm.cluster(formula = f1, data = filter(combine, treated_county == "JAC"),
                     weights = filter(combine, treated_county == "JAC")$weight,
                     cluster = filter(combine, treated_county == "JAC")$group))[ , 2],
  summary(lm.cluster(formula = f1, data = filter(combine, treated_county == "LIB"),
                     weights = filter(combine, treated_county == "LIB")$weight,
                     cluster = filter(combine, treated_county == "LIB")$group))[ , 2],
  summary(lm.cluster(formula = f1, data = filter(combine, treated_county == "WAS"),
                     weights = filter(combine, treated_county == "WAS")$weight,
                     cluster = filter(combine, treated_county == "WAS")$group))[ , 2]
)

stargazer(models,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Panhandle", "2018", "Panhandle $\\times$ 2018",
                               "Treated", "Treated $\\times$ 2018"),
          column.labels = unique(combine$treated_county),
          title = "\\label{tab:trip-diff} Turnout, 2010 --- 2018",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          table.layout = "-cmd#-t-a-s-n",
          out.header = F,
          notes = "TO REPLACE",
          se = ses_cl,
          out = "temp/trip_county_spec.tex")


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
  summarize(voted = weighted.mean(voted, weight)) %>%
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
  filter(panhandle, treated_county == "LIB") %>%
  group_by(treated, year) %>%
  summarize(voted = weighted.mean(voted, weight),
            rel = weighted.mean(rel, weight)) %>%
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
plot_neighbors
saveRDS(plot_neighbors, "./temp/ll_to.rds")

#######

ll3 <- combine %>%
  filter(panhandle) %>% 
  mutate(group = ifelse(treated, "Treated",
                        ifelse(panhandle, "Primary Controls", "Secondary Controls"))) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight))

ll3$group <- factor(ll3$group, levels = c("Treated",
                                              "Primary Controls",
                                              "Secondary Controls"))

plot_all <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all
saveRDS(plot_all, "temp/trip_diff_plot.rds")

#######

ll1 <- combine %>%
  filter(panhandle) %>% 
  mutate(group = ifelse(treated, "Treated", "Control")) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  mutate(fac = "Neighboring Voters")

ll2 <- combine %>%
  mutate(group = ifelse(panhandle, "Treated", "Control")) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  mutate(fac = "Panhandle vs Rest of State")

ll3 <- bind_rows(ll1, ll2)

ll3$group <- factor(ll3$group, levels = c("Treated",
                                          "Control"))

plot_all <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  facet_wrap(~fac) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all
saveRDS(plot_all, "temp/trip_diff_plot.rds")

#######################################
combine <- left_join(combine,
                     select(fl_roll, LALVOTERID, treated_county = county),
                     by = c("group_id" = "LALVOTERID"))
matches <- combine
f1 <- voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated

counties <- unique(matches$treated_county)

ses_go <- c()
for(c in counties){
  s <- lm.cluster(formula = f1, data = filter(matches, treated_county == c),
                  weights = filter(matches, treated_county == c)$weight,
                  cluster = filter(matches, treated_county == c)$group)
  
  h <- as.data.frame(confint(s))[6,] %>% 
    mutate(county = c)
  ses_go <- c(ses_go, list(h))
}

ses_go <- rbindlist(ses_go)
colnames(ses_go) <- c("lower", "upper", "county")

ses_go <- ses_go %>% 
  mutate(estimate = (lower + upper) / 2)


pps <- left_join(pps, ses_go, by = c("c" = "county"))

pps$s <- pps$actual / pps$expected


fff <- filter(matches, treated) %>% 
  group_by(county) %>% 
  summarize(rel = mean(rel))

pps <- left_join(pps, fff, by = c("c" = "county"))

ggplot(pps, aes(x=s, y=estimate)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  geom_smooth(method = "lm") +
  theme_bc(base_family = "LM Roman 10") + 
  geom_hline(yintercept = 0) +
  labs(x = "Share of Expected Polling Places that Remained Open",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(0, max(pps$s), 0.2)) +
  scale_y_continuous(labels = percent)

ggplot(pps, aes(x=rel, y=estimate)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  geom_smooth(method = "lm") +
  theme_bc(base_family = "LM Roman 10") + 
  geom_hline(yintercept = 0) +
  labs(x = "Relative Rainfall",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(min(pps$rel), max(pps$rel), 0.2)) +
  scale_y_continuous(labels = percent)
