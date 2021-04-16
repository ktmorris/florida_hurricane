

fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate(id = row_number())

load("./temp/mout_hurricane_full.RData")


matches <- data.table(group = c(mout$index.treated, unique(mout$index.treated)),
                      voter = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, fl_roll, by = c("voter" = "id")) %>% 
  select(-voter) %>% 
  rename(voter = LALVOTERID)

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

matches <- full_join(matches, history, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))

matches <- left_join(matches,
                     fl_roll %>% 
                       select(id, county, LALVOTERID, treated_rel = rel) %>% 
                       rename(treated_county = county,
                              treated_voter_id = LALVOTERID),
                     by = c("group" = "id")) %>% 
  mutate(treatment = treated,
         d18 = year == "2018",
         midterm = year %in% c("2010", "2014", "2018"),
         midterm_treated = treated * midterm)


matches <- left_join(matches, select(pps, treated_county = c, share_open))
saveRDS(matches, "./temp/full_reg_data.rds")

matches <- readRDS("./temp/full_reg_data.rds")

matches$treated_18 <- matches$treated * matches$d18
matches$share_open_18 <- matches$treated * matches$d18 * matches$share_open
matches$rel_18 <- matches$treated * matches$d18 * matches$treated_rel

f1 <- voted ~ treatment*d18

f2 <- voted ~ treatment*d18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f3 <- voted ~ treatment*d18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + diff

f4 <- voted ~ treatment*d18*treated_rel

f5 <- voted ~ treatment*d18*treated_rel + treatment*d18*share_open

models <- lapply(c(f1, f2, f3, f4, f5), function(f){
  m <- lm(f, matches, weights = weight)
})


ses_cl <- list(
  summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f2, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f3, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f4, data = matches, weights = matches$weight, cluster = matches$group))[ , 2],
  summary(lm.cluster(formula = f5, data = matches, weights = matches$weight, cluster = matches$group))[ , 2]
)

source("./code/misc/make_full_latex.R")
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


###########
ll <- matches %>% 
  group_by(treated, year) %>% 
  summarize_at(vars(absentee, polls, early, voted), ~ weighted.mean(., weight)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated, "Treated Voters", "Untreated Voters"),
         fac = "Post-Matching")

tot_unt <- fl_roll %>% 
  filter(!treated) %>% 
  mutate(treated = "All Untreated Voters") %>% 
  group_by(treated) %>% 
  summarize_at(vars(starts_with("v201")), ~mean(. != 1)) %>% 
  pivot_longer(cols = starts_with("v201"), names_to = "year", values_to = "voted") %>% 
  mutate(year = gsub("v", "", year))

ll <- bind_rows(ll,
                bind_rows(filter(ll, treated == "Treated Voters"),
                          tot_unt %>% 
                            mutate(treated = "Untreated Voters")) %>% 
                  mutate(fac = "Raw Data"))

ll$fac <- factor(ll$fac, levels = c("Raw Data", "Post-Matching"))

p <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = treated)) +
  facet_wrap(~ fac) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
p
saveRDS(p, "./temp/full_to_fig.rds")
####

ll2 <- matches %>% 
  group_by(treated, year, treated_county) %>% 
  summarize_at(vars(absentee, polls, early, voted), ~ weighted.mean(., weight)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group"),
         fac = "Post-Matching")

tot_unt <- fl_roll %>% 
  filter(!treated) %>% 
  mutate(treated = "Control Group") %>% 
  group_by(treated) %>% 
  summarize_at(vars(starts_with("v201")), ~mean(. != 1)) %>% 
  pivot_longer(cols = starts_with("v201"), names_to = "year", values_to = "voted") %>% 
  mutate(year = gsub("v", "", year))

tot_unt <- rbindlist(lapply(unique(ll2$treated_county), function(c){
  tot_unt %>% 
    mutate(treated_county = c,
           fac = "Unprocessed")
}))

f <- bind_rows(ll2, tot_unt, ll2 %>% filter(treated == "Treated Group") %>% mutate(fac = "Unprocessed"))

f$treated <- factor(f$treated, levels = c("Treated Group", "Control Group"))
f$fac <- factor(f$fac, levels = c("Unprocessed", "Post-Matching"))

p <- ggplot(f, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  facet_grid(treated_county~fac)
p
saveRDS(p, "temp/indiv_counties.rds")
######

lll <- pivot_longer(filter(ll, fac == "Post-Matching"), cols = c(polls, early, absentee)) %>% 
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

saveRDS(p2, "./temp/vote_mode.rds")

#####################################################

f1 <- voted ~ treatment*d18*treated_county

m <- lm(f1, matches, weights = weight)

ses <- summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$group))[ , 2]

cints <- as.data.table(confint(m))
colnames(cints) <- c("lower", "upper")

cints$name <- names(m[["coefficients"]])
cints$est <- m$coefficients

cints <- filter(cints, grepl("treatmentTRUE:d18TRUE", name))


cints$s <- cints %>% 
  filter(name == "treatmentTRUE:d18TRUE") %>% 
  dplyr::select(est) %>% 
  pull()

cints <- cints %>% 
  mutate_at(vars(lower, upper, est), ~ ifelse(name == "treatmentTRUE:d18TRUE", ., .+s)) %>% 
  mutate(county = gsub("treatmentTRUE:d18TRUE:treated_county", "", name),
         county = ifelse(county == "treatmentTRUE:d18TRUE", "BAY", county))

pps <- rbindlist(lapply(list.files("raw_data/actual_expected_polling", full.names = T), function(f){
  k <- fread(f) %>% 
    mutate(m = gsub("raw_data/actual_expected_polling/|.csv", "", f))
  
  k <- cSplit(k, "m", sep = "_") %>% 
    dplyr::select(type = m_1, county = m_2) %>% 
    group_by(county, type) %>% 
    tally()
})) %>% 
  pivot_wider(id_cols = "county", names_from = "type", values_from = "n") %>% 
  mutate(c = toupper(substring(county, 1, 3)))

pps <- left_join(pps, cints, by = c("c" = "county"))

pps$s <- pps$actual / pps$expected


fff <- filter(matches, treated) %>% 
  group_by(county) %>% 
  summarize(rel = mean(rel),
            count = n())

pps <- left_join(pps, fff, by = c("c" = "county"))

closed_counties <- ggplot(pps, aes(x=s, y=est)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  geom_smooth(method = "lm",
              mapping = aes(weight = count)) +
  theme_bc(base_family = "LM Roman 10") + 
  geom_hline(yintercept = 0) +
  labs(x = "Share of Expected Polling Places that Remained Open",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(0, max(pps$s), 0.2)) +
  scale_y_continuous(labels = percent)
closed_counties
saveRDS(closed_counties, "temp/closed_counties.rds")

rain_counties <- ggplot(pps, aes(x=rel, y=est)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper)) +
  geom_smooth(method = "lm",
              mapping = aes(weight = count)) +
  theme_bc(base_family = "LM Roman 10") + 
  geom_hline(yintercept = 0) +
  labs(x = "Relative Rainfall",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(min(pps$rel), max(pps$rel), 0.2)) +
  scale_y_continuous(labels = percent)
saveRDS(rain_counties, "temp/rain_counties.rds")
#####################################################
stargazer(m,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Treated", "2018",
                               "Treated $\\times$ 2018",
                               "Treated $\\times$ 2018 $\\times$ Calhoun",
                               "Treated $\\times$ 2018 $\\times$ Franklin",
                               "Treated $\\times$ 2018 $\\times$ Gadsden",
                               "Treated $\\times$ 2018 $\\times$ Gulf",
                               "Treated $\\times$ 2018 $\\times$ Jackson",
                               "Treated $\\times$ 2018 $\\times$ Liberty",
                               "Treated $\\times$ 2018 $\\times$ Washington"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:county-effs} Turnout, 2010 --- 2018",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          keep = c(1, 2, 10, 25:31),
          table.layout = "-cmd#-t-a-s-n",
          # order = c(1, 2, 3, 18, 19, 16, 17),
          out = "./temp/county_reg.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = list(ses),
          add.lines=list(c("Includes Fixed Effects for Treated County" , "X"),
                         c("Includes Fixed Effects for Treated County interacted with 2018" , "X")))

j <- fread("./temp/county_reg.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{2}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))



j <- bind_rows(j) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  dplyr::select(-n)

write.table(j, "./temp/county_clean_regs.tex", quote = F, col.names = F,
            row.names = F)
