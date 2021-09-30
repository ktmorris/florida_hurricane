fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate(id = row_number())

movs <- readRDS("./temp/moved_pps_dists.rds")

fl_roll <- left_join(fl_roll, movs %>% 
                       mutate(treated_change = (actual_dist - expected_dist) / 1000) %>% 
                       select(LALVOTERID, treated_change))

load("./temp/mout_hurricane_full.RData")


matches <- data.table(group = c(mout$index.treated, unique(mout$index.treated)),
                      voter = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, fl_roll, by = c("voter" = "id")) %>% 
  select(-voter, -treated_change) %>% 
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
results <- fread("./raw_data/11062018Election.txt") %>% 
  filter(RaceCode == "USR") %>% 
  group_by(district = Juris1num, candidate = paste0(CanNameFirst, CanNameLast)) %>% 
  summarize(candidatevotes = sum(CanVotes)) %>% 
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
pps <- fread("raw_data/changes.csv")
######


matches <- full_join(matches, history, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))

matches <- left_join(matches,
                     fl_roll %>% 
                       select(id, county, LALVOTERID, treated_rel = rel, treated_change) %>% 
                       rename(treated_county = county,
                              treated_voter_id = LALVOTERID),
                     by = c("group" = "id")) %>% 
  mutate(treatment = treated,
         d18 = year == "2018",
         midterm = year %in% c("2010", "2014", "2018"),
         midterm_treated = treated * midterm)


matches <- left_join(matches, select(pps, treated_county = c, share_open))


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
                  mutate(fac = "Pre-Matching"))

ll$fac <- factor(ll$fac, levels = c("Pre-Matching", "Post-Matching"))

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

saveRDS(matches, "./temp/full_reg_data.rds")

