

fl_roll <- readRDS("./temp/pre_match_full_voters_5p.rds") %>% 
  mutate(id = row_number())

load("./temp/mout_hurricane_full_5p.RData")


matches <- rbind(data.table(group = mout$index.treated,
                            voter = mout$index.control),
                 data.table(group = mout$index.treated,
                            voter = mout$index.treated))

matches <- left_join(matches, fl_roll, by = c("voter" = "id"))

##################

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")
fl_history <- filter(fl_history, LALVOTERID %in% c(matches$LALVOTERID))

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
fl_history2 <- filter(fl_history2, LALVOTERID %in% c(matches$LALVOTERID))

fl_history2 <- reshape2::melt(fl_history2, id.vars = "LALVOTERID") %>%  
  mutate(year = substring(variable, 20, 23),
         absentee = value == "Absentee",
         early = value == "Early",
         polls = value == "Poll Vote") %>% 
  dplyr::select(-variable, -value)


fl_history <- full_join(fl_history, fl_history2, by = c("LALVOTERID", "year")) %>% 
  filter(year != "")
rm(fl_history2)
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

matches <- full_join(matches, fl_history)
matches <- left_join(matches, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))

matches <- left_join(matches,
                     fl_roll %>% 
                       select(id, county, LALVOTERID) %>% 
                       rename(treated_county = county,
                              treated_voter_id = LALVOTERID),
                     by = c("group" = "id")) %>% 
  mutate(treatment = treated,
         d18 = year == "2018")

saveRDS(matches, "./temp/full_reg_data.rds")

m1 <- glm(voted ~ treatment*d18,
          data = matches, family = "binomial")
m2 <- glm(voted ~ treatment*d18 +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college,
          data = matches, family = "binomial")
m3 <- glm(voted ~ treatment*d18 +
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college + diff,
          data = matches, family = "binomial")
save(m1, m2, m3, file = "./temp/full_dind_reg.rdata")

###### overall effect for treated neighbor voters

matches <- readRDS("./temp/full_reg_data.rds") %>% 
  filter(treated_voter_id %in% readRDS("./temp/treated_neighbors.rds"))

m3_neighbors <- glm(voted ~ treatment*d18 +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college + diff,
          data = matches, family = "binomial")
save(m3_neighbors, file = "./temp/statewide_neighbors_reg.rdata")
####

ll <- matches %>% 
  group_by(year, treated) %>% 
  summarize(total_votes = sum(voted),
            voted = mean(voted),
            absentee = sum(absentee),
            early = sum(early),
            polls = sum(polls)) %>% 
  ungroup() %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group")) %>% 
  mutate_at(vars(absentee, early, polls), ~ . / total_votes)

ll$treated <- factor(ll$treated, levels = c("Treated Group", "Control Group"))

p <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() +
  theme(text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(p, "./temp/full_to_fig.rds")
