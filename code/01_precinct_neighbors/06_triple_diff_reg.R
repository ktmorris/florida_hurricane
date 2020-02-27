control_matches <- readRDS("./temp/control_matches.rds") %>% 
  rename(weight2 = weight,
         voter = control,
         control = group_id) %>% 
  mutate(second_match = T,
         first_match = F)

##########
neighbor_matches <- readRDS("./temp/neighbor_matches_weights.rds") %>% 
  filter(!treatment) %>% 
  rename(group_id = treated) %>% 
  select(-treatment)

neighbor_matches <- left_join(neighbor_matches, control_matches, by = "control") %>% 
  mutate(weight = weight * weight2) %>% 
  select(-weight2) %>% 
  mutate(panhandle = control == voter,
         treated = F) %>% 
  select(-control)

##########

fl_roll <- readRDS("./temp/pre_match_full_voters_5p.rds") %>% 
  mutate(id = row_number())

ids <- fl_roll %>% select(id, LALVOTERID)

load("./temp/mout_hurricane_full_5p.RData")


treated_matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

treated_matches <- left_join(treated_matches, ids, by = c("treated" = "id")) %>% 
  select(-treated) %>% 
  rename(group_id = LALVOTERID)

treated_matches <- left_join(treated_matches, ids, by = c("control" = "id")) %>% 
  select(-control) %>% 
  rename(voter = LALVOTERID)

treated_matches <- treated_matches %>% 
  filter(group_id %in% readRDS("./temp/treated_neighbors.rds")) %>% 
  mutate(treated = voter == group_id,
         panhandle = treated,
         second_match = F,
         first_match = T)

treated_matches$weight <- treated_matches$weight * (sum(neighbor_matches$weight) / sum(treated_matches$weight))
######
combine <- bind_rows(treated_matches, neighbor_matches)

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
#######
combine <- left_join(combine, fl_history, by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, -treated), by = c("voter" = "LALVOTERID"))
combine$d18 <- combine$year == "2018"
combine$d18_panhandle <- combine$d18 * combine$panhandle
combine$d18_treated <- combine$d18 * combine$treated

m1 <- glm(voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated,
          data = combine, weights = weight,
          family = "binomial")

m2 <- glm(voted ~ panhandle + d18 + d18_panhandle + treated + d18_treated +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college,
          data = combine, weights = weight,
          family = "binomial")

save(m1, m2, file = "./temp/triple_diff_regs.rdata")
########


ll1 <- combine %>% 
  filter(second_match == T) %>% 
  group_by(panhandle, year) %>% 
  summarize(voted = weighted.mean(voted, weight))

ggplot(ll1, aes(x = as.integer(year), y = voted, linetype = panhandle)) +
  geom_line() + geom_point() +
  theme(text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)


ll2 <- combine %>% 
  filter(first_match == T) %>% 
  group_by(treated, year) %>% 
  summarize(voted = weighted.mean(voted, weight))

ggplot(ll1, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() +
  theme(text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)

ll3 <- combine %>% 
  group_by(panhandle, year) %>% 
  summarize(voted = weighted.mean(voted, weight)) %>% 
  ungroup() %>% 
  mutate(panhandle = ifelse(panhandle, "Panhandle Voters",
                            "Secondary Control Voters"))

ll3$panhandle <- factor(ll3$panhandle, levels = c("Panhandle Voters",
                                                "Secondary Control Voters"))

plot_pan <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = panhandle)) +
  geom_line() + geom_point() +
  theme(text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(plot_pan, "./temp/plot_pan.rds")
