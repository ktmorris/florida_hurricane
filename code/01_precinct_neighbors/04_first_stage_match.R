
treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")
####

neighbors <- fread("./temp/neighbors.txt") %>% 
  filter(src_county %in% treated_counties,
         nbr_county %in% control_counties,
         (src_county != "LIB" | nbr_county != "LEO")) %>% ## uncontested, different parties
  mutate(src_countypct = paste0(src_county, as.integer(src_pct)),
         nbr_countypct = paste0(nbr_county, as.integer(nbr_pct))) %>% 
  select(src_countypct,
         nbr_countypct)
####
neighbor_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  mutate(countypct = paste0(county, Precinct)) %>% 
  filter(countypct %in% c(neighbors$src_countypct, neighbors$nbr_countypct))

neighbor_voters$treated <- neighbor_voters$county %in% treated_counties


saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
neighbor_voters <- readRDS("./temp/neighbor_voters.rds")

######
# source("./code/misc/AutoCluster4.R")
# cl <- NCPUS(detectCores() - 1)
# 
# for(i in unique(neighbors$src_countypct)){
#   tryCatch({
#     samp <- filter(neighbor_voters, countypct %in% c(filter(neighbors, src_countypct == i)$nbr_countypct, i))
#     samp <- samp[complete.cases(samp), ]
#     Tr <- samp$treated
# 
#     X = samp %>%
#       dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
#                     median_income, some_college)
# 
#     ids <- samp %>%
#       mutate(id = row_number()) %>%
#       select(id, LALVOTERID)
# 
#     genout <- GenMatch(Tr = Tr, X = X, pop.size = 100, cluster = cl)
# 
#     mout <- Match(Tr = Tr, X = X, Weight.matrix = genout)
# 
#     matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
#                           control = c(mout$index.control, unique(mout$index.treated)),
#                           weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))
# 
#     matches <- left_join(matches, ids, by = c("treated" = "id")) %>%
#       select(-treated) %>%
#       rename(treated = LALVOTERID)
# 
#     matches <- left_join(matches, ids, by = c("control" = "id")) %>%
#       select(-control) %>%
#       rename(control = LALVOTERID)
# 
#     saveRDS(matches, paste0("./temp/matches_", i, ".rds"))
#   }, error = function(e){})
# }

matches <- rbindlist(lapply(unique(neighbors$src_countypct),
                            function(i){
                              if(file.exists(paste0("./temp/matches_", i, ".rds"))){
                                t <- readRDS(paste0("./temp/matches_", i, ".rds"))
                              }else{
                                t <- data.table(x = "")
                              }
                              return(t)}
                            ), fill = T)

saveRDS(matches$treated, "./temp/treated_neighbors.rds")
saveRDS(matches$control, "./temp/control_neighbors.rds")

matches <- matches %>% 
  select(-x) %>% 
  mutate(treatment = treated == control) %>% 
  filter(!is.na(weight))

saveRDS(matches, "./temp/neighbor_matches_weights.rds")
####

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   General_2018_11_06,
                                   General_2016_11_08,
                                   General_2014_11_04,
                                   General_2012_11_06,
                                   General_2010_11_02
                                   from fl_history_18")
fl_history <- filter(fl_history, LALVOTERID %in% c(matches$control))

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
fl_history2 <- filter(fl_history2, LALVOTERID %in% c(matches$control))

fl_history2 <- reshape2::melt(fl_history2, id.vars = "LALVOTERID") %>%  
  mutate(year = substring(variable, 20, 23),
         absentee = value == "Absentee",
         early = value == "Early",
         polls = value == "Poll Vote") %>% 
  dplyr::select(-variable, -value)


fl_history <- full_join(fl_history, fl_history2, by = c("LALVOTERID", "year")) %>% 
  filter(year != "")

#####################


matches <- left_join(matches, fl_history, by = c("control" = "LALVOTERID"))
matches$d18 <- matches$year == 2018

matches <- left_join(matches, neighbor_voters, by = c("control" = "LALVOTERID"))

m1 <- glm(voted ~ treatment*d18, family = "binomial", data = matches, weights = weight)

m2 <- glm(voted ~ treatment*d18 +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college, data = matches, family = "binomial", weights = weight)

save(m1, m2, file = "./temp/ll_dind.rdata")

##############
ll <- matches %>% 
  group_by(treatment, year) %>% 
  summarize(voted = weighted.mean(voted, weight)) %>% 
  ungroup()

ll2 <- matches %>% 
  filter(voted == 1) %>% 
  group_by(treatment, year) %>% 
  summarize_at(vars(absentee, polls, early), ~ weighted.mean(., weight)) %>% 
  ungroup()

ll <- full_join(ll, ll2) %>% 
  mutate(treatment = ifelse(treatment, "Treated Group", "Control Group"))

ll$treatment <- factor(ll$treatment, levels = c("Treated Group", "Control Group"))

plot <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = treatment)) + geom_line() +
  geom_point() +
  labs(linetype = "Treatment Group",
  x = "Year",
  y = "Turnout") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "LM Roman 10"))
saveRDS(plot, "./temp/ll_to.rds")


plot2 <- ggplot(ll, aes(x = as.integer(year), y = early, linetype = treatment)) + geom_line() +
  geom_point() +
  labs(linetype = "Treatment Group",
       x = "Year",
       y = "Early Turnout") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "LM Roman 10"))

plot3 <- ggplot(ll, aes(x = as.integer(year), y = absentee, linetype = treatment)) + geom_line() +
  geom_point() +
  labs(linetype = "Treatment Group",
       x = "Year",
       y = "Absentee Turnout") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "LM Roman 10"))

plot4 <- ggplot(ll, aes(x = as.integer(year), y = polls, linetype = treatment)) + geom_line() +
  geom_point() +
  labs(linetype = "Treatment Group",
       x = "Year",
       y = "Election Day Turnout") +
  scale_y_continuous(labels = scales::percent) +
  theme(text = element_text(family = "LM Roman 10"))
