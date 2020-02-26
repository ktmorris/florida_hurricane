
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

fl_voters <- dbGetQuery(db, 
                        paste0("select LALVOTERID,
                        Voters_StateVoterID,
                        Voters_FIPS,
                        Residence_Addresses_CensusTract,
                        Residence_Addresses_CensusBlockGroup,
                        Voters_Gender,
                        Voters_Age,
                        Parties_Description,
                        EthnicGroups_EthnicGroup1Desc,
                        US_Congressional_District,
                        County,
                        Precinct
                        from fl where County in
                        ('BAY', 'CALHOUN', 'FRANKLIN', 'GADSDEN', 'GULF', 'JACKSON',
                         'LIBERTY', 'WASHINGTON',
                         'WALTON', 'HOLMES', 'WAKULLA', 'LEON')"))
## get real race gender from file
db2 <- dbConnect(SQLite(), "D:/rolls.db")
fl_race <- dbGetQuery(db2, "select Race, Voter_ID, Gender from fl_roll_201902")
dbDisconnect(db2)
rm(db2)
####
fl_voters <- inner_join(fl_voters, fl_race, by = c("Voters_StateVoterID" = "Voter_ID")) %>% 
  mutate(GEOID = paste0("12", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup),
         white = Race == 5,
         black = Race == 3,
         latino = Race == 4,
         asian = Race == 2,
         female = Gender == "F",
         male = Gender == "M",
         dem = Parties_Description == "Democratic",
         rep = Parties_Description == "Republican",
         county = substring(County, 1, 3)) %>% 
  rename(age = Voters_Age) %>% 
  select(-County)
rm(fl_race)
#############
neighbor_voters <- fl_voters %>% 
  mutate(countypct = paste0(county, Precinct)) %>% 
  filter(countypct %in% c(neighbors$src_countypct, neighbors$nbr_countypct))

neighbor_voters$treated <- neighbor_voters$county %in% treated_counties

######

census_data <- rbindlist(lapply(c(treated_countiesb, control_countiesb), function(c){
  j <- get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
})) %>% 
  dplyr::select(GEOID, median_income, some_college)
neighbor_voters <- left_join(neighbor_voters, census_data)

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
#     genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, ties = F, cluster = cl)
#     
#     mout <- Match(Tr = Tr, X = X, Weight.matrix = genout, ties = F)
#     
#     matches <- data.frame(treated = mout$index.treated,
#                           control = mout$index.control)
#     
#     samp$ids <- c(1:nrow(samp))
#     
#     matches <- left_join(matches,
#                          dplyr::select(samp, ids, LALVOTERID),
#                          by = c("treated" = "ids")) %>%
#       dplyr::select(-treated) %>%
#       rename(treated = LALVOTERID)
#     
#     matches <- left_join(matches,
#                          dplyr::select(samp, ids, LALVOTERID),
#                          by = c("control" = "ids")) %>%
#       dplyr::select(-control) %>%
#       rename(control = LALVOTERID)
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

matches <- bind_rows(matches %>% 
                       mutate(treatment = F),
                     data.table(treated = matches$treated,
                                control = matches$treated,
                                treatment = T))
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
                                   from fl_history_type")
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

m1 <- glm(voted ~ treatment*d18, family = "binomial", data = matches)

m2 <- glm(voted ~ treatment*d18 +
            white + black + latino + asian +
            female + male + dem + rep + age +
            median_income + some_college, data = matches, family = "binomial")

save(m1, m2, file = "./temp/ll_dind.rdata")

##############
ll <- matches %>% 
  group_by(treatment, year) %>% 
  summarize(total_votes = sum(voted),
            voted = mean(voted),
            absentee = sum(absentee),
            early = sum(early),
            polls = sum(polls)) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(treatment, "Treated Group", "Control Group")) %>% 
  mutate_at(vars(absentee, early, polls), ~ . / total_votes)

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
