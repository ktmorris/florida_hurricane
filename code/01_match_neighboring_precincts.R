
treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")
####

neighbors <- fread("./temp/neighbors.txt") %>% 
  filter(src_county %in% treated_counties,
         nbr_county %in% control_counties) %>% 
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

neighbors_wide <- neighbors %>% 
  group_by(src_countypct) %>% 
  mutate(count = row_number()) %>% 
  pivot_wider(id_cols = "src_countypct",
              values_from = "nbr_countypct",
              names_from = "count",
              names_prefix = "neighbor_",
              values_fill = list(nbr_countypct = "XXX"))
######

census_data <- rbindlist(lapply(c(treated_countiesb, control_countiesb), function(c){
  j <- get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
})) %>% 
  dplyr::select(GEOID, median_income, some_college)
neighbor_voters <- left_join(neighbor_voters, census_data)

saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
######
source("./code/misc/AutoCluster4.R")
cl <- NCPUS(detectCores() - 1)

for(i in c(1:12, 14:16)){
  samp <- filter(neighbor_voters, countypct %in% unlist(c(neighbors_wide[i,])))
  samp <- samp[complete.cases(samp), ]
  Tr <- samp$treated
  
  X = samp %>% 
    dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                  median_income, some_college)
  
  genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, ties = F, cluster = cl)
  
  mout <- Match(Tr = Tr, X = X, Weight.matrix = genout, ties = F)
  
  matches <- data.frame(treated = mout$index.treated,
                        control = mout$index.control)
  
  samp$ids <- c(1:nrow(samp))
  
  matches <- left_join(matches,
                       dplyr::select(samp, ids, LALVOTERID),
                       by = c("treated" = "ids")) %>% 
    dplyr::select(-treated) %>% 
    rename(treated = LALVOTERID)
  
  matches <- left_join(matches,
                       dplyr::select(samp, ids, LALVOTERID),
                       by = c("control" = "ids")) %>% 
    dplyr::select(-control) %>% 
    rename(control = LALVOTERID)
  saveRDS(matches, paste0("./temp/matches_", i, ".rds"))
}

matches <- rbindlist(lapply(c(1:12, 14:16), function(i){readRDS(paste0("./temp/matches_", i, ".rds"))}))

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


matches <- left_join(matches, fl_history, by = c("control" = "LALVOTERID"))
matches$d18 <- matches$year == 2018


m1 <- glm(voted ~ treatment*d18, family = "binomial", data = matches)


ll <- matches %>% 
  group_by(treatment, year) %>% 
  summarize(voted = mean(voted)) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(treatment, "Treated Group", "Control Group"))

ll$treatment <- factor(ll$treatment, levels = c("Treated Group", "Control Group"))

ggplot(ll, aes(x = as.integer(year), y = voted, linetype = treatment)) + geom_line() +
  geom_point() +
  labs(linetype = "Treatment Group",
  x = "Year",
  y = "Turnout") +
  scale_y_continuous(labels = scales::percent)
