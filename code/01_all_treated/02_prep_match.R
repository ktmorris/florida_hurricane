
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
                        Precinct,
                        Residence_Addresses_Latitude,
                        Residence_Addresses_Longitude,
                        Voters_OfficialRegDate
                        from fl"))
# get real race gender from file
db2 <- dbConnect(SQLite(), "E:/rolls.db")
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
cleanup("fl_voters")
########

fl_voters$treated <- fl_voters$county %in% c("BAY", "CAL", "FRA",
                                             "GAD", "GUL", "JAC", "LIB", "WAS")

#######

census_data <- readRDS("../regular_data/census_bgs_18.rds") %>%
  select(median_income, some_college, GEOID)

fl_voters <- left_join(fl_voters, census_data)

fl_voters <- fl_voters %>%
  mutate(neighbor_county = county %in% c("WAL", "HOL", "WAK", "LEO"))

##############

history <- dbConnect(SQLite(), "E:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   BallotType_General_2018_11_06,
                                   BallotType_General_2016_11_08,
                                   BallotType_General_2014_11_04,
                                   BallotType_General_2012_11_06,
                                   BallotType_General_2010_11_02
                                   from florida_history_type") %>% 
  mutate_at(vars(starts_with("Ballot")), ~ ifelse(. == "", 1,
                                                  ifelse(. == "Absentee", 2,
                                                         ifelse(. == "Early", 3, 4)))) %>% 
  rename(v2018 = BallotType_General_2018_11_06,
         v2016 = BallotType_General_2016_11_08,
         v2014 = BallotType_General_2014_11_04,
         v2012 = BallotType_General_2012_11_06,
         v2010 = BallotType_General_2010_11_02)

##############

fl_voters <- left_join(fl_voters, fl_history)
fl_voters$v18 <- fl_voters$v2018 != 1

fl_voters <- left_join(fl_voters, readRDS("temp/voter_rainfall.rds") %>% 
                         select(LALVOTERID, rel))

fl_voters <- fl_voters[complete.cases(fl_voters), ]

fl_voters <- fl_voters %>% 
  rename(latitude = Residence_Addresses_Latitude,
         longitude = Residence_Addresses_Longitude) %>% 
  select(-Voters_StateVoterID,
         -Voters_FIPS,
         -starts_with("Residence_Add"),
         -Voters_Gender,
         -Parties_Description,
         -EthnicGroups_EthnicGroup1Desc,
         -Voters_OfficialRegDate,
         -Race,
         -Gender)

saveRDS(fl_voters, "./temp/pre_match_full_voters.rds")
