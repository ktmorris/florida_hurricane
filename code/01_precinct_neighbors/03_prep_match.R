
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
                        from fl"))
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
########

fl_voters$treated <- fl_voters$county %in% c("BAY", "CAL", "FRA",
                                             "GAD", "GUL", "JAC", "LIB", "WAS")

#######

## downloading census data works better county-by-county
## commenting out because it takes so long
# census_data <- rbindlist(lapply(filter(fips_codes, state == "FL")$county_code, function(c){
#   get_basic_census_stats(geo = "block group", year = 2018, state = "FL", county = c)
# }))
# saveRDS(census_data, "./temp/block_group_census_data.RDS")

census_data <- readRDS("./temp/block_group_census_data.RDS") %>% 
  select(median_income, some_college, GEOID)

fl_voters <- left_join(fl_voters, census_data)

fl_voters <- fl_voters[complete.cases(fl_voters), ]

fl_voters <- fl_voters %>% 
  mutate(neighbor_county = county %in% c("WAL", "HOL", "WAK", "LEO"))

saveRDS(fl_voters, "./temp/pre_match_full_voters.rds")
