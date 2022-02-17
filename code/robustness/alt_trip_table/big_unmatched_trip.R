
library(stargazer)
library(data.table)
library(miceadds)
library(Matching)
library(data.table)
library(multcomp)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(caret)
library(fixest)
library(tidyverse)

pan <- readRDS("./temp/neighbor_voters.rds") %>%
  mutate(c2 = substring(GEOID, 3, 5)) %>% 
  select(-v18, -GEOID, -Precinct) %>%
  mutate(panhandle = T)


fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(!neighbor_county, !treated) %>%
  select(-v18, -neighbor_county, -GEOID, -Precinct) %>%
  rename(lat = latitude, lon = longitude) %>%
  mutate(panhandle = F,
         treated = F)

fl_voters <- bind_rows(fl_voters, pan)


fl_voters <- fl_voters %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted") %>% 
  mutate(treated_18 = treated * (year == "2018"),
         panhandle_18 = panhandle * (year == "2018"))

mov <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(change = (actual_dist - expected_dist) / 1000) %>% 
  select(LALVOTERID, change)

fl_voters <- left_join(fl_voters, mov) %>% 
  mutate(treated_change = ifelse(is.na(change), 0, change),
         treated_rel = rel)

fl_voters <- left_join(fl_voters %>%
                         select(-county),
                       filter(fread("raw_data/fips_codes.csv"), state == "FL") %>%
                         select(county_code, county) %>% 
                         mutate(county_code = str_pad(county_code, width = 3, side = "left", pad = "0")),
                       by = c("c2" = "county_code")) %>%
  mutate(county = ifelse(treated, toupper(substring(county, 1, 3)), county))


fl_voters$treated_county <- fl_voters$county
fl_voters$yint <- as.integer(fl_voters$year)
####################################


f1 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, treated_18, "BAY") |
  treated_county[treated_change, treated_rel] + year[treated_rel]

f2 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, treated_18, "BAY") |
  treated_county[treated_change, treated_rel, yint] + year[treated_rel]

f3 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, treated_18, "BAY") +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date |
  treated_county[treated_change, treated_rel] + year[treated_rel]

f4 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, treated_18, "BAY") +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date |
  treated_county[treated_change, treated_rel, yint] + year[treated_rel]


m <- feols(fml = f1, data = fl_voters, cluster = c("LALVOTERID", "c2"))
summary(m)
saveRDS(m, "temp/big_trip_primary.rds")
rm(m)
gc()

m <- feols(fml = f2, data = fl_voters, cluster = c("LALVOTERID", "c2"))
summary(m)
saveRDS(m, "temp/big_trip_cyint.rds")
rm(m)
gc()

m <- feols(fml = f3, data = fl_voters, cluster = c("LALVOTERID", "c2"))
summary(m)
saveRDS(m, "temp/big_trip_covs.rds")
rm(m)
gc()

m <- feols(fml = f4, data = fl_voters, cluster = c("LALVOTERID", "c2"))
summary(m)
saveRDS(m, "temp/big_trip_cyint_covs.rds")
rm(m)
gc()

