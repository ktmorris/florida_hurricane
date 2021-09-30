
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
library(biglm)
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

fl_voters$c2 <- as.factor(fl_voters$c2)
for(t in unique(fl_voters$county)){
  saveRDS(filter(fl_voters, county == t), paste0("temp/trip_big_", t, ".rds"))
}

####################################

f1 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change

f2 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f3 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  c2*as.integer(year)

f4 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college +
  c2*as.integer(year)


cs <- list.files("temp", pattern = "trip_big*", full.names = T)

for(c in cs){
  print(c)
  dat <- readRDS(c)
  if(c == cs[1]){
    m1 <- biglm(formula = f1, data = dat)
    m2 <- biglm(formula = f2, data = dat)
    m3 <- biglm(formula = f3, data = dat)
    m4 <- biglm(formula = f4, data = dat)
  }else{
    m1 <- update(m1, dat)
    m2 <- update(m2, dat)
    m3 <- update(m3, dat)
    m4 <- update(m4, dat)
  }
}

save(m1, m2, m3, m4, file = "temp/big_trip_lm_data.rdata")

load("temp/big_trip_lm_data.rdata")

