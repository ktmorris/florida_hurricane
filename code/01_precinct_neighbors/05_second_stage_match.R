## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- F
if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  setwd("/scratch/km3815/matching")
}

##### neighbor matches
fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  mutate(treated2 = LALVOTERID %in% filter(readRDS("./temp/neighbor_voters.rds"), treated)$LALVOTERID) %>% 
  filter(!neighbor_county, (treated2 | !treated)) %>% 
  select(-neighbor_county, -treated) %>% 
  rename(treated = treated2)

##########

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X = fl_roll %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college)

genout <- readRDS("./temp/genout_hurricane.rds")

mout <- Match(Tr = fl_roll$treated, X = X,
              estimand = "ATT", Weight.matrix = genout, M = 5)

save(mout, file = "./temp/mout_hurricane_border_treated.RData")

load("./temp/mout_hurricane_border_treated.RData")

matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("treated" = "id")) %>% 
  select(-treated) %>% 
  rename(group_id = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>% 
  select(-control) %>% 
  rename(control = LALVOTERID)

saveRDS(matches, "./temp/precinct_treated_matches.rds")


##### neighbor matches
fl_roll <- readRDS("./temp/precinct_treated_matches.rds") %>% 
  filter(!treated) %>%
  mutate(treated = neighbor_county_match) %>%
  select(-neighbor_county_match) %>%
  filter((!neighbor_county | treated))

##########

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X = fl_roll %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college)


genout <- readRDS("./temp/genout_hurricane.rds")

mout <- Match(Tr = fl_roll$treated, X = X,
                estimand = "ATT", Weight.matrix = genout, M = 5)

save(mout, file = "./temp/mout_hurricane_border_controls.RData")

load("./temp/mout_hurricane_border_controls.RData")

matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("treated" = "id")) %>% 
  select(-treated) %>% 
  rename(group_id = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>% 
  select(-control) %>% 
  rename(control = LALVOTERID)

saveRDS(matches, "./temp/control_matches.rds")

