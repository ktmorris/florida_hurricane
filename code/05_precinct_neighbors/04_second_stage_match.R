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
  
}

##### neighbor matches
fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  mutate(treated2 = LALVOTERID %in% readRDS("./temp/neighbor_matches_weights.rds")$voter) %>% 
  filter(treated2 | (!neighbor_county & !treated)) %>% 
  select(-neighbor_county, -treated) %>% 
  rename(treated = treated2)

fl_roll <- fl_roll[complete.cases(fl_roll), ] %>% 
  mutate_at(vars(starts_with("v201")), factor)

fl_roll <- cbind(fl_roll, predict(dummyVars(~v2010, data = fl_roll), newdata = fl_roll))
fl_roll <- cbind(fl_roll, predict(dummyVars(~v2012, data = fl_roll), newdata = fl_roll))
fl_roll <- cbind(fl_roll, predict(dummyVars(~v2014, data = fl_roll), newdata = fl_roll))
fl_roll <- cbind(fl_roll, predict(dummyVars(~v2016, data = fl_roll), newdata = fl_roll))

##########

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X = fl_roll %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college, starts_with("v201"), reg_date) %>% 
  select(-v2010, -v2012, -v2014, -v2016, -v2018, -ends_with(".1"))

genout <- readRDS("./temp/genout_hurricane.rds")

mout <- Match(Tr = fl_roll$treated, X = X,
              estimand = "ATT", Weight.matrix = genout, M = 5, ties = F)

save(mout, file = "./temp/mout_hurricane_second_stage.RData")

load("./temp/mout_hurricane_second_stage.RData")

matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("treated" = "id")) %>%
  select(-treated) %>%
  rename(group_id = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>%
  select(-control) %>%
  rename(voter = LALVOTERID)

saveRDS(matches, "./temp/second_stage_matches.rds")

balance <- MatchBalance(treated ~ white + black + latino + asian +
                          female + male + dem + rep + age +
                          median_income + some_college + reg_date,
                        data = fl_roll, match.out = mout)
saveRDS(balance, "./temp/bt_2s.rds")