## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- T
if(on_nyu){
  library(Matching)
  library(data.table)
  library(fixest)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
}
#####
fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(!neighbor_county)

##########

ids <- fl_roll %>%
  mutate(id = row_number()) %>%
  select(id, LALVOTERID)
# 
# X = fl_roll %>%
#   dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
#                 v2010, v2012, v2014, v2016)
# 
# 
# mout <- Matchby(Tr = fl_roll$treated, X = X,
#                 by = c(X$white, X$black, X$latino, X$asian, X$female, X$male, X$dem, X$rep, X$age,
#                        X$v2010, X$v2012, X$v2014, X$v2016), exact = rep(T, 13),
#                 estimand = "ATT", M = 5, ties = F)
# 
# save(mout, file = "./temp/mout_hurricane_full_exacts.RData")
# 
# load("./temp/mout_hurricane_full_exacts.RData")
# 
# matches <- data.table(voter = c(mout$index.control,
#                                 mout$index.treated),
#                       group = rep(mout$index.treated, 2),
#                       weight = rep(mout$weights, 2)) %>%
#   group_by(voter, group) %>%
#   summarize(weight = sum(weight)) %>%
#   ungroup()
# 
# matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
#   select(-voter) %>%
#   rename(voter = LALVOTERID)
# 
# matches <- left_join(matches, ids, by = c("group" = "id")) %>%
#   select(-group) %>%
#   rename(group = LALVOTERID)
# 
# saveRDS(matches, "temp/full_huricane_matches_exacts.rds")
# 
# ##############################################################
matches <- readRDS("temp/full_huricane_matches_exacts.rds")
matches <- left_join(matches, fl_roll, by = c("voter" = "LALVOTERID"))

matches <- matches %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted") %>% 
  mutate(treated_18 = treated * (year == "2018"))


f1 <- voted ~ treated_18 + 
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date | c2 + year

m1 <- feols(fml = f1, data = matches, weights = ~ weight, cluster = c("voter", "c2", "group"))

saveRDS(m1, "temp/exact_regs.rds")
