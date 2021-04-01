## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
set.seed(25987)

on_nyu <- T

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(caret)
  library(tidyverse)
  
  library(ebal)
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}



fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate_at(vars(starts_with("v201")), factor)


Tr <- fl_voters$treated

fl_voters <- cbind(fl_voters, predict(dummyVars(~v2010, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2012, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2014, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2016, data = fl_voters), newdata = fl_voters))
# 
# X = fl_voters %>%
#   dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
#                 median_income, some_college, starts_with("v201")) %>% 
#   select(-v2010, -v2012, -v2014, -v2016, -v2018, -ends_with(".1"))

# mb <- ebalance(Treatment = fl_voters$treated, X = X)
# 
# saveRDS(mb, "temp/ebal_out.rds")

###############################################

mb <- readRDS("temp/ebal_out.rds")

fl_voters <- bind_rows(
  filter(fl_voters, treated) %>% 
    mutate(weight = 1),
  filter(fl_voters, !treated) %>% 
    mutate(weight = mb$w)
)

##############################

balance <- MatchBalance(treated ~ white + black + latino + asian +
                          female + male + dem + rep + age +
                          median_income + some_college,
                        data = fl_voters, weights = fl_voters$weight)

saveRDS(balance, "temp/ebal_balance_table.rds")
