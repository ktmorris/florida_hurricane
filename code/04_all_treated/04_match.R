## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- T
if(on_nyu){
  library(Matching)
  library(caret)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
}



#####
fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate_at(vars(starts_with("v201")), factor)

fl_voters <- cbind(fl_voters, predict(dummyVars(~v2010, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2012, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2014, data = fl_voters), newdata = fl_voters))
fl_voters <- cbind(fl_voters, predict(dummyVars(~v2016, data = fl_voters), newdata = fl_voters))

##########

X = fl_voters %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college, starts_with("v201"), reg_date) %>% 
  select(-v2010, -v2012, -v2014, -v2016, -v2018, -ends_with(".1"))

genout <- readRDS("./temp/genout_hurricane.rds")

mout <- Matchby(Tr = fl_voters$treated, X = X,
              by = c(X$white, X$black, X$latino, X$asian, X$female, X$male, X$dem, X$rep, X$age,
                     X$v2010.2, X$v2010.3, X$v2010.4,
                     X$v2012.2, X$v2012.3, X$v2012.4,
                     X$v2014.2, X$v2014.3, X$v2014.4,
                     X$v2016.2, X$v2016.3, X$v2016.4),
              estimand = "ATT", Weight.matrix = genout, M = 5)

save(mout, file = "./temp/mout_hurricane_full.RData")

##############################
balance <- MatchBalance(treated ~ white + black + latino + asian +
                          female + male + dem + rep + age +
                          median_income + some_college + reg_date,
                        data = fl_voters, match.out = mout)
saveRDS(balance, "./temp/balance_table_full_match.rds")