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
                median_income, some_college, starts_with("v201")) %>% 
  select(-v2010, -v2012, -v2014, -v2016, -v2018, -ends_with(".1"))


pscore.glm<-glm(treated ~ white + black + latino + asian + female + male + dem +
                  rep + age + median_income + some_college + 
                  v2010.2 + v2012.2 + v2014.2 + v2016.2 +
                  v2010.3 + v2012.3 + v2014.3 + v2016.4 +
                  v2010.4 + v2012.4 + v2014.4 + v2016.3, 
                family=binomial(logit), data=fl_voters)

D <- fl_voters$treated
Y <- fl_voters$v18

X  <- fitted(pscore.glm)

r1  <- Match(Y=Y, Tr=D, X=X, M=5)

saveRDS(r1, "temp/prop_out.rds")

balance <- MatchBalance(treated ~ white + black + latino + asian +
                          female + male + dem + rep + age +
                          median_income + some_college,
                        data = fl_voters, match.out = mout)
saveRDS(balance, "./temp/balance_table_prop_score.rds")