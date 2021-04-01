## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take

on_nyu <- T
if(on_nyu){
  library(Matching)
  library(data.table)
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

X = fl_roll %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                v2010, v2012, v2014, v2016)


genout <- readRDS("./temp/genout_hurricane_exacts.rds")

mout <- Matchby(Tr = fl_roll$treated, X = X,
                by = c(X$white, X$black, X$latino, X$asian, X$female, X$male, X$dem, X$rep, X$age,
                       X$v2010, X$v2012, X$v2014, X$v2016), exact = rep(T, 13),
                estimand = "ATT", Weight.matrix = genout, M = 1, ties = T)

save(mout, file = "./temp/mout_hurricane_full_exacts.RData")
