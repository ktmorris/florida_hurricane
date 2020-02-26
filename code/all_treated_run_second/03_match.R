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



#####
fl_roll <- readRDS("./temp/pre_match_full_voters.rds")

##########

ids <- fl_roll %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

X = fl_voters %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college)


genout <- readRDS("./temp/genout_hurricane.rds")

mout <- Matchby(Tr = fl_roll$treated, X = X,
                estimand = "ATT", Weight.matrix = genout, M = 5)

save(mout, file = "./temp/mout_hurricane_full.RData")