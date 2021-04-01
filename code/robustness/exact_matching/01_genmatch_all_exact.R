## this can be run locally or on NYU's HPC. Set option in next step
## option allowed because of how long GenMatch can take
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
set.seed(25987)

on_nyu <- F

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(tidyverse)
  
  cl<-makeCluster(c(readLines(NodeFile)), type="SOCK")
  
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}

fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  group_by(treated) %>% 
  sample_frac(0.01) %>% 
  ungroup() %>% 
  filter(!neighbor_county)


Tr <- fl_voters$treated

X = fl_voters %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                v2010, v2012, v2014, v2016)

genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, ties = T, cluster = cl,
                   exact = rep(T, 13))

saveRDS(genout, "./temp/genout_hurricane.rds")
