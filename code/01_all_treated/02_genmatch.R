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
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
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
                median_income, some_college)

genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, ties = F, cluster = cl)

saveRDS(genout, "./temp/genout_hurricane.rds")
