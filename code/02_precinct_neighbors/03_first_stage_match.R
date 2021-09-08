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
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}


# buffer <- readOGR("./temp", "buffer")
# ####
# neighbor_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
#   filter(treated | neighbor_county)
# 
# pings  <- SpatialPoints(neighbor_voters[c("longitude",
#                                           "latitude")],
#                         proj4string = buffer@proj4string)
# 
# neighbor_voters$buffer <- over(pings, buffer)$treated
# 
# neighbor_voters <- filter(neighbor_voters, !is.na(buffer)) %>%
#   select(-buffer, -neighbor_county) %>%
#   rename(lat = latitude,
#          lon = longitude)
# 
# saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
neighbor_voters <- readRDS("./temp/neighbor_voters.rds")

#####
# source("./code/misc/AutoCluster4.R")
# cl <- NCPUS(detectCores() - 1)

neighbor_voters <- neighbor_voters[complete.cases(neighbor_voters), ] %>% 
  mutate_at(vars(starts_with("v201")), factor)


Tr <- neighbor_voters$treated

neighbor_voters <- cbind(neighbor_voters, predict(dummyVars(~v2010, data = neighbor_voters), newdata = neighbor_voters))
neighbor_voters <- cbind(neighbor_voters, predict(dummyVars(~v2012, data = neighbor_voters), newdata = neighbor_voters))
neighbor_voters <- cbind(neighbor_voters, predict(dummyVars(~v2014, data = neighbor_voters), newdata = neighbor_voters))
neighbor_voters <- cbind(neighbor_voters, predict(dummyVars(~v2016, data = neighbor_voters), newdata = neighbor_voters))

X = neighbor_voters %>%
  dplyr::select(rel, lat, lon, starts_with("v201")) %>% 
  select(-v2010, -v2012, -v2014, -v2016, -v2018, -ends_with(".1")) %>% 
  mutate(east = as.integer(lon > -85))

ids <- neighbor_voters %>%
  mutate(id = row_number()) %>%
  select(id, LALVOTERID)

genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, cluster = cl, ties = F,
                   exact = c(rep(F, 15), T))
save(genout, file = "./temp/neighbors_genout.rdata")
load("./temp/neighbors_genout.rdata")

mout <- Match(Tr = Tr, X = X, Weight.matrix = genout, ties = F)
save(mout, file = "./temp/mout_neighbors_first.rdata")
load("./temp/mout_neighbors_first.rdata")

matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
                      control = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("treated" = "id")) %>%
  select(-treated) %>%
  rename(treated = LALVOTERID)

matches <- left_join(matches, ids, by = c("control" = "id")) %>%
  select(-control) %>%
  rename(control = LALVOTERID)

saveRDS(matches, "./temp/matches_first_stage.rds")

#################################

saveRDS(matches$treated, "./temp/treated_neighbors.rds")
saveRDS(matches$control, "./temp/control_neighbors.rds")

matches <- matches %>% 
  rename(group_id = treated,
         voter = control)

saveRDS(matches, "./temp/neighbor_matches_weights.rds")
