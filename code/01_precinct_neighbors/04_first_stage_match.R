
buffer <- readOGR("./temp", "buffer_shape")
####
neighbor_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(treated | neighbor_county)

pings  <- SpatialPoints(neighbor_voters[c('Residence_Addresses_Longitude',
                                          'Residence_Addresses_Latitude')],
                        proj4string = buffer@proj4string)

neighbor_voters$buffer <- over(pings, buffer)$OBJECTID

neighbor_voters <- filter(neighbor_voters, !is.na(buffer)) %>% 
  select(-buffer, -neighbor_county) %>% 
  rename(lat = Residence_Addresses_Latitude,
         lon = Residence_Addresses_Longitude)

saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
neighbor_voters <- readRDS("./temp/neighbor_voters.rds")

#####
# source("./code/misc/AutoCluster4.R")
# cl <- NCPUS(detectCores() - 1)

neighbor_voters <- neighbor_voters[complete.cases(neighbor_voters), ]

Tr <- neighbor_voters$treated

X = neighbor_voters %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college, lat, lon)

ids <- neighbor_voters %>%
  mutate(id = row_number()) %>%
  select(id, LALVOTERID)

# genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, cluster = cl)
# save(genout, file = "./temp/neighbors_genout.rdata")
load("./temp/neighbors_genout.rdata")

mout <- Match(Tr = Tr, X = X, Weight.matrix = genout)
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
