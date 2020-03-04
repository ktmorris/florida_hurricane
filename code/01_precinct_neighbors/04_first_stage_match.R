
treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")
####

neighbors <- fread("./temp/neighbors.txt") %>% 
  filter(src_county %in% treated_counties,
         nbr_county %in% control_counties,
         (src_county != "LIB" | nbr_county != "LEO")) %>% ## uncontested, different parties
  mutate(src_countypct = paste0(src_county, as.integer(src_pct)),
         nbr_countypct = paste0(nbr_county, as.integer(nbr_pct))) %>% 
  select(src_countypct,
         nbr_countypct)
####
neighbor_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  mutate(countypct = paste0(county, Precinct)) %>% 
  filter(countypct %in% c(neighbors$src_countypct, neighbors$nbr_countypct))

neighbor_voters$treated <- neighbor_voters$county %in% treated_counties

neighbor_voters <- neighbor_voters %>% 
  rename(lat = Residence_Addresses_Latitude,
         lon = Residence_Addresses_Longitude)

saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
neighbor_voters <- readRDS("./temp/neighbor_voters.rds")

#####
source("./code/misc/AutoCluster4.R")
cl <- NCPUS(detectCores() - 1)

neighbor_voters <- neighbor_voters[complete.cases(neighbor_voters), ]

Tr <- neighbor_voters$treated

X = neighbor_voters %>%
  dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
                median_income, some_college, lat, lon)

ids <- neighbor_voters %>%
  mutate(id = row_number()) %>%
  select(id, LALVOTERID)

genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, cluster = cl)
save(genout, file = "./temp/neighbors_genout.rdata")
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
