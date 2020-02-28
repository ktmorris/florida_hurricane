
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


saveRDS(neighbor_voters, "./temp/neighbor_voters.rds")
neighbor_voters <- readRDS("./temp/neighbor_voters.rds")

######
# source("./code/misc/AutoCluster4.R")
# cl <- NCPUS(detectCores() - 1)
# 
# for(i in unique(neighbors$src_countypct)){
#   tryCatch({
#     samp <- filter(neighbor_voters, countypct %in% c(filter(neighbors, src_countypct == i)$nbr_countypct, i))
#     samp <- samp[complete.cases(samp), ]
#     Tr <- samp$treated
# 
#     X = samp %>%
#       dplyr::select(white, black, latino, asian, female, male, dem, rep, age,
#                     median_income, some_college)
# 
#     ids <- samp %>%
#       mutate(id = row_number()) %>%
#       select(id, LALVOTERID)
# 
#     genout <- GenMatch(Tr = Tr, X = X, pop.size = 100, cluster = cl)
# 
#     mout <- Match(Tr = Tr, X = X, Weight.matrix = genout)
# 
#     matches <- data.table(treated = c(mout$index.treated, unique(mout$index.treated)),
#                           control = c(mout$index.control, unique(mout$index.treated)),
#                           weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))
# 
#     matches <- left_join(matches, ids, by = c("treated" = "id")) %>%
#       select(-treated) %>%
#       rename(treated = LALVOTERID)
# 
#     matches <- left_join(matches, ids, by = c("control" = "id")) %>%
#       select(-control) %>%
#       rename(control = LALVOTERID)
# 
#     saveRDS(matches, paste0("./temp/matches_", i, ".rds"))
#   }, error = function(e){})
# }

matches <- rbindlist(lapply(unique(neighbors$src_countypct),
                            function(i){
                              if(file.exists(paste0("./temp/matches_", i, ".rds"))){
                                t <- readRDS(paste0("./temp/matches_", i, ".rds"))
                              }else{
                                t <- data.table(x = "")
                              }
                              return(t)}
                            ), fill = T)

saveRDS(matches$treated, "./temp/treated_neighbors.rds")
saveRDS(matches$control, "./temp/control_neighbors.rds")

matches <- matches %>% 
  select(-x) %>% 
  rename(group_id = treated,
         voter = control)

saveRDS(matches, "./temp/neighbor_matches_weights.rds")
