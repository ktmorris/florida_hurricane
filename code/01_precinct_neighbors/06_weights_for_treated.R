## find weights for treated voters to weight overall administrative effect

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

######
fl_voters <- readRDS("./temp/pre_match_full_voters.rds")
  
weights <- fl_voters %>% 
  filter(treated) %>% 
  mutate(countypct = paste0(county, Precinct),
         treated = countypct %in% c(neighbors$src_countypct, neighbors$nbr_countypct)) %>% 
  group_by(county) %>% 
  summarize(treated = sum(treated),
            count = n()) %>% 
  mutate(weight = count / treated) %>% 
  filter(!is.infinite(weight)) %>% 
  select(county, weight)

saveRDS(weights, "./temp/weights_for_treated.rds")
