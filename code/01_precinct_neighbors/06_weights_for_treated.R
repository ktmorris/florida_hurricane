## find weights for treated voters to weight overall administrative effect
weights <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(treated) %>% 
  mutate(treated2 = LALVOTERID %in% readRDS("./temp/neighbor_voters.rds")$LALVOTERID) %>% 
  group_by(county) %>% 
  summarize(treated = sum(treated),
            treated2 = sum(treated2)) %>% 
  mutate(weight = treated / treated2) %>% 
  filter(!is.infinite(weight)) %>% 
  select(county, weight)

saveRDS(weights, "./temp/weights_for_treated.rds")
