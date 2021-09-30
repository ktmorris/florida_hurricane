fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(!neighbor_county) %>%
  mutate_at(vars(starts_with("v201"), county, c2), factor)

ids <- fl_voters %>%
  mutate(id = row_number()) %>%
  select(id, LALVOTERID)

#######################
mb <- readRDS("temp/ebal_out.rds")

fl_voters <- bind_rows(
  filter(fl_voters, treated) %>%
    mutate(weight_ebal = 1),
  filter(fl_voters, !treated) %>%
    mutate(weight_ebal = mb$w)
)

fl_voters <- fl_voters %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted") %>% 
  mutate(treated_18 = treated * (year == "2018"))

mov <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(change = (actual_dist - expected_dist) / 1000) %>% 
  select(LALVOTERID, change)

fl_voters <- left_join(fl_voters, mov) %>% 
  mutate(treated_change = ifelse(is.na(change), 0, change),
         treated_rel = rel)

for(t in unique(fl_voters$county)){
  saveRDS(filter(fl_voters, county == t), paste0("temp/full_big_", t, ".rds"))
}

####################################

cs <- levels(fl_voters$county)

f1 <- voted ~ treated_18 + c2 + year +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f2 <- voted ~ treated_18 + c2 + year +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college +
  c2*as.integer(year)

for(c in cs){
  dat <- readRDS(paste0("temp/full_big_", c, ".rds"))

  print(c)
  if(c == cs[1]){
    m1 <- biglm(formula = f1, data = dat)
    m2 <- biglm(formula = f2, data = dat)
  }else{
    m1 <- update(m1, dat)
    m2 <- update(m2, dat)
  }
}

save(m1, m2, file = "temp/big_lm_data_full.rdata")