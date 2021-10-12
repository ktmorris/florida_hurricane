library(stargazer)
library(data.table)
library(miceadds)
library(Matching)
library(data.table)
library(multcomp)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(caret)
library(biglm)
library(fixest)
library(tidyverse)


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

####################################

fl_voters$yint <- as.integer(fl_voters$year)

f1 <- voted ~ treated_18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date | c2 + year

f2 <- voted ~ treated_18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date +
  i(c2, yint) | c2 + year


m1 <- feols(fml = f1, data = fl_voters, cluster = c("LALVOTERID", "c2"))
saveRDS(m1, "temp/unmatched_full_ame.rds")
rm(m1)
gc()
m2 <- feols(fml = f2, data = fl_voters, cluster = c("LALVOTERID", "c2"))
saveRDS(m2, "temp/unmatched_full_ame_cyint.rds")
rm(m2)
gc()
m3 <- feols(fml = f1, data = fl_voters, weights = ~ weight_ebal, cluster = c("LALVOTERID", "c2"))
saveRDS(m3, "temp/ebalance_reg.rds")

