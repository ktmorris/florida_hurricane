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
  library(estimatr)
  library(stargazer)
  library(ebal)
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}



fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(!neighbor_county) %>%
  mutate_at(vars(starts_with("v201")), factor)

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

###################

fl_voters_l <- select(fl_voters, white, black, latino, asian,
                      female, male, dem, rep, age, median_income,
                      some_college, LALVOTERID, weight_ebal, treatment = treated,
                      starts_with("V20")) %>%
  pivot_longer(cols = starts_with("V20"), names_to = "year",
               names_prefix = "v", values_to = "to") %>%
  mutate(to = ifelse(to == 1, 0, 1),
         d18 = year == "2018")

f2 <- to ~ treatment*d18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

m1 <- lm(f2, fl_voters_l)
m2 <- lm(f2, fl_voters_l, weight = weight_ebal)


#####################

mout <- readRDS("temp/prop_out.rds")

matches <- data.table(group = c(mout$index.treated, mout$index.treated),
                      voter = c(mout$index.control, mout$index.treated),
                      weight = c(mout$weights, mout$weights))

matches <- left_join(matches, ids, by = c("voter" = "id")) %>%
  select(-voter) %>%
  rename(voter = LALVOTERID)

matches <- left_join(matches, ids, by = c("group" = "id")) %>%
  select(-group) %>%
  rename(group = LALVOTERID)

history <- select(filter(fl_voters, LALVOTERID %in% matches$voter), LALVOTERID, starts_with("v201")) %>%
  pivot_longer(cols = starts_with("v"),
               names_to = "year", values_to = "to") %>%
  mutate(to = to != 1)

matches <- full_join(matches, history, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, fl_voters, by = c("voter" = "LALVOTERID"))

matches$year <- as.integer(gsub("v", "", matches$year))
matches$treatment <- matches$treated
matches$d18 <- matches$year == 2018

m3 <- lm(f2, matches, weight = weight)

#####################

matches <- readRDS("temp/full_huricane_matches_exacts.rds")

history <- select(filter(fl_voters, LALVOTERID %in% matches$voter), LALVOTERID, starts_with("v201")) %>%
  pivot_longer(cols = starts_with("v"),
               names_to = "year", values_to = "to") %>%
  mutate(to = to != 1)

matches <- full_join(matches, history, by = c("voter" = "LALVOTERID"))
matches <- left_join(matches, fl_voters, by = c("voter" = "LALVOTERID"))

matches$year <- as.integer(gsub("v", "", matches$year))
matches$treatment <- matches$treated
matches$d18 <- matches$year == 2018

m4 <- lm_robust(f2, matches, weight = weight)

m5 <- load("temp/county_lin.RData")

stargazer(m,
          type = "text",
          omit = c("white", "black", "latino", "asian", "female", "male",
                   "dem", "rep", "age", "median_income", "some_college",
                   "diff"),
          omit.stat = c("f", "ser", "aic"),
          table.layout = "-cmd#-t-a-s-n",
          # covariate.labels = c("Treated", "2018",
          #                      "Treated $\\times$ 2018"),
          add.lines=list(c("Includes Matched Covariates" , "X", "X", "X", "X")),
          column.labels = c("Unprocessed", "Entropy Balancing", "Propensity Score", "Exact Match"),
          dep.var.labels = "")
