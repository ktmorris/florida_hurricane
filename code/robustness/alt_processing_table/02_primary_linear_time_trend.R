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


matches <- readRDS("./temp/full_reg_data.rds")

matches$treated_18 <- matches$treated * matches$d18
matches$share_open_18 <- matches$treated * matches$d18 * matches$share_open
matches$rel_18 <- matches$treated * matches$d18 * matches$treated_rel
matches$yint <- as.integer(matches$year)

f2 <- voted ~ treated_18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date +
  i(c2, yint) | c2 + year

m1 <- feols(fml = f2, data = matches, cluster = c("voter", "c2", "group"), weight = ~ weight)


saveRDS(m1, file = "temp/ame_clin.rds")
