##this fails with 100GB of RAM. Runs successfully with 150

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


fl_voters <- readRDS("./temp/pre_match_full_voters.rds")

fl_voters <- fl_voters %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted") %>% 
  mutate(treated_18 = treated * (year == "2018"),
         panhandle_18 = (treated | neighbor_county) * (year == "2018"))


####################################

f1 <- voted ~ treated_18 + panhandle_18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date | c2 + year

m1 <- feols(fml = f1, data = fl_voters, cluster = c("LALVOTERID", "c2"))

rows <- tribble(~term,          ~m1,
                "Year Fixed Effects", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$",
                "Matched Covariates", "$\\checkmark$",)

attr(rows, 'position') <- c(5:7)

modelsummary(m1,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Administrative Treatment $\\times$ 2018",
                          "panhandle_18" = "Weather Treatment $\\times$ 2018",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:all-voters} Turnout, 2010 --- 2018",
             notes = "Robust standard errors clustered at county and individual level.",
             add_rows = rows,
             latex_options = "scale_down",
             output = "temp/all_voters.tex",
             escape = FALSE)
