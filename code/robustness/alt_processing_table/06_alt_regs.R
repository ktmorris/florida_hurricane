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
options("modelsummary_format_numeric_latex" = "plain")

m1 <- readRDS("temp/unmatched_full_ame.rds")
m2 <- readRDS("temp/unmatched_full_ame_cyint.rds")
m3 <- readRDS("temp/ame_clin.rds")
m4 <- readRDS("temp/ebalance_reg.rds")
m5 <- readRDS("temp/pscore_reg.rds")
m6 <- readRDS("temp/exact_regs.rds")

rows <- tribble(~term,          ~m1,  ~m2, ~m3, ~m4, ~m5, ~m6,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Matched Covariates", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Linear Time Trends", "", "$\\checkmark$", "$\\checkmark$", "", "", "",
                "Cluster Level:", "IC", "IC", "IGC", "IC", "IGC", "IGC")

attr(rows, 'position') <- c(3:7)

models <- list("Unprocessed" = m1,
               "Unprocessed" = m2,
               "Primary Model" = m3,
               "Entropy Balancing" = m4,
               "Propensity Score" = m5,
               "Exact Match" = m6)

modelsummary(models,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Full Treatment $\\times$ 2018",
                          "treated_18:treated_rel" = "Full Treatment $\\times$ 2018 $\\times$ Relative Rainfall",
                          "treated_18:treated_change" = "Full Treatment $\\times$ 2018 $\\times$ Change in Distance to Closest Polling Place",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             add_rows = rows,
             title = "\\label{tab:alt-specs} Alternative Processing Approaches",
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             output = "temp/rob_regs.tex",
             escape = FALSE)