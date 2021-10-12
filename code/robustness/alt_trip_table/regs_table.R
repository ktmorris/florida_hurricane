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
  library(modelsummary)
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}
options("modelsummary_format_numeric_latex" = "plain")

m1f <- readRDS("temp/big_trip_primary.rds")
m2f <- readRDS("temp/big_trip_cyint.rds")
m3f <- readRDS("temp/big_trip_covs.rds")
m4f <- readRDS("temp/big_trip_cyint_covs.rds")

load("temp/trip_diff_rob_matched.rdata")

rows <- tribble(~term,          ~m1,  ~m2, ~m3, ~m4, ~m5, ~m6, ~m7, ~m8,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Rainfall and Interactions", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Changed Distance to Polling Place and Interactions", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Matched Covariates", "", "", "$\\checkmark$", "$\\checkmark$", "", "", "$\\checkmark$", "$\\checkmark$",
                "County Linear Time Trends", "", "$\\checkmark$", "", "$\\checkmark$", "", "$\\checkmark$", "", "$\\checkmark$",
                "Cluster Level:", "IC", "IC", "IC", "IC", "IGC", "IGC", "IGC", "IGC")

attr(rows, 'position') <- c(17:23)

models <- list("Unprocessed" = m1f,
               "Unprocessed" = m2f,
               "Unprocessed" = m3f,
               "Unprocessed" = m4f,
               "Matched" = models[[1]],
               "Matched" = models[[2]],
               "Matched" = models[[3]],
               "Matched" = models[[4]])

modelsummary(models,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Administrative Treatment $\\times$ 2018",
                          "panhandle_18" = "Weather Treatment $\\times$ 2018",
                          "treated_county::GAD:treated_18" = "Gadsden Administrative Treatment $\\times$ 2018",
                          "treated_county::JAC:treated_18" = "Jackson Administrative Treatment $\\times$ 2018",
                          "treated_county::LIB:treated_18" = "Liberty Administrative Treatment $\\times$ 2018",
                          "treated_county::WAS:treated_18" = "Washington Administrative Treatment $\\times$ 2018",
                          "treated_18:treated_change" = "Administrative Treatment $\\times$ 2018 $\\times$ Change in Distance to Closest Polling Place",
                          "treated_18:treated_rel" = "Administrative Treatment $\\times$ 2018 $\\times$ Relative Rainfall",
                          "(Intercept)" = "Intercept"),
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:trip-diff-rob} Turnout, 2010 --- 2018",
             latex_options = "scale_down",
             add_rows = rows,
             output = "temp/trip_dif_rob.tex",
             escape = FALSE)

j <- fread("./temp/trip_dif_rob.tex", header = F, sep = "+") %>% 
  mutate(n = row_number())

insert1 <- "\\resizebox{1\\linewidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)

write.table(j, "./temp/trip_dif_rob.tex", quote = F, col.names = F,
            row.names = F)
