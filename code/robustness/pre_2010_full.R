options("modelsummary_format_numeric_latex" = "plain")

matches <- readRDS("./temp/full_reg_data.rds")

d <- matches %>% 
  filter(treated, year == 2018) %>% 
  dplyr::select(group, rdt = reg_date)

matches <- left_join(matches, d) %>% 
  filter(rdt > -305)

matches$treated_18 <- matches$treated * matches$d18
matches$share_open_18 <- matches$treated * matches$d18 * matches$share_open
matches$rel_18 <- matches$treated * matches$d18 * matches$treated_rel

f1 <- voted ~ treated_18 | c2 + year

f2 <- voted ~ treated_18 + 
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date | c2 + year

f3 <- voted ~ treated_18 +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + diff + reg_date | c2 + year

f4 <- voted ~ treated_18*treated_rel |
  c2[treated_rel] + year[treated_rel]

f5 <- voted ~ treated_18*treated_rel +
  treated_18*treated_change |
  c2[treated_rel, treated_change] + year[treated_rel, treated_change]

models <- lapply(c(f1, f2, f3, f4, f5), function(f){
  print(f)
  if(f[[3]][[3]] == f1[[3]][[3]] | f[[3]][[3]] == f2[[3]][[3]] | f[[3]][[3]] == f3[[3]][[3]]){
    print("a")
    feols(fml = f, data = matches, weights = ~ weight, cluster = c("group", "c2", "voter"))
  }else{
    print("b")
    feols(fml = f, data = matches, weights = ~ weight, cluster = c("group", "voter", "c2"))
  }
})

rows <- tribble(~term,          ~m1,  ~m2, ~m3, ~m4, ~m5,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Matched Covariates", "", "$\\checkmark$", "$\\checkmark$", "", "",
                "CD Competitiveness", "", "", "$\\checkmark$", "", "",
                "Rainfall and Interactions", "", "", "", "$\\checkmark$", "$\\checkmark$",
                "Changed Distance to Polling Place and Interactions", "", "", "", "", "$\\checkmark$",
                "Cluster Level:", "IGC", "IGC", "IGC", "IGC", "IGC")

attr(rows, 'position') <- c(7:15)

modelsummary(models,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Both Treatments $\\times$ 2018",
                          "treated_18:treated_rel" = "Both Treatments $\\times$ 2018 $\\times$ Relative Rainfall",
                          "treated_18:treated_change" = "Both Treatments $\\times$ 2018 $\\times$ Change in Distance to Closest Polling Place",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             add_rows = rows,
             title = "\\label{tab:full-dind-pre2010} Turnout, 2010 --- 2018",
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             output = "temp/dind_full_pre2010.tex",
             escape = FALSE)

rm(models)
gc()
