options("modelsummary_format_numeric_latex" = "plain")

combine <- readRDS("temp/trip_diff_reg_data.rds")

d <- combine %>% 
  filter(treated, year == 2018) %>% 
  select(group_id, rdt = reg_date)

combine <- left_join(combine, d) %>% 
  filter(rdt > -305)

combine$treated_county <- factor(combine$treated_county, sort(unique(combine$treated_county)))

f1 <- voted ~ panhandle_18 + treated_18 | c2 + year

f2 <- voted ~ treated_18 + panhandle_18 +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county + c2 + year + treated_county

f3 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change] + year[treated_change] + treated_county[treated_change]

f4 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]


models <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  feols(fml = f, data = combine, weights = ~ weight, cluster = c("group_id", "c2", "voter"))
})

rows <- tribble(~term,          ~m1,  ~m2, ~m3, ~m4,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Rainfall and Interactions", "", "", "", "$\\checkmark$",
                "Changed Distance to Polling Place and Interactions", "", "", "$\\checkmark$", "$\\checkmark$",
                "Cluster Level:", "IGC", "IGC", "IGC", "IGC")

attr(rows, 'position') <- c(17:21)

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
             title = "\\label{tab:trip-diff-pre2010} Turnout, 2010 --- 2018",
             latex_options = "scale_down",
             add_rows = rows,
             output = "temp/trip_dif2_pre2010.tex",
             escape = FALSE)

