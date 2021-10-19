
options("modelsummary_format_numeric_latex" = "plain")


matches <- readRDS("./temp/full_reg_data.rds")
matches$treated_18 <- matches$treated * matches$d18
matches$share_open_18 <- matches$treated * matches$d18 * matches$share_open
matches$rel_18 <- matches$treated * matches$d18 * matches$treated_rel


f1 <- voted ~ treated_18 |
  c2 + year
f2 <- voted ~ treated_18 + i(treated_county, treated_18, "BAY") |
  treated_county + c2 + year + c2^treated_county + year^treated_county
f3 <- voted ~ treated_18 + i(treated_county, treated_18, "BAY") |
  c2 + year
models <- list(
  feols(fml = f1, data = filter(matches, treated_county == "BAY"),
        weights = ~ weight, cluster = c("c2", "group", "voter")),
  feols(fml = f2, data = matches,
        weights = ~ weight, cluster = c("c2", "group", "voter")),
  feols(fml = f3, data = matches,
        weights = ~ weight, cluster = c("c2", "group", "voter"))
)



rows <- tribble(~term,          ~m1,  ~m2, ~m3,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Treated County interacted with County and Year FEs", "", "$\\checkmark$", "",
                "Cluster Level:", "IGC", "IGC", "IGC")


attr(rows, 'position') <- c(17:20)

modelsummary(models,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Both Treatments $\\times$ 2018",
                          "treated_county::CAL:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Calhoun",
                          "treated_county::FRA:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Franklin",
                          "treated_county::GAD:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Gadsden",
                          "treated_county::GUL:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Gulf",
                          "treated_county::JAC:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Jackson",
                          "treated_county::LIB:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Liberty",
                          "treated_county::WAS:treated_18" = "Both Treatments $\\times$ 2018 $\\times$ Washington",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             add_rows = rows,
             title = "Turnout, 2010 --- 2018",
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             output = "./temp/example.tex",
             escape = F)
