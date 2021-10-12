options("modelsummary_format_numeric_latex" = "plain")

matches <- readRDS("./temp/full_reg_data.rds")

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
             coef_map = c("treated_18" = "Full Treatment $\\times$ 2018",
                          "treated_18:treated_rel" = "Full Treatment $\\times$ 2018 $\\times$ Relative Rainfall",
                          "treated_18:treated_change" = "Full Treatment $\\times$ 2018 $\\times$ Change in Distance to Closest Polling Place",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             add_rows = rows,
             title = "\\label{tab:full-dind} Turnout, 2010 --- 2018",
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             output = "temp/dind_full.tex",
             escape = FALSE)

rm(models)
gc()
# #########################################################

f1 <- voted ~ treated_18 + i(treated_county, treated_18, "BAY") |
  treated_county + c2 + year + c2^treated_county + year^treated_county

m <- feols(fml = f1, data = matches, weights = ~ weight, cluster = c("c2", "group", "voter"))

###########################
cints <- rbindlist(lapply(c("CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS"), function(c){
  as.data.table(confint((glht(m, linfct =
                                c(paste0("treated_18 + treated_county::", c, ":treated_18 = 2")))))[["confint"]]) %>% 
    mutate(county = c)
}))

x <- bind_cols(confint(m),
               n = rownames(confint(m))) %>% 
  filter(n == "treated_18") %>% 
  dplyr::select(-n) %>% 
  rename(lwr = `2.5 %`, upr = `97.5 %`) %>% 
  mutate(county = "BAY",
         Estimate = (lwr + upr) / 2)

cints <- bind_rows(cints,
                   x)

saveRDS(cints, "temp/cints_county_ind.rds")
#########################
cints <- readRDS("temp/cints_county_ind.rds")
pps <- fread("raw_data/changes.csv") %>%
  select(c, s = share_open)

pps <- left_join(pps, cints, by = c("c" = "county"))

fff <- filter(matches, treated) %>%
  group_by(county) %>%
  summarize(rel = mean(rel),
            count = n())

pps <- left_join(pps, fff, by = c("c" = "county")) %>%
  rename(est = Estimate)

eq2 <- as.character(as.expression(substitute(paste(italic(R)^2 == r2),
                                             list(r2 = number(summary(lm(est ~ s,
                                                                         filter(pps),
                                                                         weight = count))$r.squared, accuracy = .01)))))

closed_counties <- ggplot(pps, aes(x=s, y=est)) +
  geom_smooth(method = "lm",
              mapping = aes(weight = count)) +
  geom_hline(yintercept = 0) +
  labs(x = "Share of Expected Polling Places that Remained Open",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(0, max(pps$s), 0.2)) +
  scale_y_continuous(labels = percent) +
  geom_pointrange(aes(ymin=lwr, ymax=upr))


closed_counties
save(eq2, closed_counties, file = "temp/closed_counties.rdata")

eq2 <- as.character(as.expression(substitute(paste(italic(R)^2 == r2),
                                             list(r2 = number(summary(lm(est ~ rel,
                                                                         filter(pps),
                                                                         weight = count))$r.squared, accuracy = .01)))))

rain_counties <- ggplot(pps, aes(x=rel, y=est)) +
  geom_smooth(method = "lm",
              mapping = aes(weight = count)) +
  geom_hline(yintercept = 0) +
  labs(x = "Relative Rainfall",
       y = "Estimated Treatment Effect") +
  scale_x_continuous(labels = percent, breaks = seq(min(pps$rel), max(pps$rel), 0.2)) +
  scale_y_continuous(labels = percent) +
  geom_pointrange(aes(ymin=lwr, ymax=upr))


rain_counties
save(eq2, rain_counties, file = "temp/rain_counties.rdata")
#####################################################
rows <- tribble(~term,          ~m1,
                "Year Fixed Effects", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$",
                "Treated County interacted with County and Year FEs", "$\\checkmark$",
                "Cluster Level:", "IGC")
attr(rows, 'position') <- c(17:20)

modelsummary(m,
             statistic = "std.error",
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             coef_map = c("treated_18" = "Treated $\\times$ 2018",
                          "treated_county::CAL:treated_18" = "Treated $\\times$ 2018 $\\times$ Calhoun",
                          "treated_county::FRA:treated_18" = "Treated $\\times$ 2018 $\\times$ Franklin",
                          "treated_county::GAD:treated_18" = "Treated $\\times$ 2018 $\\times$ Gadsden",
                          "treated_county::GUL:treated_18" = "Treated $\\times$ 2018 $\\times$ Gulf",
                          "treated_county::JAC:treated_18" = "Treated $\\times$ 2018 $\\times$ Jackson",
                          "treated_county::LIB:treated_18" = "Treated $\\times$ 2018 $\\times$ Liberty",
                          "treated_county::WAS:treated_18" = "Treated $\\times$ 2018 $\\times$ Washington",
                          "(Intercept)" = "Intercept"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:county-effs} Turnout, 2010 --- 2018",
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             add_rows = rows,
             output = "./temp/county_reg.tex",
             escape = F)

j <- fread("./temp/county_reg.tex", header = F, sep = "+")

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))



j <- bind_rows(j) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  dplyr::select(-n)

write.table(j, "./temp/county_clean_regs.tex", quote = F, col.names = F,
            row.names = F)
