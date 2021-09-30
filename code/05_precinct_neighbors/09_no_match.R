

pan <- readRDS("./temp/neighbor_voters.rds") %>%
  select(-v18, -GEOID, -Precinct) %>%
  mutate(panhandle = T)


fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(!neighbor_county, !treated) %>%
  select(-v18, -neighbor_county, -GEOID, -Precinct) %>%
  rename(lat = latitude, lon = longitude) %>%
  mutate(panhandle = F,
         treated = F)

fl_voters <- bind_rows(fl_voters, pan)


fl_voters <- fl_voters %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted") %>% 
  mutate(treated_18 = treated * (year == "2018"),
         panhandle_18 = panhandle * (year == "2018"))

mov <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(change = (actual_dist - expected_dist) / 1000) %>% 
  select(LALVOTERID, change)

fl_voters <- left_join(fl_voters, mov) %>% 
  mutate(treated_change = ifelse(is.na(change), 0, change),
         treated_rel = rel)

combine <- readRDS("temp/trip_diff_reg_data.rds")

f1 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + county*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + county*treated_change + year*treated_change

f2 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + county*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + county*treated_change + year*treated_change +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f3 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + county*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + county*treated_change + year*treated_change +
  county*as.integer(year)

f4 <- voted ~ treated_18*treated_rel + panhandle_18*treated_rel + county*treated_rel + year*treated_rel +
  treated_18*treated_change + panhandle_18*treated_change + county*treated_change + year*treated_change +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college +
  county*as.integer(year)

models1 <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  m <- lm(f, fl_voters)
})

ses_1 <- list(
  summary(lm.cluster(formula = f1, data = fl_voters, cluster = fl_voters$LALVOTERID))[ , 2],
  summary(lm.cluster(formula = f2, data = fl_voters, cluster = fl_voters$LALVOTERID))[ , 2],
  summary(lm.cluster(formula = f3, data = fl_voters, cluster = fl_voters$LALVOTERID))[ , 2],
  summary(lm.cluster(formula = f4, data = fl_voters, cluster = fl_voters$LALVOTERID))[ , 2]
)

models2 <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  m <- lm(f, combine)
})

ses_2 <- list(
  summary(lm.cluster(formula = f1, data = combine, cluster = combine$group_id))[ , 2],
  summary(lm.cluster(formula = f2, data = combine, cluster = combine$group_id))[ , 2],
  summary(lm.cluster(formula = f3, data = combine, cluster = combine$group_id))[ , 2],
  summary(lm.cluster(formula = f4, data = combine, cluster = combine$group_id))[ , 2]
)

stargazer(models1, models2,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Administrative Treatment $\\times$ 2018",
                               "Weather Treatment $\\times$ 2018",
                               "Administrative Treatment $\\times$ 2018 $\\times$ Change in Distance to Closest Polling Place",
                               "Administrative Treatment $\\times$ 2018 $\\times$ Relative Rainfall"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:alt-specs-trip} Alternative Processing Approaches",
          column.labels = c("Unmatched", "Matched"),
          column.separate = c(4, 4),
          table.placement = "h",
          se = append(ses_1, ses_2),
          omit.stat = c("f", "ser", "aic"),
          order = c("^treated_18$", "^panhandle_18$", "treated_18"),
          keep = c("^treated_18$", "^panhandle_18$", "treated_18"),
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/trip_dif_rob.tex",
          out.header = F,
          notes = "TO REPLACE",
          add.lines=list(c("County Fixed Effects" , rep(c("X", "X", "X", "X"), 2)),
                         c("Year Fixed Effects", rep(c("X", "X", "X", "X"), 2)),
                         c("Includes Matched Covariates" , rep(c("", "X", "", "X"), 2)),
                         c("County-Linear Time Trends" , rep(c("", "", "X", "X"), 2))))

#######################################

ll <- fl_voters %>% 
  mutate(county = ifelse(treated, county,
                         ifelse(panhandle, "Weather Only", "Control"))) %>% 
  group_by(county, year) %>% 
  summarize(voted = mean(voted)) %>% 
  mutate(year = as.integer(year))


ll <- rbindlist(lapply(c("BAY", "GAD", "JAC", "LIB", "WAS"), function(c){
  
  j <- filter(ll, county %in% c(c, "Weather Only", "Control")) %>% 
    mutate(group = ifelse(county == c, "Weather + Admin", county),
           county = c)
})) %>% 
  mutate(type = "Unprocessed")

ll$group <- factor(ll$group, levels = c("Weather + Admin",
                                        "Weather Only",
                                        "Control"))

ll2 <- readRDS("temp/county_level_plot_trip.rds") %>% 
  mutate(type = "Post-Matching",
         year = as.integer(year)) %>% 
  select(-fac) %>% 
  rename(county = treated_county)


ll3 <- bind_rows(ll, ll2)

ll3$type <- factor(ll3$type, levels = c("Unprocessed", "Post-Matching"))

pf <- ggplot(ll3, aes(x = year, y = voted, linetype = group)) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  facet_grid(county~type)

saveRDS(pf, "temp/triple_dif_plotted_all.rds")
