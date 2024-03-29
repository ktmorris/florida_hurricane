

first_stage <- readRDS("./temp/neighbor_matches_weights.rds") %>% 
  mutate(treated = group_id == voter,
         panhandle = T,
         secondary_control_1 = F)
######
second_stage <- readRDS("./temp/second_stage_matches.rds") %>% 
  filter(group_id != voter) %>% 
  rename(weight2 = weight)

second_stage <- left_join(second_stage,
                          select(first_stage, voter, weight, treated, gi = group_id), by = c("group_id" = "voter")) %>% 
  mutate(weight = weight * weight2,
         secondary_control_1 = treated,
         panhandle = F,
         treated = F) %>% 
  select(-weight2,
         -group_id) %>% 
  rename(group_id = gi)
######
combine <- bind_rows(first_stage, second_stage)

saveRDS(combine, "./temp/voters_weights_tripdif.rds")
######

fl_roll <- readRDS("./temp/pre_match_full_voters.rds")


history <- select(filter(fl_roll, LALVOTERID %in% combine$voter), LALVOTERID, starts_with("v201")) %>% 
  pivot_longer(cols = starts_with("v"),
               names_to = "year", values_to = "to") %>% 
  mutate(voted = to != 1,
         absentee = to == 2,
         early = to == 3,
         polls = to == 4,
         year = gsub("v", "", year)) %>% 
  select(-to)

###############
results <- fread("./raw_data/11062018Election.txt") %>% 
  filter(RaceCode == "USR") %>% 
  group_by(district = Juris1num, candidate = paste0(CanNameFirst, CanNameLast)) %>% 
  summarize(candidatevotes = sum(CanVotes)) %>% 
  group_by(district) %>% 
  mutate(share = candidatevotes / sum(candidatevotes)) %>% 
  arrange(district, desc(share)) %>% 
  filter(row_number() <= 2) %>% 
  select(district, share) %>% 
  mutate(count = row_number())

results <- pivot_wider(results, id_cols = district, values_from = share,
                       names_from = count, names_prefix = "share") %>% 
  mutate(diff = 1 - (share1 - share2)) %>% 
  select(district, diff) %>% 
  ungroup() %>% 
  mutate(district = as.integer(gsub("District ", "", district)))


######
pps <- fread("raw_data/changes.csv")

mov <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(change = (actual_dist - expected_dist) * 0.000621371) %>% 
  select(voter = LALVOTERID, change)

######
combine <- left_join(combine, history, by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, -treated), by = c("voter" = "LALVOTERID"))
combine <- left_join(combine, select(fl_roll, LALVOTERID, treated_county = county, treated_rel = rel),
                     by = c("group_id" = "LALVOTERID"))
combine <- left_join(combine, select(pps, treated_county = c, share_open))
combine <- left_join(combine, results, by = c("US_Congressional_District" = "district")) %>% 
  mutate(diff = ifelse(is.na(diff), 0, diff))
combine <- left_join(combine, mov) %>% 
  mutate(change = ifelse(is.na(change), 0, change))

combine <- left_join(combine, rename(mov, treated_change = change), by = c("group_id" = "voter")) %>% 
  mutate(change = ifelse(is.na(treated_change), 0, treated_change))


combine$d18 <- combine$year == "2018"
combine$treated_18 <- combine$treated * combine$d18
combine$share_open_18 <- combine$treated * combine$d18 * combine$share_open
combine$rel_18 <- combine$treated * combine$d18 * combine$treated_rel

combine$panhandle_18 <- combine$panhandle * combine$d18
combine$share_open_18_pan <- combine$panhandle * combine$d18 * combine$share_open
combine$rel_18_pan <- combine$panhandle * combine$d18 * combine$treated_rel
combine$rain <- combine$treated_rel
combine$share_open_good <- ifelse(combine$treated_18, combine$share_open, 1)
cleanup("combine")

saveRDS(combine, "temp/trip_diff_reg_data.rds")
cleanup("combine")
combine <- readRDS("temp/trip_diff_reg_data.rds")

combine$treated_county <- factor(combine$treated_county, sort(unique(combine$treated_county)))

f0a <- voted ~ treated_18 | c2 + year

f0b <- voted ~ panhandle_18 | c2 + year

f1 <- voted ~ panhandle_18 + treated_18 | c2 + year

f2 <- voted ~ treated_18*treated_change + panhandle_18*treated_change |
  c2[treated_change] + year[treated_change]

f3 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel |
  c2[treated_change, treated_rel] + year[treated_change, treated_rel]

f4 <- voted ~ treated_18 + panhandle_18 +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county + c2 + year + treated_county

f5 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change] + year[treated_change] + treated_county[treated_change]

f6 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]


models <- lapply(c(f0a, f0b, f1, f2, f3, f4, f5, f6), function(f){
  print(f)
  feols(fml = f, data = combine, weights = ~ weight, cluster = c("group_id", "c2", "voter"))
})

rows <- tribble(~term,          ~m1,  ~m2, ~m3, ~m4, ~m5, ~m6, ~m7, ~m8,
                "Year Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "County Fixed Effects", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                "Changed Distance to Polling Place and Interactions", "", "", "", "$\\checkmark$", "$\\checkmark$", "", "$\\checkmark$", "$\\checkmark$",
                "Rainfall and Interactions", "", "", "", "", "$\\checkmark$", "", "", "$\\checkmark$",
                "Cluster Level:", "IGC", "IGC", "IGC", "IGC", "IGC", "IGC", "IGC", "IGC")

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
                          "panhandle_18:treated_rel" = "Weather Treatment $\\times$ 2018 $\\times$ Relative Rainfall",
                          "(Intercept)" = "Intercept"),
             notes = list("Cluster notation is as follows: I(ndividual); (Matched )G(roup); C(ounty)"),
             gof_omit = 'DF|Deviance|AIC|BIC|Within|Pseudo|Log|Std|FE',
             title = "\\label{tab:trip-diff} Turnout, 2010 --- 2018",
             latex_options = "scale_down",
             add_rows = rows,
             output = "temp/trip_dif2.tex",
             escape = FALSE)

j <- fread("./temp/trip_dif2.tex", header = F, sep = "+") %>% 
  mutate(n = row_number())

insert1 <- "\\resizebox{1\\linewidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)

write.table(j, "./temp/trip_dif2.tex", quote = F, col.names = F,
            row.names = F)



########
ll <- combine %>%
  group_by(panhandle, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>%
  ungroup() %>%
  mutate(panhandle = ifelse(panhandle, "Panhandle Voters",
                            "Secondary Control Voters"))

ll$panhandle <- factor(ll$panhandle, levels = c("Panhandle Voters",
                                                  "Secondary Control Voters"))

plot_pan <- ggplot(ll, aes(x = as.integer(year), y = voted, linetype = panhandle)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
saveRDS(plot_pan, "./temp/plot_pan.rds")


##########

ll2 <- combine %>%
  filter(panhandle, treated_county == "LIB") %>%
  group_by(treated, year) %>%
  summarize(voted = weighted.mean(voted, weight),
            rel = weighted.mean(rel, weight)) %>%
  ungroup() %>%
  mutate(treated = ifelse(treated, "Treated Voters",
                            "Primary Control Voters"))

ll2$treated <- factor(ll2$treated, levels = c("Treated Voters",
                                                "Primary Control Voters"))

plot_neighbors <- ggplot(ll2, aes(x = as.integer(year), y = voted, linetype = treated)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent)
plot_neighbors
saveRDS(plot_neighbors, "./temp/ll_to.rds")

#######

ll3 <- combine %>%
  filter(panhandle) %>% 
  mutate(group = ifelse(treated, "Treated",
                        ifelse(panhandle, "Primary Controls", "Secondary Controls"))) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight))

ll3$group <- factor(ll3$group, levels = c("Treated",
                                              "Primary Controls",
                                              "Secondary Controls"))

plot_all <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all
saveRDS(plot_all, "temp/trip_diff_plot.rds")

#######

ll1 <- combine %>%
  filter(panhandle) %>% 
  mutate(group = ifelse(treated, "Treated", "Control")) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  mutate(fac = "Neighboring Voters")

ll2 <- combine %>%
  mutate(group = ifelse(panhandle, "Treated", "Control")) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  mutate(fac = "Panhandle vs Rest of State")

ll3 <- bind_rows(ll1, ll2)

ll3$group <- factor(ll3$group, levels = c("Treated",
                                          "Control"))

plot_all <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  facet_wrap(~fac) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all
saveRDS(plot_all, "temp/trip_diff_plot.rds")

###################################################


ll3 <- combine %>%
  mutate(group = ifelse(treated, "Weather + Admin", ifelse(panhandle, "Weather Only", "Control"))) %>%
  group_by(group, year, treated_county) %>%
  summarize(voted = weighted.mean(voted, weight)) %>% 
  mutate(fac = "Neighboring Voters")


ll3$group <- factor(ll3$group, levels = c("Weather + Admin",
                                          "Weather Only",
                                          "Control"))

plot_all <- ggplot(ll3, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  facet_grid(treated_county~.) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all

saveRDS(ll3, "temp/county_level_plot_trip.rds")
#################################



llb<- combine %>%
  mutate(group = ifelse(treated, "Weather + Admin", ifelse(panhandle, "Weather Only", "Control"))) %>%
  group_by(group, year) %>%
  summarize(voted = weighted.mean(voted, weight))

llb$group <- factor(llb$group, levels = c("Weather + Admin", "Weather Only", "Control"))

plot_all <- ggplot(llb, aes(x = as.integer(year), y = voted, linetype = group, shape = group)) +
  geom_line() + geom_point() + theme_bw() +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group", shape = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))
plot_all
saveRDS(plot_all, "temp/trip_combine.rds")

###################################
###################################
###################################
###################################
###################################
