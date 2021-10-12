combine <- readRDS("temp/trip_diff_reg_data.rds")

combine$yint <- as.integer(combine$year)
combine$treated_county <- factor(combine$treated_county, sort(unique(combine$treated_county)))

f1 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]

f2 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel, yint] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]

f3 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]

f4 <- voted ~ treated_18*treated_change + panhandle_18*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel +
  i(treated_county, panhandle_18, "BAY") + i(treated_county, treated_18, "BAY") +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college + reg_date |
  c2^treated_county + year^treated_county +
  c2[treated_change, treated_rel, yint] + year[treated_change, treated_rel] +
  treated_county[treated_change, treated_rel]


models <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  m <- feols(fml = f, data = combine, weights = ~weight, cluster = c("group_id", "voter", "c2"))
})


save(models, file = "temp/trip_diff_rob_matched.rdata")
