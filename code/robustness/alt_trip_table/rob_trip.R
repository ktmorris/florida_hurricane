combine <- readRDS("temp/trip_diff_reg_data.rds")

combine$yint <- as.integer(combine$year)

f1 <- voted ~ panhandle_18*treated_county + treated_18*treated_county +
  c2*treated_county + year*treated_county +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel

f2 <- voted ~ panhandle_18*treated_county + treated_18*treated_county +
  c2*treated_county + year*treated_county +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college

f3 <- voted ~ panhandle_18*treated_county + treated_18*treated_county +
  c2*treated_county + year*treated_county +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  c2*as.integer(year)

f4 <- voted ~ panhandle_18*treated_county + treated_18*treated_county +
  c2*treated_county + year*treated_county +
  treated_18*treated_change + panhandle_18*treated_change + c2*treated_change + year*treated_change +
  treated_18*treated_rel + panhandle_18*treated_rel + c2*treated_rel + year*treated_rel +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college +
  c2*yint

models <- lapply(c(f1, f2, f3, f4), function(f){
  print(f)
  m <- feols(fml = f, data = combine, weights = ~weight, cluster = c("group_id", "voter"))
})


save(models, file = "temp/trip_diff_rob_matched.rdata")
