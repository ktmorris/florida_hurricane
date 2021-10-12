
############################
combine <- readRDS("temp/trip_diff_reg_data.rds")

t <- filter(combine, treated, year == 2018) %>% 
  select(voter, latitude, longitude, rel)

p <- filter(combine, !treated, panhandle, year == 2018) %>% 
  select(voter = group_id,
         control_lat = latitude,
         control_long = longitude,
         control_rel = rel)

t <- left_join(t, p)

dists <- pointDistance(select(t, control_long, control_lat),
                       select(t, longitude, latitude), lonlat = T) * 0.000621371

saveRDS(mean(dists), "./temp/average_distance.rds")
saveRDS(mean(t$rel), "temp/treated_rainfall.rds")
saveRDS(mean(t$control_rel), "temp/control_rainfall.rds")
