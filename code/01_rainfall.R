
## get daily rainfall for october, 2000--2018
rainfall_oct <- rbindlist(lapply(c(2000:2018), function(y){
  rbindlist(lapply(c(10:31), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")
    print(paste0(as.character(y), "-10-", d))
    cpc_prcp(paste0(as.character(y), "-10-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-10-", d))
  }))
}))
## get daily rainfall for november, 2000--2018
rainfall_nov <- rbindlist(lapply(c(2000:2018), function(y){
  rbindlist(lapply(c(1:6), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")
    print(paste0(as.character(y), "-11-", d))
    cpc_prcp(paste0(as.character(y), "-11-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-11-", d))
  }))
}))

rainfall <- bind_rows(rainfall_oct, rainfall_nov)

rainfall$date <- as.Date(rainfall$date)

historical <- rainfall %>%
  group_by(year = year(date), lat, lon) %>%
  summarize(precip = sum(precip)) %>%
  mutate(g = ifelse(year == "2018", "precip_2018", "precip_historical")) %>%
  group_by(g, lat, lon) %>%
  summarize(precip = mean(precip)) %>%
  pivot_wider(id_cols = c("lat", "lon"), names_from = "g", values_from = "precip")

saveRDS(historical, "temp/rainfall.rds")
########################################
voters <- dbGetQuery(db, "select LALVOTERID, Residence_Addresses_Latitude,
                     Residence_Addresses_Longitude from fl") %>% 
  rename(latitude = Residence_Addresses_Latitude,
         longitude = Residence_Addresses_Longitude) %>% 
  filter(!is.na(latitude), !is.na(longitude))

vps <- SpatialPoints(
  voters %>%
    select(x = longitude, y = latitude)
)

################################################
rainfall <- readRDS("temp/rainfall.rds") %>%
  ungroup() %>%
  mutate(id = row_number(),
         lon = ifelse(lon > 180, -360 + lon, lon),
         rel = precip_2018 / precip_historical)

## turn weather sites into spatial object
weather_sites <- SpatialPoints(
  select(rainfall, x = lon, y = lat)
)

## find closest weather station to each voter
tree <- createTree(coordinates(weather_sites))
inds <- knnLookup(tree, newdat = coordinates(vps), k = 3)

inds <- as.data.table(inds) %>% 
  rename(loc1 = V1,
         loc2 = V2,
         loc3 = V3)

voters <- cbind(voters, inds)

voters <- left_join(voters, select(rainfall, id, rel1 = rel, lat, lon), by = c("loc1" = "id"))
voters$dist1 <- pointDistance(select(voters, longitude, latitude),
                         select(voters, lon, lat), lonlat = T) * 0.000621371

#####
voters <- left_join(select(voters, -lat, -lon), select(rainfall, id, rel2 = rel, lat, lon), by = c("loc2" = "id"))
voters$dist2 <- pointDistance(select(voters, longitude, latitude),
                              select(voters, lon, lat), lonlat = T) * 0.000621371

#####
voters <- left_join(select(voters, -lat, -lon), select(rainfall, id, rel3 = rel, lat, lon), by = c("loc3" = "id"))
voters$dist3 <- pointDistance(select(voters, longitude, latitude),
                              select(voters, lon, lat), lonlat = T) * 0.000621371


voters <- voters%>% 
  mutate(dist1w = 1 / (dist1 / (dist1 + dist2 + dist3)),
         dist2w = 1 / (dist2 / (dist1 + dist2 + dist3)),
         dist3w = 1 / (dist3 / (dist1 + dist2 + dist3)),
         rel = (rel1 * dist1w / (dist1w + dist2w + dist3w)) +
           (rel2 * dist2w / (dist1w + dist2w + dist3w)) +
           (rel3 * dist3w / (dist1w + dist2w + dist3w)))


saveRDS(select(voters, LALVOTERID, rel), "temp/voter_rainfall.rds")
