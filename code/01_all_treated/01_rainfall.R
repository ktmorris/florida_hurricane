

rainfall_oct <- rbindlist(lapply(c(2000:2018), function(y){
  rbindlist(lapply(c(10:30), function(d){
    d <- str_pad(as.character(d), side = "left", width = 2, pad = "0")
    print(paste0(as.character(y), "-10-", d))
    cpc_prcp(paste0(as.character(y), "-10-", d), us = T, drop_undefined = T) %>%
      mutate(date = paste0(as.character(y), "-10-", d))
  }))
}))

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
         lon = ifelse(lon > 180, -360 + lon, lon))

## turn weather sites into spatial object
weather_sites <- SpatialPoints(
  select(rainfall, x = lon, y = lat)
)

## find closest weather station to each voter
tree <- createTree(coordinates(weather_sites))
inds <- knnLookup(tree, newdat = coordinates(vps), k = 1)


voters <- left_join(cbind(voters, inds),
                    rainfall,
                    by = c("inds" = "id")) %>%
  mutate(rel = precip_2018 / precip_historical) %>% ## divide 2020 rainfall by historical rainfall
  select(LALVOTERID, rel)

saveRDS(voters, "temp/voter_rainfall.rds")
