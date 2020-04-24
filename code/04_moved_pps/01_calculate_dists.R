
fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>%
  filter(treated == T)


movers <- rbindlist(lapply(c("BAY", "GULF", "JACKSON",
                             "WASHINGTON", "GADSDEN", "FRANKLIN",
                             "LIBERTY", "GADSDEN"), function(c){
                               print(c)
  county_all <- filter(fl_voters, county == substring(c, 1, 3))
  
  county_voters <- SpatialPoints(
    county_all %>%  
      dplyr::select(x = Residence_Addresses_Longitude, y = Residence_Addresses_Latitude)
  )
  
  ####################
  county_actual <- SpatialPoints(
    fread(paste0("./raw_data/actual_expected_polling/actual_", c, ".csv")) %>% 
      dplyr::select(x = longitude, y = latitude)
  )
  
  tree <- createTree(coordinates(county_actual))
  inds <- knnLookup(tree, newdat = coordinates(county_voters), k = 1)
  
  
  county_all <- cbind(county_all, inds) %>% 
    rename(actual = inds)
  
  county_all <- left_join(
    county_all,
    as.data.frame(county_actual@coords) %>% 
      mutate(actual = row_number())
  ) %>% 
    rename(actual_x = x,
           actual_y = y) %>% 
    dplyr::select(-actual)
  
  #########
  county_expected <- SpatialPoints(
    fread(paste0("./raw_data/actual_expected_polling/expected_", c, ".csv")) %>% 
      dplyr::select(x = longitude, y = latitude)
  )
  
  tree <- createTree(coordinates(county_expected))
  inds <- knnLookup(tree, newdat = coordinates(county_voters), k = 1)
  
  
  county_all <- cbind(county_all, inds) %>% 
    rename(expected = inds)
  
  county_all <- left_join(
    county_all,
    as.data.frame(county_expected@coords) %>% 
      mutate(expected = row_number())
  ) %>% 
    rename(expected_x = x,
           expected_y = y) %>% 
    dplyr::select(-expected)
  
  
  county_all$actual_dist <- pointDistance(dplyr::select(county_all, Residence_Addresses_Longitude,
                                                     Residence_Addresses_Latitude),
                                       dplyr::select(county_all, actual_x, actual_y), lonlat = T)
  
  
  county_all$expected_dist <- pointDistance(dplyr::select(county_all, Residence_Addresses_Longitude,
                                                       Residence_Addresses_Latitude),
                                         dplyr::select(county_all, expected_x, expected_y), lonlat = T)
  
  return(county_all)
}))

movers <- distinct(movers)

saveRDS(movers, "./temp/moved_pps_dists.rds")
