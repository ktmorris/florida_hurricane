pps <- rbindlist(lapply(list.files("raw_data/actual_expected_polling", full.names = T), function(f){
  k <- fread(f) %>% 
    mutate(m = gsub("raw_data/actual_expected_polling/|.csv", "", f))
  
  k <- cSplit(k, "m", sep = "_") %>% 
    dplyr::select(type = m_1, county = m_2) %>% 
    group_by(county, type) %>% 
    tally()
})) %>% 
  pivot_wider(id_cols = "county", names_from = "type", values_from = "n") %>% 
  mutate(c = toupper(substring(county, 1, 3)),
         share_open = actual / expected) %>% 
  mutate(county = str_to_title(county))


ev <- fread("raw_data/ev_2016_2018.csv")

pps <- left_join(pps, ev) %>% 
  select(-c)

colnames(pps) <- c("County", "Actual", "Expected", "Share Open", "2018", "2016", "Change")

saveRDS(pps, "temp/pps_table_changes.rds")
