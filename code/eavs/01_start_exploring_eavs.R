treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")

eavs <- fread("./raw_data/eavs/EAVS_2018_for_Public_Release_nolabel.csv") %>% 
  filter(State_Abbr == "FL",
         FIPSCode != "1211100000") %>% 
  select(FIPSCode, Jurisdiction_Name, starts_with("C")) %>% 
  rename(sent_ballots = C1a,
         spoiled = C1c,
         xcounted = C3a,
         rejected = C4a,
         late = C4b,
         returned = C1b) %>% 
  select(-starts_with("C")) %>% 
  rename(counted = xcounted) %>% 
  mutate(late = as.integer(late),
         rej_rate = (rejected - late) / sent_ballots,
         spoil_rate = spoiled / sent_ballots,
         count_rate = counted / sent_ballots,
         return_rate = returned / sent_ballots,
         Jurisdiction_Name = gsub(" COUNTY", "", Jurisdiction_Name)) %>% 
  mutate(treated = Jurisdiction_Name %in% c(treated_countiesb)) %>% 
  arrange(rej_rate)

ggplot(eavs, aes(x = reorder(Jurisdiction_Name, spoil_rate), y = spoil_rate, fill = treated)) + geom_col()


summary(lm(spoil_rate ~ treated, data = eavs))
       