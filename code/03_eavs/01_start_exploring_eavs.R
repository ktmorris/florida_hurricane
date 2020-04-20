treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")

eavs <- fread("./raw_data/eavs/EAVS_2018_for_Public_Release_nolabel.csv") %>% 
  filter(State_Abbr == "FL",
         FIPSCode != "1211100000") %>% 
  select(FIPSCode, Jurisdiction_Name, starts_with("C"), A1a) %>% 
  rename(sent_ballots = C1a,
         spoiled = C1c,
         xcounted = C3a,
         rejected = C4a,
         late = C4b,
         returned = C1b,
         registered_voters = A1a) %>% 
  select(-starts_with("C")) %>% 
  rename(counted = xcounted) %>% 
  mutate(late = as.integer(late),
         rej_rate = (rejected - late) / sent_ballots,
         spoil_rate = spoiled / sent_ballots,
         count_rate = counted / sent_ballots,
         return_rate = returned / sent_ballots,
         Jurisdiction_Name = gsub(" COUNTY", "", Jurisdiction_Name)) %>% 
  mutate(treated = Jurisdiction_Name %in% c(treated_countiesb),
         request_rate = sent_ballots / registered_voters) %>% 
  arrange(request_rate)
#################

e16 <- read_xls("./raw_data/eavs/EAVS 2016 Final Data for Public Release v.4 (CRD).xls",
                sheet = "SECTION A")
colnames(e16) <- make.names(colnames(e16))
e16 <- e16 %>% 
  filter(State == "FL") %>% 
  select(FIPSCode, registered_voters_16 = A1a) %>% 
  mutate(registered_voters_16 = as.integer(registered_voters_16))

eb16 <- read_xls("./raw_data/eavs/EAVS 2016 Final Data for Public Release v.4 (CRD).xls",
                 sheet = "SECTION C")
colnames(eb16) <- make.names(colnames(eb16))
eb16 <- eb16 %>% 
  filter(State == "FL") %>% 
  select(FIPSCode, sent_16 = C1a) %>% 
  mutate(sent_16 = as.integer(sent_16))

e16 <- full_join(e16, eb16) %>% 
  mutate(req_rate_16 = sent_16 / registered_voters_16)

###################

f <- full_join(eavs, e16) %>% 
  mutate(diff = request_rate - req_rate_16) %>% 
  mutate(treated = ifelse(treated, "Treated", "Not Treated"))

f$treated <- factor(f$treated, levels = c("Treated", "Not Treated"))

# f <- full_join(eavs, e16) %>% 
#   mutate(diff = request_rate - req_rate_16) %>% 
#   mutate(treated = ifelse(treated, "Treated",
#                           ifelse(Jurisdiction_Name %in% c("WAKULLA", "WALTON", "HOLMES", "LEON"),
#                                  "Neighbor", "Not Treated")))
# 
# f$treated <- factor(f$treated, levels = c("Treated", "Neighbor", "Not Treated"))


eavsplot <- ggplot(f, aes(x = reorder(Jurisdiction_Name, diff), y = diff, fill = treated)) +
  geom_col(color = "black") + theme_bw() +
  scale_fill_manual(values = c("grey", "navy"), name = "Treatment Group") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Percentage Point Difference in Absentee Request Rate\n2016 - 2018") +
  scale_y_continuous(labels = percent)
saveRDS(eavsplot, "./temp/eavsplot.rds")
