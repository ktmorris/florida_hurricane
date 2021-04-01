

h <- dbGetQuery(db, "select County, Voters_OfficialRegDate from fl")

h <- h %>% 
  filter(!(County %in% c("WALTON", "HOLMES", "WAKULLA", "LEON"))) %>% 
  mutate(Voters_OfficialRegDate = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>% 
  filter((Voters_OfficialRegDate <= "2018-10-09" & Voters_OfficialRegDate > "2018-10-02") |
           (Voters_OfficialRegDate <= "2016-10-11" & Voters_OfficialRegDate > "2016-10-04") |
           (Voters_OfficialRegDate <= "2014-10-06" & Voters_OfficialRegDate > "2014-09-29") |
           (Voters_OfficialRegDate <= "2012-10-09" & Voters_OfficialRegDate > "2012-10-02") |
           (Voters_OfficialRegDate <= "2010-10-04" & Voters_OfficialRegDate > "2010-09-27")) %>% 
  mutate(year = year(Voters_OfficialRegDate),
         treated = County %in% c("BAY", "CALHOUN", "FRANKLIN",
                                 "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")) %>% 
  group_by(treated, year) %>% 
  tally() %>% 
  mutate(treated = ifelse(treated, "Treated Group", "Control Group"))


h$treated <- factor(h$treated, levels = c("Treated Group", "Control Group"))

h$n2 <- ifelse(h$treated == "Treated Group", h$n,
               h$n * (sum(h$n * (h$treated == "Treated Group")) / sum(h$n * (h$treated== "Control Group"))))

ggplot(h, aes(x = year, y = n2, linetype = treated)) + geom_line() +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, 
                                    colour = "grey20"),
        strip.background = element_rect(fill = NA, 
                                        colour = "grey20")) +
  ggtitle("Scaled Registrations in Final Week Before Deadline") +
  labs(y = "Number of Registrations", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = comma)

