

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
  mutate(treated = ifelse(treated, "Treated Counties", "Control Counties"))


h$treated <- factor(h$treated, levels = c("Treated Counties", "Control Counties"))

scale_fac <- sum(h$n * (h$treated == "Treated Counties")) / sum(h$n * (h$treated== "Control Counties"))

h$n2 <- ifelse(h$treated == "Treated Counties", h$n,
               h$n * scale_fac)

f <- ggplot(h, aes(x = year, y = n2, linetype = treated)) + geom_line() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Registrations in Treated Counties", x = "Year", linetype = "Group") +
  scale_y_continuous(labels = comma,
                     sec.axis = sec_axis(~ . / scale_fac, name = "Registrations in Control Counties",
                                         labels = comma)) +
  geom_point()


save(f, scale_fac, file = "temp/regs_before_deadline.rdata")
