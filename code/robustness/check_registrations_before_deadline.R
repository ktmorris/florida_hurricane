

h <- dbGetQuery(db, "select County, Voters_OfficialRegDate from fl")

h <- h %>% 
  filter(!(County %in% c("WALTON", "HOLMES", "WAKULLA", "LEON"))) %>% 
  mutate(Voters_OfficialRegDate = as.Date(Voters_OfficialRegDate, "%m/%d/%Y")) %>% 
  filter((Voters_OfficialRegDate <=   "2018-11-06" & Voters_OfficialRegDate > (as.Date("2018-11-06") - 35)) |
           (Voters_OfficialRegDate <= "2016-11-08" & Voters_OfficialRegDate > (as.Date("2016-11-08") - 35)) |
           (Voters_OfficialRegDate <= "2014-11-04" & Voters_OfficialRegDate > (as.Date("2014-11-04") - 35)) |
           (Voters_OfficialRegDate <= "2012-11-06" & Voters_OfficialRegDate > (as.Date("2012-11-06") - 35)) |
           (Voters_OfficialRegDate <= "2010-11-02" & Voters_OfficialRegDate > (as.Date("2010-11-02") - 35))) %>% 
  mutate(year = year(Voters_OfficialRegDate),
         treated = County %in% c("BAY", "CALHOUN", "FRANKLIN",
                                 "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")) %>% 
  group_by(treated, year) %>% 
  tally() %>% 
  mutate(treated = ifelse(treated, "Covered Counties", "Uncovered Counties"))


h$treated <- factor(h$treated, levels = c("Covered Counties", "Uncovered Counties"))

scale_fac <- sum(h$n * (h$treated == "Covered Counties")) / sum(h$n * (h$treated== "Uncovered Counties"))

h$n2 <- ifelse(h$treated == "Covered Counties", h$n,
               h$n * scale_fac)

f <- ggplot(h, aes(x = year, y = n2, linetype = treated)) + geom_line() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Registrations in Covered Counties", x = "Year", linetype = "Group") +
  scale_y_continuous(labels = comma,
                     sec.axis = sec_axis(~ . / scale_fac, name = "Registrations in Uncovered Counties",
                                         labels = comma)) +
  geom_point()

f
save(f, scale_fac, file = "temp/regs_before_deadline.rdata")
