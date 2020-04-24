fl_voters <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(ratio = actual_dist / expected_dist) %>% 
  mutate(actual_dist = actual_dist / 1000,
         expected_dist = expected_dist / 1000)
  
###########
history <- dbConnect(SQLite(), "D:/national_file_history.db")

fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   BallotType_General_2018_11_06,
                                   BallotType_General_2016_11_08,
                                   BallotType_General_2014_11_04,
                                   BallotType_General_2012_11_06,
                                   BallotType_General_2010_11_02
                                   from florida_history_type")
fl_history <- filter(fl_history, LALVOTERID %in% c(fl_voters$LALVOTERID))
colnames(fl_history) <- c("LALVOTERID", "v2018", "v2016", "v2014", "v2012", "v2010")
#############

fl_voters <- full_join(fl_voters, fl_history, by = "LALVOTERID")

fl_voters <- fl_voters %>% 
  mutate_at(vars("v2018", "v2016", "v2014", "v2012", "v2010"),
            ~ ifelse(. %in% c("", "Provisional"), "Abstain", .)) %>% 
  mutate_at(vars("v2018", "v2016", "v2014", "v2012", "v2010"),
            ~ factor(., levels = c("Poll Vote", "Early", "Absentee", "Abstain"))) %>% 
  mutate(change = (actual_dist - expected_dist),
         change = ifelse(is.na(change), 0, change))

m1 <- multinom(v2018 ~ ratio + expected_dist +
                 white + black + latino + asian +
                 male + dem + rep + age +
                 v2016 + v2014 + v2012 + v2010, data = fl_voters)

r2 <- round(PseudoR2(m1), digits = 3)
obs <- comma(length(residuals(m1)) / 4)

###########

m2 <- multinom(v2018 ~ change + expected_dist +
                 white + black + latino + asian +
                 male + dem + rep + age +
                 v2016 + v2014 + v2012 + v2010, data = fl_voters)

r22 <- round(PseudoR2(m2), digits = 3)
obs2 <- comma(length(residuals(m2)) / 4)
##########

stargazer(m2,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Change in Distance to Polling Place (km)", 
                               "Distance to Closest Planned Polling Place (km)",
                               "White", "Black", "Latino", "Asian", "Male", "Democrat", "Republican", "Age"),
          dep.var.labels = c("Early", "Absentee", "Abstain"),
          title = "\\label{tab:treated-multi} Vote Mode in 2018 (Relative to In-Person on Election Day)",
          table.placement = "H",
          omit = c("female", "v201"),
          omit.stat = "aic",
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          add.lines=list(c("Includes vote-mode in 2010, 2012, 2014, and 2016" , "", "X", ""),
                         c("Number of Observations" , "", obs2, ""),
                         c("McFadden Pseudo R2" , "", r22, "")))

j <- fread("./temp/test.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{4}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))


j <- j %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)

write.table(j, "./temp/multinom.tex", quote = F, col.names = F,
            row.names = F)


#########
marg <- ggeffect(model = m2, "change [all]")

marg_plot <- ggplot(data = filter(marg, response.level != "Poll.Vote")) + 
  geom_histogram(aes(x = change, y = ..count../250000), position="identity", linetype=1,
                 fill="gray60", data = fl_voters, alpha=0.5, bins = 30) + 
  facet_grid(. ~ response.level) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25) +
  xlab("Distance to Closest Open Polling Place - Distance to Closest Planned Polling Place (in kilometers)") +
  ylab("Predicted Probability") + scale_x_continuous(labels = comma, limits = c(min(marg$x), 20)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Relocated Polling Place on Vote Mode") +
  labs(caption = "Notes: Distribution of changed distance to polling place shown at bottom.") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0))


saveRDS(marg_plot, "./temp/marginal_effects_plot.rds")

####################
tt <- fl_voters

fl_voters <- filter(fl_voters, !is.na(expected_dist))

m3 <- multinom(v2016 ~ expected_dist + 
                 white + black + latino + asian +
                 male + dem + rep + age +
                 v2014 + v2012 + v2010, data = fl_voters)

marg <- ggeffect(model = m3, "expected_dist")

marg_plot2 <- ggplot(data = filter(marg, response.level != "Poll.Vote")) + 
  geom_histogram(aes(x = expected_dist, y = ..count../250000), position="identity", linetype=1,
                 fill="gray60", data = fl_voters, alpha=0.5, bins = 30) + 
  facet_grid(. ~ response.level) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25) +
  xlab("Distance to Planned 2018 Polling Place (in meters)") +
  ylab("Predicted Probability") + scale_x_continuous(labels = comma, limits = c(min(marg$x), 10000)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Marginal Effect of Distance to Planned 2018 Polling Place on 2016 Vote Mode") +
  labs(caption = "Notes: Distribution of distance to planned 2018 polling place shown at bottom.") +
  theme_bw() +
  theme(plot.caption = element_text(hjust = 0))
