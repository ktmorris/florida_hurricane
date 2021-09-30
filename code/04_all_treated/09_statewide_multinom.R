fl_roll <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county) %>% 
  mutate(id = row_number())

load("./temp/mout_hurricane_full.RData")


matches <- data.table(group = c(mout$index.treated, unique(mout$index.treated)),
                      voter = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, fl_roll, by = c("voter" = "id"))

##################

history <- dbConnect(SQLite(), "D:/national_file_history.db")
fl_history <- dbGetQuery(history, "select LALVOTERID,
                                   BallotType_General_2018_11_06,
                                   BallotType_General_2016_11_08,
                                   BallotType_General_2014_11_04,
                                   BallotType_General_2012_11_06,
                                   BallotType_General_2010_11_02
                                   from florida_history_type")
fl_history <- filter(fl_history, LALVOTERID %in% c(matches$LALVOTERID))
colnames(fl_history) <- c("LALVOTERID", "v2018", "v2016", "v2014", "v2012", "v2010")
###############
matches <- full_join(matches, fl_history, by = "LALVOTERID")

matches <- matches %>% 
  mutate_at(vars("v2018", "v2016", "v2014", "v2012", "v2010"),
            ~ ifelse(. %in% c("", "Provisional"), "Abstain", .)) %>% 
  mutate_at(vars("v2018", "v2016", "v2014", "v2012", "v2010"),
            ~ factor(., levels = c("Poll Vote", "Early", "Absentee", "Abstain")))


m1 <- multinom(v2018 ~ treated +
                 v2016 + v2014 + v2012 + v2010, data = matches, weights = weight)


r2 <- round(PseudoR2(m1), digits = 3)
obs <- comma(length(residuals(m1)) / 4)
stargazer(m1,
          header = F,
          type = "text", notes.align = "l",
          #covariate.labels = c("Ratio", "Change (km)", "White", "Black", "Latino", "Asian", "Male", "Democrat", "Republican", "Age"),
          #dep.var.labels = c("Early", "Absentee", "Abstain"),
          title = "\\label{tab:treated-multi} Vote Mode in 2018 (Relative to In-Person on Election Day)",
          table.placement = "H",
          #column.labels = c("Ratio of Distance Change", "Distance Change in KM"),
          #column.separate = c(3, 3),
          omit = c("female", "v201"),
          omit.stat = "aic",
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          add.lines=list(c("Includes vote-mode in 2010, 2012, 2014, and 2016" , "", "X", ""),
                         c("Number of Observations" , "", obs, ""),
                         c("McFadden Pseudo R2" , "", r2, "")))
