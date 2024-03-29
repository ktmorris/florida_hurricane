options("modelsummary_format_numeric_latex" = "plain")

fl_voters <- readRDS("./temp/moved_pps_dists.rds") %>% 
  mutate(ratio = actual_dist / expected_dist) %>% 
  mutate(actual_dist = actual_dist * 0.000621371,
         expected_dist = expected_dist * 0.000621371) %>% 
  mutate_at(vars(starts_with("v20")), ~ ifelse(. == 1, "Abstain",
                                             ifelse(. == 2, "Absentee",
                                                    ifelse(. == 3, "Early", "Poll Vote"))))
  
fl_voters <- fl_voters %>% 
  mutate_at(vars("v2018", "v2016", "v2014", "v2012", "v2010"),
            ~ factor(., levels = c("Poll Vote", "Abstain", "Early", "Absentee"))) %>% 
  mutate(change = (actual_dist - expected_dist),
         change = ifelse(is.na(change), 0, change))

###########

m2 <- multinom(v2018 ~ change + expected_dist +
                 white + black + latino + asian +
                 male + dem + rep + age +
                 v2016 + v2014 + v2012 + v2010, data = fl_voters)

r22 <- round(PseudoR2(m2, which = "McFadden"), digits = 3)
obs2 <- length(residuals(m2)) / 4
##########

rows <- tribble(~term,          ~x, ~y, ~z,
                "Vote-mode in 2010, 2012, 2014, and 2016", "", "$\\checkmark$", "",
                "Number of Observations", "", as.character(obs2), "",
                "McFadden Pseudo R2", "", as.character(r22), "")
attr(rows, 'position') <- c(22:24)

ms <- list(" " = m2)

modelsummary(ms,
             group = term ~ model + y.level,
             coef_map = c("change" = "Change in Distance to Polling Place (miles)", 
                          "expected_dist" = "Distance to Closest Planned Polling Place (miles)",
                          "whiteTRUE" = "White",
                          "blackTRUE" = "Black",
                          "latinoTRUE" = "Latino",
                          "asianTRUE" = "Asian",
                          "maleTRUE" = "Male",
                          "demTRUE" = "Democrat",
                          "repTRUE" = "Republican",
                          "age" = "Age",
                          "(Intercept)" = "Intercept"),
             stars = c("*" = 0.05, "**" = 0.01, "***" = 0.001),
             title = "\\label{tab:treated-multi} Vote Mode in 2018 (Relative to In-Person on Election Day)",
             add_rows = rows,
             exponentiate = T,
             output = "./temp/multinom.tex",
             escape = T)


j <- fread("./temp/multinom.tex", header = F, sep = "+") %>% 
  mutate(n = row_number(),
         n = ifelse(n == 33, 29.1, n)) %>% 
  arrange(n) %>% 
  mutate(n = row_number(),
         V1 = gsub(" /", "", V1))

insert3 <- "\\midrule"

j <- bind_rows(j, data.frame(V1 = c(insert3), n = c(30))) %>%
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>%
  arrange(n) %>%
  dplyr::select(-n)

write.table(j, "./temp/multinom.tex", quote = F, col.names = F,
            row.names = F)


#########
marg <- ggeffect(model = m2, "change [all]") %>% 
  mutate(response.level = ifelse(response.level == "Poll.Vote", "In Person (ED)",
                                 ifelse(response.level == "Early", "In Person (Early)", response.level)))

marg$response.level <- factor(marg$response.level, levels = c("In Person (ED)", "Abstain", "In Person (Early)", "Absentee"))

marg_plot <- ggplot(data = filter(marg)) + 
  geom_histogram(aes(x = change, y = ..count../250000), position="identity", linetype=1,
                 fill="gray60", data = fl_voters, alpha=0.5, bins = 30) + 
  facet_grid(. ~ response.level) +
  geom_line(aes(x = x, y = predicted)) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), fill= "blue", alpha=0.25) +
  xlab("Distance to Closest Open Polling Place - Distance to Closest Planned Polling Place (in miles)") +
  ylab("Predicted Probability") + scale_x_continuous(labels = comma, limits = c(min(marg$x), 15)) +
  scale_y_continuous(labels = percent) +
  ggtitle("") +
  labs(caption = "Notes: Distribution of changed distance to polling place shown at bottom.") +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, 
                                    colour = "grey20"),
        strip.background = element_rect(fill = NA, 
                                        colour = "grey20"),
        plot.caption = element_text(hjust = 0))

saveRDS(marg_plot, "./temp/marginal_effects_plot.rds")

####################
