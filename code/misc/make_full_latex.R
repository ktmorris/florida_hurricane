stargazer(models,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Treated", "2018",
                               
                               "Relative Rainfall in 2018",
                               "Treated $\\times$ Relative Rainfall in 2018",
                               "2018 $\\times$ Relative Rainfall in 2018",
                               
                               "Share of Expected Polling Places Open in 2018",
                               "Treated $\\times$ Share of Expected Polling Places Open in 2018",
                               "2018 $\\times$ Share of Expected Polling Places Open in 2018",
                               
                               "Treated $\\times$ 2018",
                               "Treated $\\times$ 2018 $\\times$ Relative Rainfall in 2018",
                               "Treated $\\times$ 2018 $\\times$ Share of Expected Polling Places Open in 2018"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:full-dind} Turnout, 2010 --- 2018",
          table.placement = "h",
          omit.stat = c("f", "ser", "aic"),
          keep = c("treatment", "d18", "rel", "share"),
          table.layout = "-cmd#-t-a-s-n",
          order = c(1, 2, 15, 18, 19, 16, 20, 21, 17, 22, 23),
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = ses_cl,
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "X"),
                         c("Includes control for CD competitiveness", "", "", "X"),
                         c("Includes rainfall and its interactions", "", "", "", "X", "X"),
                         c("Includes share of polling places open and its interactions", "", "", "", "", "X")))

j <- fread("./temp/test.tex", header = F, sep = "+")

j <- j[c(1:14, 33:nrow(j)), ]

note.latex <- "\\multicolumn{5}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))


insert1 <- "\\resizebox{1\\textwidth}{!}{%"
insert2 <- "}"

j <- bind_rows(j, data.frame(V1 = c(insert1, insert2), n = c(3.1, nrow(j) + 1 - 0.01))) %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  dplyr::select(-n)

write.table(j, "./temp/dind_full.tex", quote = F, col.names = F,
            row.names = F)
