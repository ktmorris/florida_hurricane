stargazer(m1, m2, m3,
          header = F,
          type = "latex", notes.align = "l",
          covariate.labels = c("Treated", "2018", "Treated $\\times$ 2018"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:full-dind} Turnout, 2010 --- 2018",
          table.placement = "H",
          omit.stat = c("f", "ser", "aic"),
          omit = c("white", "black", "latino", "asian", "female", "male",
                   "dem", "rep", "age", "median_income", "some_college",
                   "diff"),
          table.layout = "-cmd#-t-a-s-n",
          out = "./temp/test.tex",
          out.header = F,
          notes = "TO REPLACE",
          se = list(coef(summary(m1, cluster = c("group")))[, 2],
                    coef(summary(m2, cluster = c("group")))[, 2],
                    coef(summary(m3, cluster = c("group")))[, 2]),
          add.lines=list(c("Includes Other Matched Covariates" , "", "X", "X"),
                         c("Includes control for CD competitiveness", "", "", "X")))

j <- fread("./temp/test.tex", header = F, sep = "+")

note.latex <- "\\multicolumn{4}{l}{\\scriptsize{\\parbox{.5\\linewidth}{\\vspace{2pt}$^{***}p<0.01$, $^{**}p<0.05$, $^*p<0.1$. \\\\Robust standard errors (clustered at level of match) in parentheses.}}}"

j <- j %>% 
  mutate(n = row_number(),
         V1 = ifelse(grepl("TO REPLACE", V1), note.latex, V1),
         V1 = ifelse(grepl("\\\\#tab", V1), gsub("\\\\#", "", V1), V1)) %>% 
  filter(!grepl("Note:", V1))


j <- j %>% 
  mutate(V1 = gsub("dollarsign", "\\\\$", V1)) %>% 
  arrange(n) %>% 
  select(-n)

write.table(j, "./temp/dind_full.tex", quote = F, col.names = F,
            row.names = F)