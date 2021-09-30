
matches <- readRDS("./temp/full_reg_data.rds")

matches$treated_18 <- matches$treated * matches$d18
matches$share_open_18 <- matches$treated * matches$d18 * matches$share_open
matches$rel_18 <- matches$treated * matches$d18 * matches$treated_rel

f1 <- voted ~ treated_18 + c2 + year +
  c2*as.integer(year)

m1 <- lm(f1, data = matches, weights = weight)

ses_cl <- list(
  summary(lm.cluster(formula = f1, data = matches, weights = matches$weight, cluster = matches$c2))[,2]
)

save(m1, ses_cl, file = "temp/ame_clin.rdata")
