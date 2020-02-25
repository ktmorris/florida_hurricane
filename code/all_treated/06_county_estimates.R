treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")

lu <- data.frame(code = treated_counties,
                 name = treated_countiesb)

matches <- readRDS("./temp/full_reg_data.rds")

lapply(treated_counties, function(c){
  data <- filter(matches, treated_county == c)
  m1 <- glm(voted ~ treated*I(year == "2018"),
            data = data, family = "binomial")
  m2 <- glm(voted ~ treated*I(year == "2018") +
              white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college,
            data = data, family = "binomial")
  m3 <- glm(voted ~ treated*I(year == "2018") +
              white + black + latino + asian +
              female + male + dem + rep + age +
              median_income + some_college + diff,
            data = data, family = "binomial")
  save(m1, m2, m3, file = paste0("./temp/full_reg_", c, ".rdata"))
  
  c1 <- confint(m1)
  c2 <- confint(m2)
  c3 <- confint(m3)
  save(c1, c2, c3, file = paste0("./temp/cints_", c, ".rdata"))
})

tidies <- rbindlist(lapply(treated_counties,
                           function(county){
                             print(paste(county))
                             load(paste0("./temp/cints_", county, ".rdata"))
                             
                             model1 <- as.data.frame(exp(c1)) %>% 
                               mutate(vars = rownames(c1),
                                      model = "No Controls")
                             
                             colnames(model1) <- c("conf.low", "conf.high", "term", "model")
                             
                             model2 <- as.data.frame(exp(c2)) %>% 
                               mutate(vars = rownames(c2),
                                      model = "With Controls")
                             
                             colnames(model2) <- c("conf.low", "conf.high", "term", "model")
                             
                             model3 <- as.data.frame(exp(c3)) %>% 
                               mutate(vars = rownames(c3),
                                      model = "Plus CD Competitiveness")
                             
                             colnames(model3) <- c("conf.low", "conf.high", "term", "model")
                             
                             
                             tidies <- bind_rows(model1, model2, model3) %>% 
                               mutate(estimate = (conf.low + conf.high) / 2) %>% 
                               filter(grepl(pattern = "[:]", term)) %>% 
                               mutate(term = county)
                           }))

tidies$model <- factor(tidies$model, levels = c("No Controls", "With Controls", "Plus CD Competitiveness"))

tidies <- left_join(tidies, lu, by = c("term" = "code")) %>% 
  mutate(term = str_to_title(name)) %>% 
  select(-name)

tidies$term <- fct_rev(tidies$term)

#############
ci_level = 0.95
legend.title = "Model"
facet.label.pos = "top"
n_models <- length(unique(tidies$model))
oshapes <- c(21:25, 15:18, 3, 4, 8)
colors <- get_colors("CUD Bright", n_models)
shapes <- oshapes[1:n_models]
exp <- T

################

p <- ggplot(data = tidies)

p <- p + ggstance::geom_pointrangeh(aes(y = term, x = estimate, 
                                        xmin = conf.low, xmax = conf.high, linetype = model, 
                                        shape = model), position = ggstance::position_dodgev(height = -.5), 
                                    fill = "white", fatten = 3, size = 0.8, show.legend = T)

p <- p + geom_vline(xintercept = 1 - !exp, linetype = 2, 
                    size = 0.25) +
  scale_linetype_manual(values = c("dotted", "longdash", "solid"), name = legend.title) + 
  scale_shape_manual(values = shapes, name = legend.title) + theme() + 
  drop_y_gridlines() + theme(axis.title.y = element_blank(), 
                             axis.text.y = element_text(size = 10),
                             panel.grid.major.x = element_line(linetype = "solid"),
                             text = element_text(family = "LM Roman 10"),
                             legend.pos = "right") + 
  xlab("exp(Estimate)")

saveRDS(p, "./temp/county_estimates.rds")
