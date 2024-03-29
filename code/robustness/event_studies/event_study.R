

f1 <- voted ~ trig | c2 + year

f2 <- voted ~ trig*treated_rel + trig*treated_change |
  c2[treated_rel, treated_change] + year[treated_rel, treated_change]

matches <- readRDS("./temp/full_reg_data.rds")

hold <- NULL

for(y in c("2012", "2014", "2016","2018")){
  print(y)
  
  if(y == "2018"){
    dat <- matches
  }
  dat <- filter(matches, year <= y)
  dat$trig <- dat$treated * (dat$year == y)
  
  m1 <- feols(fml = f1, data = dat, weights = ~weight, cluster = c("voter", "c2", "group"))
  
  j <- cbind(as.data.table(confint(m1)), rownames(confint(m1))) %>% 
    rename(var = V2) %>% 
    filter(var == "trig") %>% 
    mutate(year = y,
           model = "overall",
           county = "overall")
  rm(m1)
  gc()
  
  m2 <- feols(fml = f2, data = dat, weights = ~weight, cluster = c("voter", "group", "c2"))
  
  k <- cbind(as.data.table(confint(m2)), rownames(confint(m2))) %>% 
    rename(var = V2) %>% 
    filter(var == "trig:treated_change") %>% 
    mutate(year = y,
           model = "inter",
           county = "overall")
  
  h <- bind_rows(j, k)
  
  for(c in unique(dat$treated_county)){
    print(c)
    dat2 <- filter(dat, treated_county == c)
    
    w <- dat2$weight
    
    m1 <- feols(fml = f1, data = dat2, weights = ~weight, cluster = c("voter", "group", "c2"))
    m2 <- feols(fml = f2, data = dat2, weights = ~weight, cluster = c("voter", "group", "c2"))
    
    j <- cbind(as.data.table(confint(m1)), rownames(confint(m1))) %>% 
      rename(var = V2) %>% 
      filter(var == "trig") %>% 
      mutate(year = y,
             model = "overall",
             county = c)
    
    k <- cbind(as.data.table(confint(m2)), rownames(confint(m2))) %>% 
      rename(var = V2) %>% 
      filter(var == "trig:treated_change") %>% 
      mutate(year = y,
             model = "inter",
             county = c)
    
    h <- bind_rows(h, j, k)
  }
  hold <- bind_rows(hold, h)
}

saveRDS(hold, "temp/event_study_all.rds")

es <- readRDS("temp/event_study_all.rds") %>% 
  mutate(county = ifelse(county == "overall", "Overall", county))

es$county <- factor(es$county, levels = c("Overall", "BAY", "CAL", "FRA", "GAD",
                                          "GUL", "JAC", "LIB", "WAS"))

colnames(es) <- c("lower", "upper", "var", "year", "model", "county")

es$estimate <- (es$lower + es$upper) / 2


theme_bc <- function(base_size = 11, base_family = "BentonSans",
                     legend.position = "right", face = "plain", ...) {
  library(extrafont)
  half_line <- base_size/2
  theme_bw(base_family = base_family) %+replace%
    theme(plot.caption = element_text(size = rel(0.8), hjust = 0,
                                      family = base_family,
                                      vjust = 1, margin = margin(t = half_line)),
          plot.title = element_text(size = rel(1.2), hjust = 0.5,
                                    vjust = 1, margin = margin(b = half_line),
                                    family = base_family),
          plot.subtitle = element_text(hjust = 0.5, vjust = 1, margin = margin(b = half_line)),
          legend.position = legend.position,
          text = element_text(family = base_family, face = face,
                              colour = "black", size = base_size, lineheight = 0.9,
                              hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(),
                              debug = FALSE),
          ...)
}

p <- ggplot(data = filter(es, model == "overall"), aes(y = year, x = estimate, xmin = lower, 
                               xmax = upper, color = county)) +
  ggstance::geom_pointrangeh(aes(y = year, x = estimate, 
                                 xmin = lower, xmax = upper, color = county),
                             position = ggstance::position_dodgev(height = .5), 
                             fill = "white", fatten = 3, size = 0.8, show.legend = T) +
  coord_flip() + theme_bc(base_family = "LM Roman 10") +
  theme(legend.position = "bottom", text = element_text(family = "LM Roman 10")) +
  labs(y = "Year", x = "Estimate", color = "County") + scale_x_continuous(labels = percent) +
  geom_vline(xintercept = 0, linetype = "dashed")

p
saveRDS(p, "temp/es_overall.rds")
