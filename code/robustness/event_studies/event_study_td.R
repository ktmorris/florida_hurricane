library(stargazer)
library(data.table)
library(miceadds)
library(Matching)
library(fixest)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(caret)
library(tidyverse)


f1 <- voted ~ ptrig + ttrig + county + year

f2 <- voted ~ ttrig*treated_change + ptrig*treated_change + county*treated_change + year*treated_change +
  ttrig*treated_rel + ptrig*treated_rel + county*treated_rel + year*treated_rel

matches <- readRDS("temp/trip_diff_reg_data.rds")

hold <- NULL

for(y in c("2012", "2014", "2016","2018")){
  print(y)
  
  if(y == "2018"){
    dat <- matches
  }else{
    dat <- filter(matches, year <= y)
  }
  dat <- filter(matches, year <= y)
  dat$ttrig <- dat$treated * (dat$year == y)
  dat$ptrig <- dat$panhandle * (dat$year == y)
  
  m1 <- feols(fml = f1, data = dat, weights = ~weight, cluster = c("c2", "voter", "group_id"))
  
  j <- cbind(as.data.table(confint(m1)), rownames(confint(m1))) %>% 
    rename(var = V2) %>% 
    filter(var == "ttrig") %>% 
    mutate(year = y,
           model = "overall",
           county = "overall")
  rm(m1)
  gc()
  
  m2 <- feols(fml = f2, data = dat, weights = ~weight, cluster = c("group_id", "voter"))
  
  k <- cbind(as.data.table(confint(m2)), rownames(confint(m2))) %>% 
    rename(var = V2) %>% 
    filter(var == "ttrig:treated_change") %>% 
    mutate(year = y,
           model = "inter",
           county = "overall")
  
  h <- bind_rows(j, k)
  
  for(c in unique(dat$treated_county)){
    print(c)
    dat2 <- filter(dat, treated_county == c)
    
    w <- dat2$weight
    
    m1 <- feols(fml = f1, data = dat2, weights = ~weight, cluster = c("county", "group_id", "voter"))
    m2 <- feols(fml = f2, data = dat2, weights = ~weight, cluster = c("group_id", "voter"))
    
    j <- cbind(as.data.table(confint(m1)), rownames(confint(m1))) %>% 
      rename(var = V2) %>% 
      filter(var == "ttrig") %>% 
      mutate(year = y,
             model = "overall",
             county = c)
    
    k <- cbind(as.data.table(confint(m2)), rownames(confint(m2))) %>% 
      rename(var = V2) %>% 
      filter(var == "ttrig:treated_change") %>% 
      mutate(year = y,
             model = "inter",
             county = c)
    
    h <- bind_rows(h, j, k)
  }
  hold <- bind_rows(hold, h)
}

saveRDS(hold, "temp/event_study_trip.rds")


es <- readRDS("temp/event_study_trip.rds") %>% 
  mutate(county = ifelse(county == "overall", "Overall", county))

es$county <- factor(es$county, levels = c("Overall", "BAY", "CAL", "FRA", "GAD",
                                          "GUL", "JAC", "LIB", "WAS"))

colnames(es) <- c("lower", "upper", "var", "year", "model", "county")

es$estimate <- (es$lower + es$upper) / 2

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
saveRDS(p, "temp/es_trip_overall.rds")
