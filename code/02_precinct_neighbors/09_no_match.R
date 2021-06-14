library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(caret)
library(tidyverse)
library(estimatr)
library(stargazer)
library(ebal)

pan <- readRDS("./temp/neighbor_voters.rds") %>% 
  select(-v18, -GEOID, -Precinct) %>% 
  mutate(panhandle = T)


fl_voters <- readRDS("./temp/pre_match_full_voters.rds") %>% 
  filter(!neighbor_county, !treated) %>% 
  select(-v18, -neighbor_county, -GEOID, -Precinct) %>% 
  rename(lat = latitude, lon = longitude) %>% 
  mutate(panhandle = F,
         treated = F)

fl_voters <- bind_rows(fl_voters, pan)


fl_voters <- fl_voters %>%
  mutate_at(vars(starts_with("v201")), ~ ifelse(. == 1, 0, 1)) %>%
  pivot_longer(cols = starts_with("v201"), names_to = "year",
               names_prefix = "v", names_transform = integer, values_to = "voted")

fl_voters$d18 <- fl_voters$year == "2018"
fl_voters$yint <- as.integer(fl_voters$year)

f1 <- lm(voted ~ panhandle*d18 + treated*d18  +
           white + black + latino + asian +
           female + male + dem + rep + age +
           median_income + some_college, fl_voters)

f2 <- lm(voted ~ panhandle*d18 + treated*d18  +
  white + black + latino + asian +
  female + male + dem + rep + age +
  median_income + some_college +
  yint * county, fl_voters)

f3 <- readRDS("temp/county_lin_trip.rds")

stargazer(f1, f2, f3,
          header = F,
          type = "text", notes.align = "l",
          covariate.labels = c("Weather Treatment",
                               "Weather Treatment $\\times$ 2018",
                               "Administrative Treatment",
                               "Administrative Treatment $\\times$ 2018", "2018"),
          dep.var.labels = c("Turnout"),
          title = "\\label{tab:alt-specs-trip} Alternative Processing Approaches",
          column.labels = c("Unprocessed", "Unprocessed", "Primary Model"),
          table.placement = "h",
          omit.stat = c("f", "ser", "aic"),
          # omit = c("white", "black", "latino", "asian", "female", "male",
          #          "dem", "rep", "age", "median_income", "some_college",
          #          "diff", "yint", "county"),
          table.layout = "-cmd#-t-a-s-n",
          
          order = c("panhandle", "treated", "d18"),
          # order = c(1, 4, 2, 18, 21, 27, 29),
          out = "./temp/trip_dif_rob.tex",
          out.header = F,
          notes = "TO REPLACE",
          add.lines=list(c("Includes Matched Covariates" , "X", "X", "X"),
                         c("County Fixed Effects" , "", "X", "X"),
                         c("County-Linear Time Trends" , "", "X", "X")))

#######################################

ll <- fl_voters %>% 
  mutate(county = ifelse(treated, county,
                         ifelse(panhandle, "Weather Only", "Control"))) %>% 
  group_by(county, year) %>% 
  summarize(voted = mean(voted)) %>% 
  mutate(year = as.integer(year))


ll <- rbindlist(lapply(c("BAY", "GAD", "JAC", "LIB", "WAS"), function(c){
  
  j <- filter(ll, county %in% c(c, "Weather Only", "Control")) %>% 
    mutate(group = ifelse(county == c, "Weather + Admin", county),
           county = c)
})) %>% 
  mutate(type = "Unprocessed")

ll$group <- factor(ll$group, levels = c("Weather + Admin",
                                        "Weather Only",
                                        "Control"))

ll2 <- readRDS("temp/county_level_plot_trip.rds") %>% 
  mutate(type = "Post-Matching",
         year = as.integer(year)) %>% 
  select(-fac) %>% 
  rename(county = treated_county)


ll3 <- bind_rows(ll, ll2)

ll3$type <- factor(ll3$type, levels = c("Unprocessed", "Post-Matching"))

pf <- ggplot(ll3, aes(x = year, y = voted, linetype = group)) +
  geom_line() + geom_point() +
  theme_bw() + 
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom") +
  labs(y = "Turnout", x = "Year", linetype = "Treatment Group") +
  scale_y_continuous(labels = percent) +
  facet_grid(county~type)

saveRDS(pf, "temp/triple_dif_plotted_all.rds")
