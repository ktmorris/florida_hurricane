
treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")
####

neighbors <- fread("./temp/neighbors.txt") %>% 
  filter(src_county %in% treated_counties,
         nbr_county %in% control_counties,
         (src_county != "LIB" | nbr_county != "LEO")) %>% ## uncontested, different parties
  mutate(src_countypct = paste0(src_county, as.integer(src_pct)),
         nbr_countypct = paste0(nbr_county, as.integer(nbr_pct))) %>% 
  select(src_countypct,
         nbr_countypct)
#####
counties <- readOGR("./raw_data/shapefiles/Florida_Counties",
                    "Florida_Counties")
counties <- spTransform(counties, CRS = proj4string(map))
counties@data$id <- rownames(counties@data)
counties_m <- fortify(counties)
counties_m <- left_join(counties_m, counties@data)

#####

map <- readOGR("./raw_data/shapefiles/fl_2016",
               "fl_2016")

map <- gBuffer(map, byid = TRUE, width = 0)

map@data$id <- rownames(map@data)
ll <- fortify(map)

ll <- left_join(ll, map@data)
ll <- filter(ll, county %in% c(treated_counties, control_counties))

ll <- ll %>% 
  mutate(countypct = paste0(county, as.integer(as.character(pct))),
         who = ifelse(countypct %in% neighbors$src_countypct, "Treated Precinct",
                      ifelse(countypct %in% neighbors$nbr_countypct, "Control Precinct",
                             "Discarded Precinct")))

rat <- (max(ll$long) - min(ll$long)) / (max(ll$lat) - min(ll$lat))

ll$who <- factor(ll$who, levels = c("Treated Precinct", "Control Precinct", "Discarded Precinct"))

plot <- ggplot() +
  geom_polygon(data = ll, aes(x = long, y = lat, group = group, fill = who)) +
  geom_path(data = ll, aes(x = long, y = lat, group = group), color = "white") +
  geom_path(data = counties_m, aes(x = long, y = lat, group = group), size = 1) +
  coord_equal(xlim = c(-1200000, -250000),
              ylim = c(1800000, (1820000 + (1200000 - 250000) / rat))) +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  labs(fill = "Group", x = NULL, y = NULL) +
  scale_fill_grey(guide = guide_legend(title.position = "top",
                                       title.hjust = 0.5))
saveRDS(plot, "./temp/treated_control_precincts_map.rds")
