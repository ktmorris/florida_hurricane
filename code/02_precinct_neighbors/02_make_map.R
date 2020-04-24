
treated_counties <- c("BAY", "CAL", "FRA", "GAD", "GUL", "JAC", "LIB", "WAS")
treated_countiesb <- c("BAY", "CALHOUN", "FRANKLIN", "GADSDEN", "GULF", "JACKSON", "LIBERTY", "WASHINGTON")
control_counties <- c("WAL", "HOL", "WAK", "LEO")
control_countiesb <- c("WALTON", "HOLMES", "WAKULLA", "LEON")

#####
counties <- readOGR("./raw_data/shapefiles/Florida_Counties",
                    "Florida_Counties")
counties@data$id <- rownames(counties@data)
counties_m <- fortify(counties)
counties_m <- left_join(counties_m, counties@data)

counties_m <- filter(counties_m, COUNTYNAME %in% c(treated_countiesb, control_countiesb))

counties_m$groupb <- ifelse(counties_m$COUNTYNAME %in% treated_countiesb, "Treated County", "Control County")

#####
buffer <- readOGR("./temp", "buffer_shape")

buffer <- fortify(buffer)
buffer$groupb <- "Two Mile Buffer"


plot <- ggplot() +
  geom_polygon(data = counties_m, aes(x = long, y = lat, group = group, fill = groupb)) +
  geom_polygon(data = buffer, aes(x = long, y = lat, group = group, fill = groupb)) +
  geom_path(data = counties_m, aes(x = long, y = lat, group = group), color = "white") +
  coord_equal() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  labs(fill = "Group", x = NULL, y = NULL) +
  scale_fill_grey(guide = guide_legend(title.position = "top",
                                       title.hjust = 0.5))

#####
counties_f <- fortify(counties)
counties_f <- left_join(counties_f, counties@data)

counties_f <- mutate(counties_f, inc = COUNTYNAME %in% c(treated_countiesb, control_countiesb))

plot2 <- ggplot() +
  geom_polygon(data = counties_f, aes(x = long, y = lat, group = group, fill = inc)) +
  coord_equal() +
  theme(legend.position = "none",
        axis.ticks = element_line(color = "transparent"),
        axis.line = element_line(color = "transparent"),
        axis.text = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  labs(fill = NULL, x = NULL, y = NULL) +
  scale_fill_manual(values = c("grey", "black"))

map <- ggdraw() +
  draw_plot(plot) +
  draw_plot(plot2, x = 0.7, y = 0, width = 0.3, height = 0.3)
map

saveRDS(map, "./temp/treated_control_precincts_map.rds")
