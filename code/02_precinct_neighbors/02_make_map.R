
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
counties_m$groupb <- ifelse(counties_m$COUNTYNAME %in% treated_countiesb, "Treated by Weather +\nAdministrative Changes", "Treated by Weather Only")
counties_m$groupb <- factor(counties_m$groupb, levels = c("Treated by Weather +\nAdministrative Changes", "Treated by Weather Only", "2.5 Mile Buffer"))

#####

buffer <- readOGR("./temp", "buffer_shape")
buffer <- fortify(buffer)
buffer$groupb <- "2.5 Mile Buffer"
buffer$groupb <- factor(buffer$groupb, levels = c("Treated by Weather +\nAdministrative Changes",
                                                  "Treated by Weather Only", "2.5 Mile Buffer"))

plot <- ggplot() +
  geom_polygon(data = counties_m, aes(x = long, y = lat, group = group, fill = groupb), color = "white") +
  geom_polygon(data = buffer, aes(x = long, y = lat, group = group, color = groupb),
               fill = "red", alpha = 0.25) +
  coord_equal() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        text = element_text(family = "LM Roman 10")) +
  labs(fill = "Group", x = NULL, y = NULL,
       color = "") +
  scale_color_manual(values = c("red"),
                    guide = guide_legend(title.position = "top",
                                         title.hjust = -.5)) +
  scale_fill_manual(values = c("#333333", "#CCCCCC", "#989898"),
                    guide = guide_legend(title.position = "top",
                                         title.hjust = .25))
plot

#####
counties@data$inc <- counties@data$COUNTYNAME %in% c(treated_countiesb, control_countiesb)
counties <- gUnaryUnion(counties, id = counties@data$inc)

counties_f <- fortify(counties)

plot2 <- ggplot() +
  geom_polygon(data = counties_f, aes(x = long, y = lat, group = group, fill = id)) +
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
