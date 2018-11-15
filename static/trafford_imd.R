## Trafford IMD 2015 ##

# load libraries
library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext)

# load geospatial data
imd <- st_read("https://www.traffordDataLab.io/open_data/imd_2015/IMD_2015_trafford.geojson")
wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_full_resolution.geojson")

# plot map
ggplot() +
  geom_sf(data = imd, aes(fill = factor(decile)), alpha = 0.8, colour = "#FFFFFF", size = 0.2) +
  geom_sf(data = wards, fill = NA, alpha = 1, colour = "#212121",  size = 0.8) +
  geom_shadowtext(data = wards, aes(x = lon, y = lat, label = area_name), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", nudge_y = 0.002) +
  scale_fill_manual(breaks = 1:10,
                    values = c("#A31A31", "#D23B33", "#EB6F4A", "#FCB562", "#F4D78D", "#D8E9EC", "#AAD1DE", "#75A8C8", "#4D77AE", "#353B91"),
                    labels = c("Most\ndeprived", 2:9, "Least\ndeprived")) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Index of Multiple Deprivation (2015)",
       subtitle = "Trafford Lower-layer Super Output Areas by decile",
       caption = "Source: Indices of Multiple Deprivation (2015), MHCLG | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2018",
       x = NULL, y = NULL,
       fill = "") +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.subtitle = element_text(size = 12, face = "plain", colour = "#757575", margin = margin(b = 5)),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4),
        legend.title = element_text(colour = "#757575"),
        legend.text = element_text(colour = "#757575"),
        legend.position = c(0.18, 0.95)) +
  guides(fill = guide_legend(label.position = "bottom", 
                             label.hjust = 0,
                             direction = "horizontal",
                             nrow = 1,
                             keyheight = unit(2, units = "mm"), 
                             keywidth = unit(5, units = "mm")))

# write results
ggsave("output/trafford_imd.png", dpi = 300, scale = 1)

