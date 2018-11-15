## Trafford's localities and wards ##

# load libraries
library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext)

# load geospatial data
localities <- st_read("https://www.traffordDataLab.io/spatial_data/council_defined/trafford_localities.geojson")
wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson")

# plot map
ggplot() +
  geom_sf(data = wards, fill = "#DDDDCC", alpha = 1, colour = "#FFFFFF",  size = 0.5) +
  geom_sf(data = localities, fill = NA, colour = "#212121",  size = 1) +
  geom_shadowtext(data = wards, aes(x = lon, y = lat, label = area_name), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 2.5, bg.colour = "#212121", nudge_y = 0.002) +
  geom_shadowtext(data = localities, aes(x = lon, y = lat, label = area_code), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 4, bg.colour = "#212121", nudge_y = -0.002) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Trafford's localities and wards",
       subtitle = NULL,
       caption = "Contains OS data © Crown copyright and database right (2018) | @traffordDataLab",
       x = NULL, y = NULL) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4))

# write results
ggsave("output/trafford_localities.png", dpi = 300, scale = 1)
