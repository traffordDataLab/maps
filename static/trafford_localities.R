# Trafford localities #

library(tidyverse) ; library(sf) ; library(ggplot2) ; library(ggspatial)

sf <- st_read("https://www.trafforddatalab/spatial_data/council_defined/trafford_localities.geojson")

ggplot() +
  geom_sf(data = sf, fill = "#CCE3AA", alpha = 1, colour = "#FFFFFF",  size = 2) +
  geom_text(data = sf, aes(x = lon, y = lat, label = locality), colour = "#FFFFFF", size = 4, fontface = "bold") +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Trafford's localities",
       subtitle = NULL,
       caption = "Contains OS data © Crown copyright and database right (2018) | @traffordDataLab",
       x = NULL, y = NULL) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4))

ggsave("trafford_localities.png", dpi = 300, scale = 1)
