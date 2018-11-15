## Trafford's Metrolink network ##

# load libraries
library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggrepel) ; library(ggspatial) ; library(shadowtext)

# load geospatial data
lad <- st_read("https://www.traffordDataLab.io/spatial_data/local_authority/2016/trafford_local_authority_generalised.geojson")
lines <- st_read("https://www.traffordDataLab.io/open_data/transport/metrolink/lines/trafford_metrolink_lines.geojson")
stations <- st_read("https://www.traffordDataLab.io/open_data/transport/metrolink/stops/trafford_metrolink_stops.geojson") %>% 
  mutate(lng = map_dbl(geometry, ~st_coordinates(.x)[[1]]),
         lat = map_dbl(geometry, ~st_coordinates(.x)[[2]]))

# plot map
ggplot() +
  geom_sf(data = lad, fill = "#DDDDCC", alpha = 1, colour = "#212121",  size = 0.5) +
  geom_sf(data = lines, colour = "#FFDC45", size = 1) +
  geom_sf(data = stations, colour = "#FFDC45", fill = "#ffffff", shape = 21, size = 3, stroke = 1) +
  geom_shadowtext(data = filter(stations, !name %in% c("Trafford Bar", "Firswood", "Dane Road")), aes(lng, lat, label = name), 
                  colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", hjust = 0, nudge_x = 0.003) +
  geom_shadowtext(data = filter(stations, name == "Dane Road"), aes(lng, lat, label = name), 
                   colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", hjust = 0, nudge_x = -0.019) +
  geom_shadowtext(data = filter(stations, name == "Trafford Bar"), aes(lng, lat, label = name), 
                  colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", hjust = 0, nudge_x = -0.022) +
  geom_shadowtext(data = filter(stations, name == "Firswood"), aes(lng, lat, label = name), 
                  colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", hjust = 0, nudge_x = -0.017) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Trafford's Metrolink network",
       subtitle = NULL,
       caption = "Contains OpenStreetMap, Transport for Greater Manchester and OS data \n© Crown copyright and database right (2018) | @traffordDataLab",
       x = NULL, y = NULL) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4))

# write results
ggsave("output/trafford_metrolink.png", dpi = 300, scale = 1)
