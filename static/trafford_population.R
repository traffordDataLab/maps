## Trafford's population ##

# load libraries
library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext) ; library(viridis)

# load geospatial data
pop <- read_csv("https://www.traffordDataLab.io/open_data/mid-2017_population_estimates/mid-2017_population_estimates_ward.csv") %>% 
  filter(gender == "Persons") %>% 
  select(area_code, all_ages)

localities <- st_read("https://www.traffordDataLab.io/spatial_data/council_defined/trafford_localities.geojson")
wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_full_resolution.geojson") %>% 
  left_join(., pop, by = "area_code")

# plot map
ggplot() +
  geom_sf(data = wards, aes(fill = all_ages), alpha = 1, colour = "#FFFFFF",  size = 0.5) +
  geom_sf(data = localities, fill = NA, colour = "#212121",  size = 1) +
  geom_shadowtext(data = wards, aes(x = lon, y = lat, label = area_name), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 2.5, bg.colour = "#212121", nudge_y = 0.002) +
  geom_shadowtext(data = localities, aes(x = lon, y = lat, label = area_code), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 4, bg.colour = "#212121", nudge_y = -0.002) +
  scale_fill_viridis(discrete = F,
                     name = "Persons", label = scales::comma,
                     direction = -1,
                     guide = guide_colourbar(
                       direction = "horizontal",
                       barheight = unit(3, units = "mm"),
                       barwidth = unit(75, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5)) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Trafford's resident population (2017)",
       subtitle = NULL,
       caption = "Source: Mid-2017 population estimates, ONS | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2018",
       x = NULL, y = NULL) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4),
        legend.title = element_text(colour = "#757575"),
        legend.text = element_text(colour = "#757575"),
        legend.position = c(0.18, 0.95))

# write results
ggsave("output/trafford_population.png", dpi = 300, scale = 1)
