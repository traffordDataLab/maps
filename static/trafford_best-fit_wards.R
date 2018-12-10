# Best fit wards #

# load libraries
library(tidyverse) ; library(sf) ; library(ggplot2) ; library(cowplot)

# load geospatial data
wards <- st_read("https://opendata.arcgis.com/datasets/7193daa99995445aa84a0b23352e56a1_2.geojson") %>% 
  filter(wd17cd %in% paste0("E0", seq(5000819, 5000839, 1))) %>% 
  mutate(centroid_lng = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         centroid_lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(area_code = wd17cd, area_name = wd17nm, centroid_lng, centroid_lat)

lsoa <- st_read("https://opendata.arcgis.com/datasets/da831f80764346889837c72508f046fa_2.geojson") %>% 
  filter(grepl("Trafford", lsoa11nm)) %>% 
  select(lsoa11cd)

lookup <- read_csv("https://opendata.arcgis.com/datasets/500d4283cbe54e3fa7f358399ba3783e_0.csv") %>% 
  filter(LAD17CD == "E08000009") %>% 
  select(lsoa11cd = LSOA11CD, area_code = WD17CD, area_name = WD17NM)

best_fit <- lsoa %>% 
  left_join(., lookup, by = "lsoa11cd") %>% 
  group_by(area_code, area_name) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

# plot map
p <- map(best_fit$area_name,
         function(x) {
           ggplot() +
             geom_sf(data = filter(wards, area_name == x), fill = "#DDDDCC", alpha = 1, colour = "#FFFFFF", size = 1.2) +
             geom_sf(data = filter(best_fit, area_name == x), fill = NA, colour = "#212121",  size = 0.5, linetype = "dotted") +
             labs(title = x, subtitle = NULL, caption = NULL, x = NULL, y = NULL) +
             coord_sf(crs = st_crs(4326), datum = NA) +
             theme_void(base_family = "Roboto") +
             theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
                   strip.text = element_text(family = "Open Sans", size = 10, colour = "#757575", face = "plain"))
         })

title <- ggdraw() + draw_label("Best-fit wards", fontface = 'bold')
plot <- plot_grid(title, plotlist = p, nrow = 4)

# write results
ggsave("output/trafford_best-fit_wards.png", dpi = 300, scale = 1.2)

