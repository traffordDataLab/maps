# Trafford localities
# 2023-04-20

# New locality boundaries based on the new wards which come into effect from 2023-05-04

# Source: Trafford Council Planning Department.
# URL: https://www.trafford.gov.uk
# Licence: Open Government Licence v3.0

# load libraries ---------------------------
library(tidyverse) ; library(sf); library(nngeo); library(ggrepel)


## The latest version of Trafford's wards come into effect from 04 May 2023 and have been provided by the Trafford Council Planning department. These will be available from the OS Open Geography Portal in time
wards <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2023/trafford_ward_full_resolution.geojson")

localities <- wards %>% 
    # Assign the wards into their relevant locality names and collapse the internal boundaries into a single area for each
    mutate(area_name = case_when(
        area_name %in% c("Ashton upon Mersey", "Brooklands", "Manor", "Sale Central", "Sale Moor") ~ "Central",
        area_name %in% c("Gorse Hill & Cornbrook", "Longford", "Lostock & Barton", "Old Trafford", "Stretford & Humphrey Park") ~ "North",
        area_name %in% c("Altrincham", "Bowdon", "Broadheath", "Hale", "Hale Barns & Timperley South", "Timperley Central", "Timperley North") ~ "South",
        area_name %in% c("Bucklow-St Martins", "Davyhulme", "Flixton", "Urmston") ~ "West")) %>% 
    group_by(area_name) %>% 
    summarise() %>%
    
    # Calculate and store the coordinates of each locality's centroid as a "lat" and "lon" property
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    
    # Some "artifacts" (small polygons) seem to be present within the Central and South localities - probably loose ends when creating the ward boundaries. This removes them    
    st_remove_holes()

map_plot <- ggplot() +
                geom_sf(data = wards, fill = "#cce3aa", colour = "#757575", alpha = 1, linewidth = 0.1) +
                geom_sf(data = localities, fill = "transparent", colour = "#ffffff", linewidth = 1.2) +
                geom_text(data = wards,
                          mapping = aes(x = lon, y = lat, label = str_wrap(area_name, width = 15), group = area_name),
                          color = "#212121", size = 2.75) +
                geom_text_repel(data = localities, 
                                mapping = aes(x = lon, y = lat, label = area_name),
                                colour = "#ffffff", bg.color = "#636363", bg.r = 0.08, size = 5, fontface = "bold", point.size = NA, nudge_y = -0.0023) +
                labs(x = NULL, y = NULL, title = "Trafford's localities", subtitle = "Based on wards from 04 May 2023",
                     caption = "Contains OS data © Crown copyright and database right (2023)\nTrafford Council | @traffordDataLab") +
                coord_sf(datum = NA) +
                theme_void(base_family = "Roboto") +
                theme(plot.title = element_text(size = 20, face = "bold"),
                      plot.subtitle = element_text(size = 14),
                      axis.text = element_blank(),
                      legend.position = "none",
                      plot.margin = unit(c(0.25,0,0.25,0), "cm"))


# write image files ---------------------------
ggsave("output/trafford_localities.svg", plot = map_plot, dpi = 300, scale = 1, units = "px")
ggsave("output/trafford_localities.png", plot = map_plot, dpi = 300, scale = 1, units = "px")
ggsave("output/trafford_localities.jpg", plot = map_plot, dpi = 300, scale = 1, units = "px")
