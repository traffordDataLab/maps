# Trafford localities/neighbourhoods visualised for the Trafford Locality Plan (Health and Care Strategy)
# 2024-11-08
# Licence: Open Government Licence v3.0

# "Trafford red" colour sourced from: https://gmintegratedcare.org.uk/my-borough/trafford/ this will be used as branding within the plan.


# load libraries ---------------------------
library(tidyverse) ; library(sf); library(nngeo); library(ggrepel);


## The latest version of Trafford's wards come into effect from 04 May 2023 and have been provided by the Trafford Council Planning department. These will be available from the OS Open Geography Portal in time
wards <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2023/trafford_ward_full_resolution.geojson") %>%
    mutate(locality_name = case_when(
        area_name %in% c("Ashton upon Mersey", "Brooklands", "Manor", "Sale Central", "Sale Moor") ~ "Central",
        area_name %in% c("Gorse Hill & Cornbrook", "Longford", "Lostock & Barton", "Old Trafford", "Stretford & Humphrey Park") ~ "North",
        area_name %in% c("Altrincham", "Bowdon", "Broadheath", "Hale", "Hale Barns & Timperley South", "Timperley Central", "Timperley North") ~ "South",
        area_name %in% c("Bucklow-St Martins", "Davyhulme", "Flixton", "Urmston") ~ "West"))

localities <- wards %>% 
    # Collapse the internal ward boundaries into a single area for each locality
    group_by(locality_name) %>% 
    summarise() %>%
    rename(area_name = locality_name) %>%
    
    # Calculate and store the coordinates of each locality's centroid as a "lat" and "lon" property
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    
    # Some "artifacts" (small polygons) seem to be present within the Central and South localities - probably loose ends when creating the ward boundaries. This removes them    
    st_remove_holes()


# Function to create the plots ---------------------------
# This function creates a plot of all localities but with the chosen one highlighted
create_locality_plot <- function(locality) {
    ggplot() +
    geom_sf(data = wards, fill = "#ddd", colour = "#757575", alpha = 1, linewidth = 0.1) +
    geom_sf(data = localities %>% filter(area_name != locality), fill = "transparent", colour = "#ffffff", linewidth = 1.2) +
    geom_sf(data = localities %>% filter(area_name == locality), fill = "transparent", colour = "#c7001e", linewidth = 1.2) +
    geom_text_repel(data = localities %>% filter(area_name == locality),
                    mapping = aes(x = lon, y = lat, label = area_name),
                    colour = "#212121", bg.color = "#fff", bg.r = 0.08, size = 5, fontface = "bold", point.size = NA, nudge_y = -0.0023) +
    labs(x = NULL, y = NULL, title = NULL,
         caption = "Contains OS data © Crown copyright and database right (2023)\nTrafford Council | @traffordDataLab") +
    coord_sf(lims_method = "geometry_bbox") +
    theme_void(base_family = "Roboto") +
    theme(plot.title = element_blank(),
          axis.text = element_blank(),
          legend.position = "none")
}

# This function just creates a plot of the chosen individual locality
create_individual_locality_plot <- function(locality) {
    ggplot() +
        geom_sf(data = wards %>% filter(locality_name == locality), fill = "#ddd", colour = "#757575", alpha = 1, linewidth = 0.1) +
        geom_sf(data = localities %>% filter(area_name == locality), fill = "transparent", colour = "#c7001e", linewidth = 1.2) +
        geom_text_repel(data = localities %>% filter(area_name == locality),
                        mapping = aes(x = lon, y = lat, label = area_name),
                        colour = "#212121", bg.color = "#fff", bg.r = 0.08, size = 5, fontface = "bold", point.size = NA) +
        labs(x = NULL, y = NULL, title = NULL,
             caption = "Contains OS data © Crown copyright and database right (2023)\nTrafford Council | @traffordDataLab") +
        coord_sf(lims_method = "geometry_bbox") +
        theme_void(base_family = "Roboto") +
        theme(plot.title = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
}

# Create the image files ---------------------------
ggsave("output/trafford_localities_North.png", plot = create_locality_plot("North"), dpi = 300, scale = 1, units = "px", bg = "#fff")
ggsave("output/trafford_localities_West.png", plot = create_locality_plot("West"), dpi = 300, scale = 1, units = "px", bg = "#fff")
ggsave("output/trafford_localities_Central.png", plot = create_locality_plot("Central"), dpi = 300, scale = 1, units = "px", bg = "#fff")
ggsave("output/trafford_localities_South.png", plot = create_locality_plot("South"), dpi = 300, scale = 1, units = "px", bg = "#fff")

ggsave("output/trafford_north_locality_individual.png", plot = create_individual_locality_plot("North"), dpi = 300, scale = 1, units = "px")
ggsave("output/trafford_west_locality_individual.png", plot = create_individual_locality_plot("West"), dpi = 300, scale = 1, units = "px")
ggsave("output/trafford_central_locality_individual.png", plot = create_individual_locality_plot("Central"), dpi = 300, scale = 1, units = "px")
ggsave("output/trafford_south_locality_individual.png", plot = create_individual_locality_plot("South"), dpi = 300, scale = 1, units = "px")

