library(tidyverse) ; library(sf)
library(nngeo)

wards <- st_read("https://www.trafforddatalab.io/spatial_data/ward/2023/trafford_ward_full_resolution.geojson") %>%
  mutate(area_name = gsub(" Ward", "", area_name))

localities <- wards %>% 
  mutate(area_name = 
           case_when(
             area_name %in% c("Ashton upon Mersey", "Brooklands","Sale Central", "Sale Moor", "Manor") ~ "Central",
             area_name %in% c("Old Trafford", "Gorse Hill & Cornbrook", "Lostock & Barton", "Longford", "Stretford & Humphrey Park") ~ "North",
             area_name %in% c("Altrincham", "Bowdon", "Broadheath", "Hale Barns & Timperley South", "Hale", "Timperley Central", "Timperley North") ~ "South",
             area_name %in% c("Bucklow-St Martins", "Davyhulme", "Urmston", "Flixton", "Urmston") ~ "West"))  %>%
  group_by(area_name) %>% 
  summarise() %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  st_remove_holes()

#st_write(localities, "localities_2023.geojson", driver = "GeoJSON")

ggplot() +
  geom_sf(data = wards, fill = "#DDDDCC", colour = "#ffffff" ,alpha = 1, size = 0.7) +
  geom_text(data = wards,
            aes(x = lon, y = lat, label = str_wrap(area_name, width = 15), group = area_name),
            color = "#212121", size = 3, fontface = "bold",
            box.padding = 0, point.padding = 0) +
  geom_sf(data = localities, fill = "transparent", colour = "#757575", size = 1.4) +
  geom_text(data = localities, mapping = aes(x = lon, y = lat, label = area_name), colour = "#757575", fontface = "bold", size = 5) +
  geom_text(data = wards,
            aes(x = lon, y = lat, label = str_wrap(area_name, width = 15), group = area_name),
            color = "#212121", size = 3, fontface = "bold",
            box.padding = 0, point.padding = 0) +
  labs(x = NULL, y = NULL, title = "Trafford's 2023 localities and wards",
       caption = "Contains OS data © Crown copyright and database right (2023). Source: Trafford Council\n@traffordDataLab") +
  coord_sf(datum = NA) +
  theme_void() +
  theme(plot.title = element_text(size = 18),
        axis.text = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"))

ggsave(file = "output/trafford_wards&localities_2023.png", dpi = 300, scale = 1)
