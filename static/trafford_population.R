## Trafford's population map mid-2024 estimates ##

# load libraries ---------------------------------------------------------------
library(tidyverse) ; library(sf) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext) ; library(viridis) ; library(jsonlite)

# load mid-2024 population estimates

# Source: ONS Population estimates - small area (2021 based) by single year of age - England and Wales for mid-2024
# URL: https://www.nomisweb.co.uk/query/construct/summary.asp?mode=construct&version=0&dataset=2014
# Licence: Open Government Licence
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/NM_2014_1.data.csv?geography=763369116...763369136&date=latest&gender=0&c_age=200&measures=20100") %>%
    select(area_code = GEOGRAPHY_CODE, n = OBS_VALUE)

# load geospatial data ---------------------------------------------------------

# Source: ONS Open Geography Portal

codes <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD23_LAD23_UK_LU_DEC/FeatureServer/0/query?where=LAD23NM%20%3D%20'", URLencode(toupper("Trafford"), reserved = TRUE), "'&outFields=WD23CD,WD23NM,LAD23CD,LAD23NM&outSR=4326&f=json"), flatten = TRUE) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  distinct(attributes.WD23CD) %>% 
  pull(attributes.WD23CD) 

wards <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Wards_December_2023_Boundaries_UK_BFE/FeatureServer/0/query?where=", 
                        URLencode(paste0("WD23CD IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                        "&outFields=WD23CD,WD23NM,LONG,LAT&outSR=4326&f=json")) %>%
  select(area_code = WD23CD, area_name = WD23NM, lon = LONG, lat = LAT) %>%
  left_join(df, by = "area_code") %>%
  mutate(label = paste0(str_wrap(area_name, width = 15),"\n",format(n, big.mark = ",")))

localities <- st_read("https://www.traffordDataLab.io/spatial_data/council_defined/2023/trafford_localities_full_resolution.geojson")

# plot map ---------------------------------------------------------------------
ggplot() +
  geom_sf(data = wards, aes(fill = n), alpha = 1, colour = "#FFFFFF",  linewidth = 0.5) +
  geom_sf(data = localities, fill = NA, colour = "#212121",  linewidth = 1) +
  geom_shadowtext(data = wards, aes(label = label, geometry = geometry), stat = "sf_coordinates", colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 2.5, bg.colour = "#212121", nudge_y = 0.002) +
  geom_shadowtext(data = localities, aes(x = lon, y = lat, label = area_name), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 4, bg.colour = "#212121", nudge_y = -0.002) +
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
  labs(title = "Trafford's resident population (2024)",
       subtitle = NULL,
       caption = "Source: Mid-2024 population estimates, ONS | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2026",
       x = NULL, y = NULL) +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#707070", margin = margin(t = 15), vjust = 4),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4),
        legend.title = element_text(colour = "#707070"),
        legend.text = element_text(colour = "#707070"),
        legend.position = c(0.18, 0.95))

# write results ----------------------------------------------------------------
ggsave("output/trafford_population_2024.png", dpi = 300, scale = 1, units = "px", width = 2574, height = 2154)
