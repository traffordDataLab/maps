## Trafford's population map 2022##

# load libraries ---------------------------------------------------------------
library(httr) ; library(readxl) ; library(tidyverse) ; library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext) ; library(viridis)

# load mid-2022 population estimates

# Source: Mid-year estimates 2022. Ward-level population estimates (official statistics in development
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental
# Licence: Open Government Licence

tmp <- tempfile(fileext = ".xlsx")


GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2021andmid2022/sapewardstablefinal.xlsx",
    write_disk(tmp))

df <- read_xlsx(tmp, sheet = 8, skip = 3) %>%
  filter(`LAD 2023 Code` == "E08000009") %>%
  select(area_code = `Ward 2023 Code`, n = Total)

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
  labs(title = "Trafford's resident population (2022)",
       subtitle = NULL,
       caption = "Source: Mid-2020 population estimates, ONS | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2024",
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
ggsave("output/trafford_population_2022.png", dpi = 300, scale = 1)
