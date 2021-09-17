## Trafford's population ##

# load libraries ---------------------------------------------------------------
library(sf) ; library(tidyverse) ; library(ggplot2) ; library(ggspatial) ; library(shadowtext) ; library(viridis)

# load mid-2020 population estimates

# Source: Nomis
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
pop <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1656750701...1656750715,1656750717,1656750716,1656750718...1656750721&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(area_code = GEOGRAPHY_CODE, n = OBS_VALUE)

# load geospatial data ---------------------------------------------------------

# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-ew-bgc
lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv") %>% 
  filter(LAD19NM == "Trafford") %>% 
  pull(WD19CD)

wards <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2019_Boundaries_EW_BGC/MapServer/0/query?where=", 
                        URLencode(paste0("wd19cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
                        "&outFields=wd19cd,wd19nm,long,lat&outSR=4326&f=geojson")) %>% 
  select(area_code = wd19cd, area_name = wd19nm, lon = long, lat) %>% 
  left_join(., pop, by = "area_code")

localities <- st_read("https://www.traffordDataLab.io/spatial_data/council_defined/trafford_localities.geojson")

# plot map ---------------------------------------------------------------------
ggplot() +
  geom_sf(data = wards, aes(fill = n), alpha = 1, colour = "#FFFFFF",  size = 0.5) +
  geom_sf(data = localities, fill = NA, colour = "#212121",  size = 1) +
  geom_shadowtext(data = wards, aes(x = lon, y = lat, label = area_name), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 2.5, bg.colour = "#212121", nudge_y = 0.002) +
  geom_shadowtext(data = localities, aes(x = lon, y = lat, label = locality), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 4, bg.colour = "#212121", nudge_y = -0.002) +
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
  labs(title = "Trafford's resident population (2020)",
       subtitle = NULL,
       caption = "Source: Mid-2020 population estimates, ONS | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2021",
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
ggsave("output/trafford_population_2020.png", dpi = 300, scale = 1)
