## 2019 Index of Multiple Deprivation ##

library(sf) ; library(tidyverse) ; library(jsonlite) ; library(ggspatial) ; library(shadowtext)

id <- "Trafford"

# English Indices of Deprivation 2019 #
# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2019
# Licence: Open Government Licence 3.0

imd <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833982/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators.csv") %>% 
  clean_names() %>% 
  filter(local_authority_district_name_2019 == id) %>% 
  select(lsoa11cd = 1, 5:34) %>% 
  gather(variable, value, -lsoa11cd) %>% 
  mutate(measure = case_when(str_detect(variable, "score") ~ "score", 
                             str_detect(variable, "decile") ~ "decile", 
                             str_detect(variable, "rank") ~ "rank"),
         index_domain = case_when(str_detect(variable, "index_of_multiple_deprivation") ~ "Index of Multiple Deprivation", 
                                  str_detect(variable, "employment") ~ "Employment",
                                  str_detect(variable, "education") ~ "Education, Skills and Training",
                                  str_detect(variable, "health") ~ "Health Deprivation and Disability",
                                  str_detect(variable, "crime") ~ "Crime",
                                  str_detect(variable, "barriers") ~ "Barriers to Housing and Services",
                                  str_detect(variable, "living") ~ "Living Environment",
                                  str_detect(variable, "idaci") ~ "Income Deprivation Affecting Children",
                                  str_detect(variable, "idaopi") ~ "Income Deprivation Affecting Older People",
                                  TRUE ~ "Income")) %>% 
  select(lsoa11cd,
         measure,
         value,
         index_domain) %>% 
  spread(measure, value) %>% 
  mutate(year = "2019")

# Statistical and administrative geographies #

# Source: ONS Open Geography Portal 
# Publisher URL: http://geoportal.statistics.gov.uk/
# Licence: Open Government Licence 3.0
lsoa <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson")) 

codes <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD18_LAD18_UK_LU/FeatureServer/0/query?where=LAD18NM%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=WD18CD,LAD18NM&outSR=4326&f=json"), flatten = TRUE) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  distinct(attributes.WD18CD) %>% 
  pull(attributes.WD18CD)

wards <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2018_Boundaries_V3/MapServer/2/query?where=", 
                        URLencode(paste0("wd18cd IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                        "&outFields=wd18cd,wd18nm,long,lat&outSR=4326&f=geojson"))

# Join IMD 2019 to LSOA boundaries
sf <- left_join(lsoa, filter(imd, 
                             index_domain == "Index of Multiple Deprivation"), by = "lsoa11cd")

# Plot map
ggplot() +
  geom_sf(data = sf, aes(fill = factor(decile)), alpha = 0.8, colour = "#FFFFFF", size = 0.2) +
  geom_sf(data = wards, fill = NA, alpha = 1, colour = "#212121",  size = 0.8) +
  geom_shadowtext(data = wards, aes(x = long, y = lat, label = wd18nm), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", nudge_y = 0.002) +
  scale_fill_manual(breaks = 1:10,
                    values = c("#453B52", "#454F69", "#3F657E", "#317B8D", "#239296", "#26A898", "#43BD93", "#6AD189", "#98E37D", "#CAF270"),
                    labels = c("Most\ndeprived", 2:9, "Least\ndeprived")) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Index of Multiple Deprivation (2019)",
       subtitle = "Trafford Lower-layer Super Output Areas by decile",
       caption = "Source: 2019 Indices of Deprivation, MHCLG | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2019",
       x = NULL, y = NULL,
       fill = "") +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.subtitle = element_text(size = 12, face = "plain", colour = "#757575", margin = margin(b = 5)),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4),
        legend.title = element_text(colour = "#757575"),
        legend.text = element_text(colour = "#757575"),
        legend.position = c(0.16, 0.95)) +
  guides(fill = guide_legend(label.position = "bottom", 
                             label.hjust = 0,
                             direction = "horizontal",
                             nrow = 1,
                             keyheight = unit(2, units = "mm"), 
                             keywidth = unit(5, units = "mm")))

# Write output
ggsave("output/trafford_imd19.png", dpi = 300, scale = 1)

