## 2025 Index of Multiple Deprivation ##

library(sf) ; library(tidyverse) ; library(jsonlite) ; library(ggspatial) ; library(shadowtext) ; library(janitor)

id <- "Trafford"

# English Indices of Deprivation 2025 #
# Source: Ministry of Housing, Communities and Local Government
# Publisher URL: https://www.gov.uk/government/statistics/announcements/english-indices-of-deprivation-2025
# Licence: Open Government Licence 3.0

imd <- read_csv("https://assets.publishing.service.gov.uk/media/68ff5daabcb10f6bf9bef911/File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv") %>% 
  clean_names() %>% 
  filter(local_authority_district_name_2024 == id) %>% 
  select(lsoa21cd = 1, 5:34) %>% 
  gather(variable, value, -lsoa21cd) %>% 
  mutate(measure = case_when(str_detect(variable, "score") ~ "score", 
                             str_detect(variable, "decile") ~ "decile", 
                             str_detect(variable, "rank") ~ "rank"),
         index_domain = case_when(str_detect(variable, "index_of_multiple_deprivation") ~ "Index of Multiple Deprivation", 
                                  str_detect(variable, "employment") ~ "Employment Deprivation",
                                  str_detect(variable, "education") ~ "Education, Skills and Training Deprivation",
                                  str_detect(variable, "health") ~ "Health Deprivation and Disability",
                                  str_detect(variable, "crime") ~ "Crime",
                                  str_detect(variable, "barriers") ~ "Barriers to Housing and Services",
                                  str_detect(variable, "living") ~ "Living Environment Deprivation",
                                  str_detect(variable, "idaci") ~ "Income Deprivation Affecting Children",
                                  str_detect(variable, "idaopi") ~ "Income Deprivation Affecting Older People",
                                  TRUE ~ "Income Deprivation")) %>% 
  select(lsoa21cd,
         measure,
         value,
         index_domain) %>% 
  spread(measure, value) %>% 
  mutate(year = "2025")

# Statistical and administrative geographies #

# Source: ONS Open Geography Portal 
# Publisher URL: http://geoportal.statistics.gov.uk/
# Licence: Open Government Licence 3.0

# LSOA Generalised Clipped (V5) boundaries
lsoa <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/FeatureServer/0/query?where=UPPER(lsoa21nm)%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=lsoa21cd,lsoa21nm&outSR=4326&f=geojson"))

# Electoral Ward to Local Authority District (LAD) Lookup in UK
codes <- fromJSON(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD25_LAD25_UK_LU_v2/FeatureServer/0/query?where=LAD25NM%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=WD25CD&outSR=4326&f=json"), flatten = TRUE) %>% 
  pluck("features") %>% 
  as_tibble() %>% 
  distinct(attributes.WD25CD) %>% 
  pull(attributes.WD25CD)

# Electoral Ward Generalised Clipped (V2) boundaries
wards <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD_MAY_2025_UK_BGC_V2/FeatureServer/0/query?where=", 
                        URLencode(paste0("wd25cd IN (", paste(shQuote(codes), collapse = ", "), ")")), 
                        "&outFields=wd25cd,wd25nm,long,lat&outSR=4326&f=geojson")) 

# Join IMD 2025 to LSOA boundaries
sf <- left_join(lsoa %>% clean_names(), filter(imd, 
                             index_domain == "Index of Multiple Deprivation"), by = "lsoa21cd")

# Plot map
ggplot() +
  geom_sf(data = sf, aes(fill = factor(decile)), alpha = 0.8, colour = "#FFFFFF", linewidth = 0.2) +
  geom_sf(data = wards , fill = NA, alpha = 1, colour = "#212121",  linewidth = 0.8) +
  geom_shadowtext(data = wards %>% clean_names(), aes(x = long, y = lat, label = wd25nm), colour = "#FFFFFF", family = "Open Sans", fontface = "bold", size = 3, bg.colour = "#212121", nudge_y = 0.002) +
  scale_fill_manual(breaks = 1:10,
                    values = c("#453B52", "#454F69", "#3F657E", "#317B8D", "#239296", "#26A898", "#43BD93", "#6AD189", "#98E37D", "#CAF270"),
                    labels = c("Most\ndeprived", 2:9, "Least\ndeprived")) +
  annotation_scale(location = "bl", style = "ticks", line_col = "#212121", text_col = "#212121") +
  annotation_north_arrow(height = unit(0.8, "cm"), width = unit(0.8, "cm"), location = "tr", which_north = "true") +
  labs(title = "Index of Multiple Deprivation (2025)",
       subtitle = "Trafford Lower-layer Super Output Areas by decile",
       caption = "Source: 2025 Indices of Deprivation, MHCLG | @traffordDataLab\n Contains Ordnance Survey data © Crown copyright and database right 2025",
       x = NULL, y = NULL,
       fill = "") +
  coord_sf(crs = st_crs(4326), datum = NA) +
  theme_void(base_family = "Roboto") +
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        text = element_text(colour = "#212121"),
        plot.title = element_text(size = 18, face = "bold", colour = "#757575", margin = margin(t = 15), vjust = 4),
        plot.subtitle = element_text(size = 12, face = "plain", colour = "#757575", margin = margin(b = 5)),
        plot.caption = element_text(size = 10, colour = "#212121", margin = margin(b = 15), vjust = -4),
        plot.background = element_rect(fill = 'white', colour = 'white'),
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
ggsave("output/trafford_imd25.png", dpi = 300, scale = 1, width = 3549, height = 3320, units = "px")

