#  2011 Output Area Classification map #

# load libraries ------------------------------
library(tidyverse) ; library(sf) ; library(ggtext) ; library(ggsci)

# Create a string object with the name of the local authority ------------------------------
id <- "Trafford"

# load data ------------------------------

# ONS 2011 Census Output Area Classification 
# Source: ONS; University College London
# URL: https://www.ons.gov.uk/methodology/geography/geographicalproducts/areaclassifications/2011areaclassifications/datasets
# Pen portraits: https://www.ons.gov.uk/methodology/geography/geographicalproducts/areaclassifications/2011areaclassifications/penportraitsandradialplots

url <- "https://www.ons.gov.uk/file?uri=/methodology/geography/geographicalproducts/areaclassifications/2011areaclassifications/datasets/2011oacclustersandnamescsvv2.zip"
download.file(url, dest = "2011oacclustersandnamescsvv2.zip")
unzip("2011oacclustersandnamescsvv2.zip")
file.remove("2011oacclustersandnamescsvv2.zip")

oac <- read_csv("2011 OAC Clusters and Names csv v2.csv") %>% 
  filter(`Local Authority Name` == id) %>% 
  select(oa11cd = `Output Area Code`,
         supergroup = `Supergroup Name`,
         group = `Group Name`,
         subgroup = `Subgroup Name`)

# Geospatial data
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk

# Output Areas
oa <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_December_2011_Boundaries_EW_BFE/FeatureServer/0/query?where=LAD16NM%20%3D%20'TRAFFORD'&outFields=OA11CD,LAD16NM&outSR=4326&f=geojson") %>% 
  select(oa11cd = OA11CD) %>% 
  left_join(oac, by = "oa11cd")

# Local authority
lad <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_May_2020_UK_BGC_V3/FeatureServer/0/query?where=lad20nm%20like%20'%25", id, "%25'&outFields=lad20cd,lad20nm&outSR=4326&f=geojson"), quiet = TRUE)

# plot map ------------------------------
ggplot() +
  geom_sf(data = lad, lwd = 0) +
  geom_sf(data = oa, aes(fill = factor(supergroup)), 
          alpha = 1, lwd = 0, show.legend = FALSE) +
  scale_fill_d3() +
  labs(x = NULL, y = NULL,
       title = "2011 Area Classifications for Output Areas",
       subtitle = paste0("<span style = 'color:#757575;'>Trafford</span>"),
       caption = "Contains Ordnance Survey data © Crown copyright and database right 2020\nSource: ONS | UCL",
       fill = "OAC Supergroups") +
  coord_sf(datum = NA) +
  theme_minimal() +
  theme(plot.margin = unit(rep(0.5, 4), "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_markdown(size = 12, margin = margin(b = 20)),
        plot.caption = element_text(colour = "grey60", margin = margin(t = 20, b = -10)),
        strip.text = element_text(hjust = 0, face = "bold")) +
  facet_wrap(~supergroup, nrow = 2)

# write results ------------------------------
ggsave("output/trafford_OAC.png", dpi = 300, scale = 1)
