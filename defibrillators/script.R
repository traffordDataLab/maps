## Defibrillator locations in Trafford

# load libraries --------------------------------
library(sf) ; library(leaflet) ; library(htmltools) ; library(htmlwidgets)

# load data --------------------------------
trafford <- st_read("https://github.com/traffordDataLab/boundaries/raw/master/trafford/trafford_local_authority.geojson")
defibrillators <- st_read("https://github.com/traffordDataLab/open_data/raw/master/defibrillators/trafford_defibrillators.geojson")

# town centre locations --------------------------------
a <- st_point(c(-2.323696, 53.425589)) # Sale
b <- st_point(c(-2.308697, 53.446673)) # Stretford
c <- st_point(c(-2.374682, 53.448751)) # Urmston
d <- st_point(c(-2.348894, 53.386907)) # Altrincham
e <- st_point(c(-2.428069, 53.418401)) # Partington and Carrington
f <- st_point(c(-2.271703, 53.461666)) # Old Trafford
g <- st_point(c(-2.319255, 53.396747)) # Timperley
point_geometry <- st_sfc(a, b, c, d, e, f, g, crs = 4326)
point_attributes <- data.frame(
  name = c("Sale", "Stretford", "Urmston", "Altrincham", "Partington and Carrington", "Old Trafford", "Timperley"))
town_centres <- st_sf(point_attributes, geometry = point_geometry)

# create icon --------------------------------
icon_defib <- makeAwesomeIcon(icon = "heartbeat", library = "fa", markerColor = "red", iconColor = "#fff")

# load map --------------------------------
map <- leaflet(height = "100%", width = "100%") %>% 
  addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
           attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="http://cartodb.com/attributions">CartoDB</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2017)</a>',
           group = "CartoDB",
           options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>% 
  addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
           attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>  | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2017)</a>',
           group = "OpenStreetMap",
           options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>% 
  addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}", 
           attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community | <a href="https://www.ons.gov.uk/methodology/geography/licences"> Contains OS data © Crown copyright and database right (2017)</a>', 
           group = "Satellite",
           options = providerTileOptions(minZoom = 10, maxZoom = 14)) %>%
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addLayersControl(position = 'topleft',
                   baseGroups = c("CartoDB", "OpenStreetMap", "Satellite"),
                   options = layersControlOptions(collapsed = TRUE)) %>% 
  addPolygons(data = trafford, fillColor = "#757575", stroke = TRUE, weight = 3, color = "#212121", opacity = 1) %>% 
  addAwesomeMarkers(data = defibrillators, icon = icon_defib, options = markerOptions(riseOnHover = TRUE, opacity = 0.75,
                    popup = sprintf(
                      "<b>%s</b><hr/>%s<br/>%s<br/>%s<br/>%s<br/>", 
                      htmlEscape(defibrillators$Location),
                      htmlEscape(defibrillators$Address1),
                      htmlEscape(defibrillators$Address2),
                      htmlEscape(defibrillators$Address3),
                      htmlEscape(defibrillators$Postcode)))) %>% 
  addLabelOnlyMarkers(data = town_centres, label = ~as.character(name), 
                      labelOptions = labelOptions(noHide = T, textOnly = T, direction = "bottom",
                                                  style = list(
                                                    "color"="white",
                                                    "text-shadow" = "-1px -1px 10px #757575, 1px -1px 10px #757575, 1px 1px 10px #757575, -1px 1px 10px #757575"))) %>% 
  addControl("<strong>Defibrillator locations in Trafford</strong><br><br><span>The map shows the locations of defibrillators across Trafford, (last updated 24th Jan 2017). You can select the individual defibrillator icons to view more information about them. The locations may not be exact in all cases as they are derived from postcodes.</span></br>",
             position='topright', className = "info") 

# add some CSS styles --------------------------------
browsable(
  tagList(list(
    tags$head(
      tags$style("
                 html, body {height: 100%;margin: 0;}
                 .leaflet-control-layers-toggle {height: 44; width: 44;}
                 .leaflet-bar a, .leaflet-bar a:hover, .leaflet-touch .leaflet-bar a, .leaflet-touch .leaflet-bar a:hover {height: 34px; width: 34px; line-height: 34px;}
                 .info {width: 300px;}")
      ),
    map
  ))
)

# save as HTML --------------------------------
saveWidget(map, "defribrillators.html")