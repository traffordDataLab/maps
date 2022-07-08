# Trafford wards draft proposals#

# Source: Trafford Council & LGBCE


library(tidyverse)
library(sf) 
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets) 
library(htmltools)

# read layers data

lgbce_final_proposals <- st_read("data/Trafford_final_proposals.geojson") 

#polling_districts <- st_read("https://www.trafforddatalab.io/spatial_data/polling_districts/trafford_polling_districts.geojson")

polling_stations_raw <- st_read("https://www.trafforddatalab.io/open_data/elections/polling_stations/trafford_polling_stations.geojson") 

polling_stations <- polling_stations_raw %>%
  mutate(long = unlist(map(polling_stations_raw$geometry,1)),
         lat = unlist(map(polling_stations_raw$geometry,2))) %>%
  mutate(popup = str_c("<strong>", name, "</strong><br/>",
                address, "<br/>",
                postcode) %>% 
  map(HTML))

centroids_LGBCE <- st_centroid(lgbce_final_proposals) %>%
  mutate(long = unlist(map(geometry,1)),
         lat = unlist(map(geometry,2)))



# build map

# create icon --------------------------------
icon <- makeAwesomeIcon(icon = "fa-check-square-o", library = "fa", markerColor = "blue", iconColor = "#fff") #38A9DC

map <- leaflet(height = "100%", width = "100%") %>% 
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2021)</a> | Source: <a href="https://democratic.trafford.gov.uk/mgMemberIndex.aspx?FN=WARD&VW=TABLE&PIC=1" target="_blank">Trafford Council</a> , <a href="https://www.lgbce.org.uk/all-reviews/north-west/greater-manchester/trafford" target="_blank">LGBCE</a>',
           options = tileOptions(minZoom = 12, maxZoom = 17), group = "Low Detail") %>%
  
  addPolygons(data = lgbce_final_proposals, weight = 5, fillOpacity = 0, opacity = 1, color = "#212121", group = "LGBCE final") %>%
  
  addLabelOnlyMarkers(data = centroids_LGBCE, lng = ~long, lat = ~lat, label = ~as.character(Name), 
                      labelOptions = labelOptions(noHide = T, textOnly = T, direction = "top",
                                                  style = list(
                                                    "color"="white",
                                                    "text-shadow" = "-1px -1px 10px #555555, 1px -1px 10px #555555, 1px 1px 10px #555555, -1px 1px 10px #555555"))) %>%
  
  #addPolygons(data = polling_districts, weight = 3, fillOpacity = 0.0, opacity = 1, color = "#2A81CB", label = ~area_name, group = "Polling Districts",  highlightOptions = highlightOptions(color = "yellow", weight = 3, bringToFront = TRUE)) %>% ##2A81CB
  
  addAwesomeMarkers(data = polling_stations, ~long, ~lat, icon=icon, group = "Polling Stations", popup = ~popup) %>%
  
  addLabelOnlyMarkers(data = polling_stations, lng = ~long, lat = ~lat, label = ~name, group = "Polling Stations Labels",
                      labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE, direction = "bottom"
                                                  )) %>%
  
  groupOptions("Polling Stations Labels", zoomLevels = 15:17) %>%
  

  addControl(paste0("<h1>Trafford new wards and polling stations</h1>"), position = 'topright',  className = "map-title") %>% 
  
  # addLayersControl(
  #   overlayGroups = c("Polling Stations"),
  #   options = layersControlOptions(collapsed = FALSE)
  #  ) %>%
  
  addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                     autoCenter = TRUE, maxZoom = 60, 
                                     setView = TRUE)) %>%
  
  #hideGroup("Polling Districts") %>%

    onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}")) %>%
    onRender(paste0("function(el, x) {$('head').append(","\'<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto\"/>\'",");}"))

# add page title and CSS
browsable(
  tagList(list(
    tags$html(lang = "en-GB"),
    tags$head(
      tags$title("Trafford new wards and polling stations"),
      tags$style(
        "html, body {
        height: 100%; 
        margin: 0; 
        font-family: 'Open Sans', sans-serif;
        }
        
        main {
        display: block;
        box-sizing: border-box;
        width: 100%;
        height: 100%;
        }
        
        h1, h2, h3 {
        font-family: 'Roboto', sans-serif;
        color: #707070;
        }
        
        a {
        text-decoration: none;
        }
        
        a:hover {
        text-decoration: underline;
        }
        
        .leaflet-bar a.leaflet-disabled {
        color: #757575;
        }
        
        .leaflet-container a {
        color: #046dc3;
        }
        
        .leaflet-bar a {
        color: #212121;
        }
        
        /* Override Leaflet style to increase contrast */
        .leaflet-container a.leaflet-popup-close-button {
        color: #555;
        }
        .leaflet-container a.leaflet-popup-close-button:hover {
        color: #212121;
        }
        
        .leaflet-popup-content {
        margin: 0.5em 1.5em 0.5em 1.5em;
        min-width: 100px !important;
        max-height: 300px;
        overflow: auto;
        }
        
        .leaflet-tooltip {
        font-weight: bold;
        }

        .leaflet-control.map-title {
        background-color: transparent;
        }
        
        .leaflet-control.map-title h1 {
        text-shadow: -1px -1px #FFFFFF, 1px -1px #FFFFFF, -1px 1px #FFFFFF, 1px 1px #FFFFFF;
        padding: 0;
        margin: 0;
        }
        
        .leaflet-control-attribution {
        /* fixes text scaling issue when changing from portrait to landscape on mobile browsers */
        -webkit-text-size-adjust: 100%;
        -moz-text-size-adjust: 100%;
        -ms-text-size-adjust: 100%;  
        }
        
        .boldText {
        font-weight: bold;
        }"
      )
      )
      ),
    tags$main(map)
      )
    )
