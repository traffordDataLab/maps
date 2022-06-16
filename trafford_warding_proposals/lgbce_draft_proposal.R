# Trafford wards draft proposals#

# Source: Trafford Council & LGBCE


library(tidyverse)
library(sf) 
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets) 
library(htmltools)

# read layers data

Trafford_option_A <- st_read("data/Boundary_Review_Ward_Proposals_18082021.geojson") 

Trafford_option_B <- st_read("data/Boundary_Review_Ward_Proposals_20082021.geojson") 

lgbce_draft <- st_read("data/lgbce_draft.geojson") %>%
  st_transform(4326)

Trafford_wards <- st_read("data/electoral_ward.geojson") 

lgbce_final_proposals <- st_read("data/Trafford_final_proposals.geojson") 

polling_districts <- st_read("https://www.trafforddatalab.io/spatial_data/polling_districts/trafford_polling_districts.geojson")

polling_stations <- st_read("https://www.trafforddatalab.io/spatial_data/polling_districts/trafford_polling_districts.geojson")

# build map

map <- leaflet(height = "100%", width = "100%") %>% 
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2021)</a> | Source: <a href="https://democratic.trafford.gov.uk/mgMemberIndex.aspx?FN=WARD&VW=TABLE&PIC=1" target="_blank">Trafford Council</a> , <a href="https://www.lgbce.org.uk/all-reviews/north-west/greater-manchester/trafford" target="_blank">LGBCE</a>',
           options = tileOptions(minZoom = 12, maxZoom = 17), group = "Low Detail") %>%
  addPolygons(data = Trafford_wards, fillColor = "#fc6721", weight = 3, fillOpacity = 0.05, opacity = 1, color = "#fc6721", label = ~area_name, group = "Trafford wards", highlightOptions = highlightOptions(color = "yellow", weight = 2, bringToFront = TRUE)) %>% 
  #addPolygons(data = Trafford_option_A, fillColor = "green", weight = 3, fillOpacity = 0.02, opacity = 1, color = "green", label = ~Ward, group = "Trafford option A") %>% 
  #addPolygons(data = Trafford_option_B, fillColor = "#2A81CB", weight = 3, fillOpacity = 0.05, opacity = 1, color = "#2A81CB", label = ~Ward, group = "Trafford option B") %>% 
  #addPolygons(data = lgbce_draft, fillColor = "#925544", weight = 3, fillOpacity = 0.05, opacity = 1, color = "#925544", label = ~WardName, group = "LGBCE draft") %>% 
  addPolygons(data = lgbce_final_proposals, fillColor = "#CCCCCC", weight = 3, fillOpacity = 0.05, opacity = 1, color = "#212121", label = ~Name, group = "LGBCE final", highlightOptions = highlightOptions(color = "yellow", weight = 2, bringToFront = TRUE)) %>%
  addPolygons(data = polling_districts, fillColor = "#925544", weight = 3, fillOpacity = 0.05, opacity = 1, color = "#925544", label = ~area_name, group = "Polling Districts", highlightOptions = highlightOptions(color = "yellow", weight = 2, bringToFront = TRUE)) %>% 
  
  

  addControl(paste0("<h1>Trafford warding proposals</h1>"), position = 'topright',  className = "map-title") %>% 
  addLayersControl(
    baseGroups = c("Low Detail"),
    #overlayGroups = c("Trafford wards", "Trafford option A","Trafford option B","LGBCE draft","LGBCE final"),
    overlayGroups = c("Trafford wards", "LGBCE final", "Polling Districts"),
    options = layersControlOptions(collapsed = FALSE)
   ) %>%
  
  #hideGroup("Trafford option A") %>%
  #hideGroup("Trafford option B") %>%
  #hideGroup("LGBCE draft") %>%
  hideGroup("LGBCE final") %>%
  hideGroup("Polling Districts") %>%
    onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, viewport-fit=cover\">\'",");}")) %>%
    onRender(paste0("function(el, x) {$('head').append(","\'<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto\"/>\'",");}"))

# add page title and CSS
browsable(
  tagList(list(
    tags$html(lang = "en-GB"),
    tags$head(
      tags$title("Trafford warding proposals"),
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
        
        address {
        display: block;
        margin: 0;
        padding: 0;
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
        
        .leaflet-popup {
        position: absolute;
        }
        
        /* Override Leaflet style to increase contrast */
        .leaflet-container a.leaflet-popup-close-button {
        color: #555;
        }
        .leaflet-container a.leaflet-popup-close-button:hover {
        color: #212121;
        }
        
        .leaflet-popup-content {
        margin: 0 1.5em 1.5em 1.5em;
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
