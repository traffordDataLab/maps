# Map of hybrid working locations#

# Source: Trafford Council


library(tidyverse)
library(sf) 
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets) 
library(htmltools)

# read locations 
df <- read_csv("data/locations_hybrid_working.csv") %>%
  mutate(`Opening Times` = gsub(", ", "<br />",`Opening Times`))

# create HTML popup
locations <- df %>% 
  mutate(popup = paste0("<h3>",Location, "</h3>",
                        "<address>",
                        if_else(is.na(Address), "", Address), "<br />",
                        if_else(is.na(Email), "", str_c("<a href='mailto:", Email, "'>", Email, "</a><br />")),
                        if_else(is.na(Telephone), "", Telephone),
                        "</address>",
                        if_else(is.na(`Opening Times`), "", `Opening Times`))) 


local_authority <- st_read("data/local_authority.geojson")



# build map

map <- leaflet(height = "100%", width = "100%") %>% 
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2021)</a> | Source: <a href="https://democratic.trafford.gov.uk/mgMemberIndex.aspx?FN=WARD&VW=TABLE&PIC=1" target="_blank">Trafford Council</a>',
           options = tileOptions(minZoom = 12, maxZoom = 17)) %>%
  addPolygons(data = local_authority, fillColor = "#CCCCCC", weight = 0.8, opacity = 1, color = "#212121") %>% 
  addAwesomeMarkers(data = locations, lng = ~lon, lat = ~lat, popup = ~popup, label = ~Location, icon = awesomeIcons(icon = "fas fa-circle", library = "fa", iconColor = "#FFFFFF", markerColor = "blue")) %>%

  
  addControl(paste0("<h1>Locations for hybrid working</h1>"), position = 'topright',  className = "map-title") %>% 
  onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, viewport-fit=cover\">\'",");}")) %>%
  onRender(paste0("function(el, x) {$('head').append(","\'<link rel=\"stylesheet\" href=\"https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto\"/>\'",");}"))

# add page title and CSS
browsable(
  tagList(list(
    tags$html(lang = "en-GB"),
    tags$head(
      tags$title("Locations for hybrid working"),
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
