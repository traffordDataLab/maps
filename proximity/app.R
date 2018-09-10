## Promiximity between schools and fast food outlets ##

## load R packages ---------------------------
library(shiny); library(tidyverse) ; library(httr) ; library(geojsonio) ; library(jsonlite) ; library(sf) ; library(leaflet) ; library(leaflet.extras)

## load geospatial data ---------------------------

# Trafford Metropolitan District boundary
trafford <- st_read("https://www.traffordDataLab.io/spatial_data/local_authority/2016/trafford_local_authority_generalised.geojson")

# Trafford town centres
centres <- st_read("https://www.traffordDataLab.io/spatial_data/town_centres/trafford_town_centres.geojson")

# Schools and colleges
edu <- st_read("https://www.traffordDataLab.io/open_data/schools_and_colleges/trafford_schools_and_colleges.geojson")

# Fast food outlets
fast_food <- st_read("https://github.com/traffordDataLab/open_data/raw/master/fast_food_outlets/trafford_fast_food_outlets.geojson")

ui <- bootstrapPage(
  tags$head(includeCSS("styles_base.css"), includeCSS("styles_shiny.css"), includeCSS("styles_map.css"),
            tags$style(type = "text/css", "html, body {width:100%;height:100%}", ".leaflet-container {cursor:crosshair !important;}"),
            leafletOutput("map", width = "100%", height = "100%"),
            absolutePanel(id = "shinyControls", class = "panel panel-default controls", fixed = TRUE, draggable = FALSE,
                          fluidRow(
                            tags$h4("Measuring the proximity of schools to fast food outlets"),
                            tabsetPanel(
                              tabPanel("Controls",
                                       radioButtons(inputId = "measure",
                                                    label = NULL,
                                                    choices = c("Distance" = "distance", "Travel time" = "time"),
                                                    selected = "time"),
                                       conditionalPanel(
                                         condition = "input.measure == 'time'",
                                         selectInput(inputId = "mode",
                                                     label = NULL,
                                                     choices = c("Bike" = "cycling-regular", "Car" = "driving-car", "Walk" = "foot-walking"),
                                                     selected = "foot-walking"),
                                         sliderInput("time_slider", 
                                                     label = h5("Range (minutes)"), 
                                                     min = 5, max = 30, value = 5, step = 5, 
                                                     ticks = FALSE, post = " minutes")),
                                       uiOutput("range"),
                                       br(),
                                       downloadButton(outputId ='download', 
                                                      label = 'Download GeoJSON',
                                                      style = 'padding:4px; font-size:80%'),
                                       br(),
                                       tags$small("© Powered by ", tags$a(href="https://openrouteservice.org/", "openrouteservice"))),
                              tabPanel("Info",
                                       tags$br(),
                                       tags$p("How many fast food outlets are within a 5 minute walk or 500 metres of a particular school? This app will help you to find out."),
                                       tags$h4("Instructions:"),
                                       tags$p("Enter the name of a school into the search bar. Select the school's location on the map with the crosshairs and then choose a measure of reachability: distance or travel time. Toggle on the fast food outlets and count how many fall within your chosen distance or travel time polygon(s)."),
                                       tags$h4("Data:"),
                                       tags$p("The location of schools and colleges in Trafford derive from the", tags$a(href="https://get-information-schools.service.gov.uk/", "Department of Education."),
                                       "Fast food outlet locations were extracted from the ", 
                                       tags$a(href="http://ratings.food.gov.uk/open-data/en-GBFood", "Food Standards Agency.")))
)))))
                                            

server <- function(input, output, session) {
  
  output$range <- renderUI({
    if(input$measure == "distance"){
      sliderInput("distance_slider", 
                  label = h5("Range (kilometres)"), min = 0.5, max = 3, value = 0.5, step = 0.5, 
                  ticks = FALSE, post = " km")
      }else{
        NULL
        }
    })
  
  click <- eventReactive(input$map_click,{
    event <- input$map_click
    })
  
  iso <- reactive({
    if(input$measure == "distance"){
      param_profile <- "driving-car"
      param_range <- input$distance_slider
      param_interval <- 0.5
      param_units <- "km"
      }
    else {
      param_profile <- input$mode
      param_range <- input$time_slider*60
      param_interval <- 60*5
      param_units <- ""
      }
    
    request <- GET(url = "https://api.openrouteservice.org/isochrones?",
                   query = list(api_key = "58d904a497c67e00015b45fc6862cde0265d4fd78ec660aa83220cdb",
                                locations = paste0(click()$lng,",",click()$lat),
                                profile = param_profile,
                                range_type = input$measure,
                                range = param_range,
                                interval = param_interval,
                                units = param_units))
    
    content <- content(request, as = "text", encoding = "UTF-8")
    results <- fromJSON(txt = content)
    class(results) <- "geo_list"
    sf <- geojson_sf(results) %>% arrange(desc(value))
    })

  observeEvent(input$reset, {
    leafletProxy("map") %>% clearGroup("isochrones")
    })
  
  output$map <- renderLeaflet({
    
    fast_food_pop <- paste0("<span style='text-align: center; font-size: 15px;'><strong>", fast_food$name, "</strong></span>")
    
    html_logo <- "<a href='https://trafforddatalab.github.io' target='_blank'><img src='https://trafforddatalab.github.io/assets/logo/trafforddatalab_logo.svg' style='width: 93px; border: 0;' alt='Trafford Data Lab' border='0'></a>"
    
    leaflet() %>% 
      addTiles(urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png",
               attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="http://cartodb.com/attributions">CartoDB</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2018)</a>',
               group = "Low Detail",
               options = providerTileOptions(minZoom = 10, maxZoom = 17)) %>% 
      setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
      addPolylines(data = trafford, stroke = TRUE, weight = 3, color = "#212121", opacity = 1) %>% 
      addLabelOnlyMarkers(data = centres, lng = ~lon, lat = ~lat, label = ~as.character(name), 
                          labelOptions = labelOptions(noHide = T, textOnly = T, direction = "bottom",
                                                      style = list(
                                                        "color"="white",
                                                        "text-shadow" = "-1px -1px 10px #757575, 1px -1px 10px #757575, 1px 1px 10px #757575, -1px 1px 10px #757575"))) %>%
      addAwesomeMarkers(data = edu, label = ~Name, icon = ~makeAwesomeIcon(icon = "graduation-cap", library = "fa", iconColor = "#000000", markerColor = "blue"), group = "Schools and colleges", options = markerOptions(riseOnHover = TRUE, opacity = 0.75)) %>% 
      addAwesomeMarkers(data = fast_food, popup = fast_food_pop, icon = ~makeAwesomeIcon(icon = "pizza", library = "ion", iconColor = " #000000", markerColor = "lightred"), group = "Fast food outlets", options = markerOptions(riseOnHover = TRUE, opacity = 0.75)) %>% 
      addSearchFeatures(targetGroups = 'Schools and colleges',
                        options = searchFeaturesOptions(
                          zoom = 17,
                          openPopup = FALSE,
                          collapsed = FALSE,
                          position = "topleft",
                          textPlaceholder = "Search schools...")) %>% 
      addLayersControl(position = 'topleft',
                       overlayGroups = "Fast food outlets", 
                       options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup("Fast food outlets") %>% 
      addControl(html = html_logo, position = "bottomright") %>% 
      htmlwidgets::onRender(
        " function(el, t) {
        var myMap = this;
        myMap._container.style['background'] = '#ffffff';}")
    })

  
  observe({
    
    factpal <- colorFactor(palette = c( "#7f2704","#a63603","#d94801","#f16913","#fd8d3c","#fdae6b"), 
                           levels = factor(iso()$value), ordered = TRUE)

    map <- leafletProxy('map') %>%
      clearGroup("isochrones") %>% 
      addPolygons(data = iso(),
                  fill = TRUE, fillColor = "#ffffff", fillOpacity = 0.1,
                  stroke = TRUE, opacity = 1, color = ~factpal(iso()$value), weight = 6, 
                  dashArray = "1,13", options = pathOptions(lineCap = "round"),
                  label = if(input$measure == "time"){
                    as.character(paste0(iso()$value/60, " minutes"))
                  }else{
                    as.character(paste0(iso()$value/1000, "km"))
                  },
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
                  highlight = highlightOptions(weight = 6, color = "#FFFF00", fillOpacity = 0, bringToFront = FALSE),
                  group = "isochrones") %>%
      addAwesomeMarkers(data = click(), lat = ~lat, lng = ~lng,
                        icon = if(input$measure == "time" & input$mode == "cycling-regular"){
                          ~makeAwesomeIcon(icon = "bicycle", library = "fa", iconColor = "black", markerColor = "white")
                        } else if(input$measure == "time" & input$mode == "driving-car"){
                          ~makeAwesomeIcon(icon = "car", library = "fa", iconColor = "black", markerColor = "white")
                        } else if(input$measure == "time" & input$mode == "foot-walking") {
                          ~makeAwesomeIcon(icon = "male", library = "fa", iconColor = "black", markerColor = "white")
                        } else if(input$measure == "distance"){
                          ~makeAwesomeIcon(icon = "road", library = "fa", iconColor = "black", markerColor = "white")
                        } else {  
                          return()
                        },
                        group = "isochrones")
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste("export.geojson")
    },
    content = function(file) {
      st_write(iso(), file, driver = "GeoJSON")
    }
    )

}

shinyApp(ui, server)