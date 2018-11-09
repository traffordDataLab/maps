## Indices of Multiple Deprivation (2015) ##

library(shiny) ; library(tidyverse) ; library(sf) ; library(leaflet) ; library(htmltools)

# Load data ---------------------------

# IMD 2015 by LSOA (source: Department for Communities and Local Government)
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/imd_2015/IMD_2015_long.csv",
               col_types = cols(lsoa11cd = col_factor(NULL),
                                measure = col_factor(NULL), 
                                value = col_double(), 
                                index_domain = col_factor(NULL))) %>% 
  spread(measure, value) %>% 
  rename(score = Score, rank = Rank, decile = Decile)

# Lower-layer Super Output Area boundaries (source: ONS Open Geography Portal)
lsoa <- st_read("https://www.traffordDataLab.io/spatial_data/lookups/lsoa_to_ward_best-fit_lookup.geojson")

# Local Authority boundaries (source: ONS Open Geography Portal)
la <- st_read("https://www.traffordDataLab.io/spatial_data/local_authority/2016/gm_local_authority_generalised.geojson") %>% 
  rename(lad16nm = area_name)

ui <- bootstrapPage(
  tags$head(includeCSS("styles_base.css"), includeCSS("styles_shiny.css"), includeCSS("styles_map.css"),
            tags$style(type = "text/css", "html, body {width:100%;height:100%}")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, height = "auto",
                tags$h4("Indices of Multiple Deprivation"),
                fluidRow(
                  tabsetPanel(
                    tabPanel("Controls",
                             radioButtons(inputId = "domain", label = NULL,
                                          choices = c("Index of Multiple Deprivation", "Income", "Employment", "Education, Skills and Training", 
                                                      "Health Deprivation and Disability", "Crime", "Barriers to Housing and Services", "Living Environment"),
                                          selected = "Index of Multiple Deprivation"),
                             hr(),
                             uiOutput("info")),
                    tabPanel("About",
                             tags$br(),
                             tags$p("This application visualises the English Indices of Deprivation 2015 for Lower-layer Super Output Areas (LSOA) within Greater Manchester. Each LSOA contains approximately 1,500 residents or 650 households."),
                             tags$p("Each LSOA is ranked from 1 (most deprived) to 32,844 (least deprived) and assigned to a decile ranging from 1 (most deprived 10%) to 10 (least deprived 10%)."),
                             tags$h4("Sources"),
                             "IMD data: ", tags$a(href="https://www.gov.uk/government/statistics/english-indices-of-deprivation-2015", "DCLG,2015", target="_blank"),
                             tags$br(),         
                             "Boundary layers: ", tags$a(href="http://geoportal.statistics.gov.uk/", "ONS Open Geography Portal", target="_blank"),
                             tags$br(),
                             "Source code ", tags$a(href="https://github.com/traffordDataLab/apps/tree/master/IMD_2015", "available on GitHub", target="_blank"),
                             tags$br(),
                             tags$h4("Credits"),
                             "This", tags$a(href="https://cran.r-project.org/web/packages/shiny/index.html", "Shiny", target="_blank"), "app uses the following R packages:",
                             tags$a(href="https://cran.r-project.org/web/packages/tidyverse/index.html", "tidyverse,", target="_blank"),
                             tags$a(href="https://cran.r-project.org/web/packages/sf/index.html", "sf,", target="_blank"),
                             tags$a(href="https://cran.r-project.org/web/packages/leaflet/index.html", "leaflet,", target="_blank"),
                             tags$a(href="https://cran.r-project.org/web/packages/htmltools/index.html", "htmltools.", target="_blank")
                    )))))

server <- function(input, output, session) {
  values <- reactiveValues(highlight = c())
  
  filteredData <- reactive({
    lsoa <- left_join(lsoa, subset(df, index_domain == input$domain), by = "lsoa11cd")
  })
  
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
  
  output$info <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$h4("Hover over an LSOA for details"))
    } else {
      lsoaCode <- filteredData()$lsoa11cd[values$highlight == lsoa$lsoa11cd]
      return(tags$div(
        HTML(paste(tags$h4("LSOA: ", lsoaCode))),
        HTML(paste("in ", tags$span(filteredData()[filteredData()$lsoa11cd == lsoaCode,]$wd17nm), " Ward, ",
                   tags$span(filteredData()[filteredData()$lsoa11cd == lsoaCode,]$lad17nm), sep = "")),
        br(), br(),
        HTML("<table>
             <tr>
             <th>Rank</th>
             <th>Decile</th>
             </tr>
             <tr>
             <td style='width: 75px'>", formatC(filteredData()[filteredData()$lsoa11cd == lsoaCode,]$rank, format="f", big.mark = ", ", digits=0), "</td>
             <td>", filteredData()[filteredData()$lsoa11cd == lsoaCode,]$decile, "</td>
             </tr>
             </table>")
        ))
    }
    })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
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
      addTiles(urlTemplate = "", 
               attribution = '<a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2017)</a>',
               group = "No background") %>% 
      setView(-2.28417866956407, 53.5151885751656, zoom = 11) %>% 
      addLayersControl(position = 'topleft',
                       baseGroups = c("CartoDB", "OpenStreetMap", "Satellite", "No background"),
                       options = layersControlOptions(collapsed = TRUE))
  })
  
  observe({
    pal <- colorFactor(c("#A31A31", "#D23B33", "#EB6F4A", "#FCB562", "#F4D78D", "#D8E9EC", "#AAD1DE", "#75A8C8", "#4D77AE", "#353B91"), domain = 1:10, ordered = TRUE)
    html_logo <- "<a href='https://trafforddatalab.github.io' target='_blank'><img src='https://trafforddatalab.github.io/assets/logo/trafforddatalab_logo.svg' style='width: 93px; border: 0;' alt='Trafford Data Lab' border='0'></a>"
    
    leafletProxy("map", data = lsoa) %>%
      clearShapes() %>% clearControls() %>% clearMarkers() %>% 
      addPolygons(data = filteredData(), fillColor = ~pal(decile), fillOpacity = 0.4, weight = 0.7, opacity = 1, color = "#757575", layerId = ~lsoa11cd,
                  highlight = highlightOptions(color = "#FFFF00", weight = 3, bringToFront = TRUE)) %>%
      addPolylines(data = la, stroke = TRUE, weight = 3, color = "#212121", opacity = 1) %>% 
      addLabelOnlyMarkers(data = la, lng = ~lon, lat = ~lat, label = ~as.character(lad16nm), 
                          labelOptions = labelOptions(noHide = T, textOnly = T, direction = "bottom",
                                                      style = list(
                                                        "color"="white",
                                                        "text-shadow" = "-1px -1px 10px #757575, 1px -1px 10px #757575, 1px 1px 10px #757575, -1px 1px 10px #757575"))) %>%
      addLegend(position = "bottomleft", 
                colors = c("#A31A31", "#D23B33", "#EB6F4A", "#FCB562", "#F4D78D", "#D8E9EC", "#AAD1DE", "#75A8C8", "#4D77AE", "#353B91"),
                title = "Deciles of deprivation",
                labels = c("10% most deprived", "2","3","4","5","6","7","8","9", "10% least deprived"), opacity = 0.4) %>%
      addControl(html = html_logo, position = "bottomright")
  })
    }

shinyApp(ui, server)