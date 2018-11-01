#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(stringr)
library(fs)
library(dplyr)
library(tools)
library(tidyr)
library(kableExtra)
library(scales)
library(lubridate)
library(zoo)
library(sf)
library(ggplot2)
library(ggmap)
library(data.table)
library(ggrepel)
library(maptools)
library(leaflet)
library(readxl)
library(rgdal)

bus_routes <- "https://opendata.arcgis.com/datasets/a33afaedf9264a97844080839a6f5ec9_0.geojson"
res_routes <- readOGR(dsn = bus_routes, layer = "OGRGeoJSON")
geojson <- jsonlite::fromJSON(bus_routes)
bus_stops <- "https://opendata.arcgis.com/datasets/021adadcf6854f59852ff4652ad90c11_0.geojson"
res_stops <-  readOGR(dsn = bus_stops, layer = "OGRGeoJSON")
geojson1 <- jsonlite::fromJSON(bus_stops)
zip_code <- "https://opendata.arcgis.com/datasets/fee863cb3da0417fa8b5aaf6b671f8a7_0.geojson"
zip_boundary <- readOGR(dsn = zip_code, layer = "OGRGeoJSON")
geojson2 <- jsonlite::fromJSON(zip_code)
# Define UI for application that draws a route map
ui <- fluidPage(
   
   # Application title
   titlePanel("Miami Bus Route Data"),
   mainPanel(leafletOutput("map")),
   mainPanel(leafletOutput("stops")), 
   mainPanel(leafletOutput("zipcodes"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
    addPolygons(data = res_routes, opacity = .5, color = "#00a1e4", 
                weight = 3, smoothFactor = 5, fill = FALSE)
  })
  output$stops <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
    setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
    addCircleMarkers(data = res_stops, radius = 5, fillColor = "#00a1e4",
                     color = "white", fillOpacity = 10, opacity = .5,
                     stroke = TRUE)
  })
  output$zipcodes <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>% # Add default OpenStreetMap map tiles
      setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
      addPolygons(data=zip_boundary, opacity = .5, fillColor = "#00a1e4", 
                  weight = 3, color = "white", fillOpacity = .5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

