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
library(dplyr)
library(tools)
library(tidyr)
library(ggplot2)
library(ggmap)
library(scales)
library(data.table)
library(ggrepel)
library(maptools)
library(leaflet)
library(plotly)
library(rgdal)
library(leaflet.extras)

bus_routes <- "https://opendata.arcgis.com/datasets/a33afaedf9264a97844080839a6f5ec9_0.geojson"
res_routes <- readOGR(dsn = bus_routes, layer = "OGRGeoJSON")
routes <- jsonlite::fromJSON(bus_routes)
bus_stops <- "https://opendata.arcgis.com/datasets/021adadcf6854f59852ff4652ad90c11_0.geojson"
res_stops <- readOGR(dsn = bus_stops, layer = "OGRGeoJSON")
geojson1 <- jsonlite::fromJSON(bus_stops)
zip_code <- "https://opendata.arcgis.com/datasets/fee863cb3da0417fa8b5aaf6b671f8a7_0.geojson"
zip_boundary <- readOGR(dsn = zip_code, layer = "OGRGeoJSON")
geojson2 <- jsonlite::fromJSON(zip_code)
zip_csv <- read_csv("Zip_Code.csv")
zip_csv$ZIP <- as.character(as.numeric(zip_csv$ZIP))
options <- c("Median Income" = "median_income", 
             "Median Population" = "median_population", 
             "Total Zipcode Area" = "Shape__Area", 
             "Income and Population" = "bus_stops")
# Define UI for application that draws a route map
ui <- fluidPage(
   
   # Application title
   titlePanel("Miami Bus Route Data"),
   tags$a(href = "https://github.com/sarakvaska/poverty_and_transportation", "Github Code"),
   p("Summary: I will be mapping Miami bus routes and bus stops along with poverty levels 
     by zip code. The goal of this project is to determine whether there exists a correlation 
     between public transportation and poverty levels in Miami’s neighborhoods. I am going to 
     be looking specifically at whether neighborhoods with high poverty rates have a larger 
     coverage of public transportation. To do so, I will be looking at the numbers of routes 
     in these neighborhoods along with the number of stops and comparing what I find with the 
     routes and stops in more affluent neighborhoods."),
   # Output: Tabset with routes, stops, zipcodes
   tabsetPanel(type = "tabs",
               tabPanel("Routes", leafletOutput("map")),
               tabPanel("Bus Stops and Zipcode Boundaries", leafletOutput("zipcodes")), 
               tabPanel("Scatterplots", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("x", label = "X-axis:", choices = c(options), 
                               selected = "Median Income"),
                            checkboxInput("line", label = "Show Best Fit Line", value = FALSE)), 
                            mainPanel(plotlyOutput("plots")))))
   )
geojson <- reactive({
  readLines("https://opendata.arcgis.com/datasets/a33afaedf9264a97844080839a6f5ec9_0.geojson") %>% 
  paste(collapse = "\n")
})
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
    addGeoJSONv2(geojson(), weight = 3, color = "#00a1e4", opacity = 1, 
                 fill = FALSE, labelProperty = "RTNAME",
                 highlightOptions = highlightOptions(weight = 2, color='white', 
                                                     fillOpacity = 1, opacity = 1,
                                                     bringToFront = TRUE, sendToBack = TRUE))
  })
  output$zipcodes <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>% # Add default OpenStreetMap map tiles
      setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
      addCircleMarkers(data = res_stops, radius = 5, fillColor = "#00a1e4",
                       color = "white", fillOpacity = 10, opacity = .5,
                       stroke = TRUE, 
                       clusterOptions = markerClusterOptions(iconCreateFunction =
                                              JS("
                                                 function(cluster) {
                                                 return new L.DivIcon({
                                                 html: '<div style=\"background-color:rgba(255, 255, 255, 1)\"><span>' + cluster.getChildCount() + '</div><span>',
                                                 className: 'marker-cluster'
                                                 });
                                                 }"), maxClusterRadius = 100)) %>%
      addPolygons(data=zip_boundary, opacity = 1, fillColor = "#00a1e4", 
                  weight = 3, color = "#2ab7ca ", fillOpacity = .25,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE), 
                  label = zip_boundary@data[["ZIPCODE"]], group = 'zips') %>%
      addSearchFeatures(targetGroups = 'zips', options = searchFeaturesOptions(zoom = 13, hideMarkerOnCollapse = TRUE)) %>%
      addControl("<P><B>Hint:</B> Start your search with 3 to see a list of all zipcodes in Miami!</P>",
                 position='bottomright')
  })
  output$plots <- renderLeaflet({
    if(input$line == FALSE) {
      if(input$x == "median_population" || input$x == "median_income" ||
         input$x == "Shape__Area") {
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "bus_stops", x = input$x, color = "ZIP")) +
                   geom_point(if(input$x == "median_income") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=",")))
                   }
                   else if (input$x == "median_population") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=",")))
                   }
                   else {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Zipcode Area: ", 
                                       format(((zip_csv$Shape__Area)), 
                                              nsmall=1, big.mark=",")))
                   }) + 
                   labs(x = names(options[which(options == input$x)]), 
                        y = "Total Bus Stops", 
                        color = "Zipcodes") + 
                   if (input$x == "median_income") {
                     scale_x_continuous(labels = scales::dollar)
                   } else if (input$x == "median_population") {
                     scale_x_continuous(labels = scales::comma)
                   } else {
                     scale_x_log10()
                   }, tooltip = "text")
      }
      else {
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "median_income", x = "median_population", color = "bus_stops")) +
                   geom_point(aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=",")))) + 
                   labs(x = "Median Population", 
                        y = "Median Income", color = "Bus Stop Count") + 
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::dollar), tooltip = "text")
      }
    }
    else {
      if(input$x == "median_population" || input$x == "median_income" ||
         input$x == "Shape__Area") {
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "bus_stops", x = input$x, color = "ZIP")) +
                   geom_point(if(input$x == "median_income") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=",")))
                   }
                   else if (input$x == "median_population") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=",")))
                   }
                   else {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Zipcode Area: ", format(((zip_csv$Shape__Area)), 
                                                                    nsmall=1, big.mark=",")))
                   }) + geom_smooth(aes(group = "ZIP"), se = FALSE, method = "lm") + 
                   labs(x = names(options[which(options == input$x)]), 
                        y = "Total Bus Stops", 
                        color = "Zipcodes") + if (input$x == "median_income") {
                          scale_x_continuous(labels = scales::dollar)
                        } else if (input$x == "median_population") {
                          scale_x_continuous(labels = scales::comma)
                        } else {
                          scale_x_log10()
                        }, tooltip = "text")
      }
      else {
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "median_income", x = "median_population", color = "bus_stops")) +
                   geom_point(aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                                "<br>Median Income: $", 
                                                format(((zip_csv$median_income)), 
                                                       nsmall=1, big.mark=","), 
                                                "<br>Median Population: ", 
                                                format(((zip_csv$median_population)), 
                                                       nsmall=1, big.mark=",")))) + 
                   geom_smooth(aes(group = "ZIP"), se = FALSE, method = "lm") +
                   labs(x = "Median Population", 
                        y = "Median Income", color = "Bus Stop Count") + 
                   scale_x_continuous(labels = scales::comma) +
                   scale_y_continuous(labels = scales::dollar), tooltip = "text")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

