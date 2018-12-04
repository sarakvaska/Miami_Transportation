#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Import libraries that are used in app
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
library(stargazer)

# bus routes is geojson data downloaded from Miami's Open Data Hub. More specifically, it 
# contains the geojson data needed to map all the bus routes in Miami

bus_routes <- "https://opendata.arcgis.com/datasets/a33afaedf9264a97844080839a6f5ec9_0.geojson"

# res_routes uses readOGR so that R can read the geojson data properly so that it can be made 
# of use

res_routes <- readOGR(dsn = bus_routes, layer = "OGRGeoJSON")

# routes is a dataframe of the geojson bus_routes data

routes <- jsonlite::fromJSON(bus_routes)

# bus_stops is geojson data downloaded from Miami's Open Data Hub. More specifically, it 
# contains the geojson data needed to map all the bus stop locations in Miami

bus_stops <- "https://opendata.arcgis.com/datasets/021adadcf6854f59852ff4652ad90c11_0.geojson"

# res_stops uses readOGR so that R can read the geojson data properly so that it can be made 
# of use

res_stops <- readOGR(dsn = bus_stops, layer = "OGRGeoJSON")

# zip_code is geojson data downloaded from Miami's Open Data Hub. More specifically, it 
# contains the geojson data needed to map all the zipcode boundaries in Miami

zip_code <- "https://opendata.arcgis.com/datasets/fee863cb3da0417fa8b5aaf6b671f8a7_0.geojson"

# zip_boundary uses readOGR so that R can read the geojson data properly so that it can be made 
# of use

zip_boundary <- readOGR(dsn = zip_code, layer = "OGRGeoJSON")


# zip_csv is a dataframe containing all the data from the Zip_Code.csv file
zip_csv <- read_csv("Zip_Code.csv")

# This line of code turns the numeric ZIP column of the zip_csv file 
# into a character column -- I did this so that the numbers on the scatterplots 
# would appear correctly on the axii (as ascending instead of individually)

zip_csv$ZIP <- as.character(as.numeric(zip_csv$ZIP))

# options is a list of all the user can choose to visualize in the shiny app scatterplots

options <- c("Median Income" = "median_income", 
             "Median Population" = "median_population", 
             "Income and Population" = "bus_stops", 
             "Total Zipcode Area" = "Shape__Area")

# Define UI for application that draws shiny app

ui <- fluidPage(
   
   # Application title
  
   titlePanel("Observing the Factors that Affect Miami's Transportation"),
   tags$a(href = "https://github.com/sarakvaska/poverty_and_transportation", "Github Code"),
   p("Summary: I will be mapping Miami bus routes and bus stops along with poverty levels 
     by zip code. The goal of this project is to determine whether there exists a correlation 
     between public transportation and poverty levels in Miami’s neighborhoods. I am going to 
     be looking specifically at whether neighborhoods with high poverty rates have a larger 
     coverage of public transportation. To do so, I will be looking at the numbers of routes 
     in these neighborhoods along with the number of stops and comparing what I find with the 
     routes and stops in more affluent neighborhoods."),
   
   # Output: Tabset with route map, zipcode and stops map, and scatterplots
   
   tabsetPanel(type = "tabs",
               tabPanel("Routes", leafletOutput("map")),
               tabPanel("Bus Stops and Zipcode Boundaries", leafletOutput("zipcodes")), 
               tabPanel("Visualized Data", 
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("x", label = "View by Factor that affects stop count:", choices = c(options), 
                               selected = "Median Income"),
                            checkboxInput("line", label = "Show Best Fit Line", value = FALSE), 
                            htmlOutput("regression_statement"),
                            htmlOutput("regression_table")),
                            mainPanel(plotlyOutput("plots")))))
   )

# read geojson bus route data through readlines and put into variable called geojson so that I am able
# to use it in addGeoJSONv2, which will map it in my Shiny app

geojson <- reactive({
  readLines("https://opendata.arcgis.com/datasets/a33afaedf9264a97844080839a6f5ec9_0.geojson") %>% 
  paste(collapse = "\n")
})

# server outputs my functions in my app 

server <- function(input, output) {
  
  # output$map renders my leaflet map for routes and my UI (above) reads in the leafletOutput("map") from 
  # how I've defined it here so that it can be displayed in its repsective tab 
  output$map <- renderLeaflet({
    
    # I am using leaflet in order to display my map widget 
    
    leaflet() %>%
      
    # addProviderTiles displays the background of the map. providers$Esri.WorldImagery adds the OpenStreetMap
    # tiles so that it looks like users are viewing a satelite image
      
    addProviderTiles(providers$Esri.WorldImagery) %>%
      
    # setView is set on the lng and lat of Miami and zoomed into 10 so that users can 
    # see it from a place where it's viewable enough and information on the map isn't missing 
      
    setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
    
    # addGeoJSONv2 displays the routes onto my map. I set the lines of the routes to a weight of 3 so that they were
    # not too thick. I set fill equal to FALSE so that the spaces in between the routes were not filled, because 
    # leaflet was trying to fill them in as shapes versus lines. The labelProperty makes it so that users can move
    # the mouse around and, when they crossover a line, the route name pops up. I set the popupProperty to LINENAME so
    # that users can see the LINENAME (where each route begins and ends) when they click on the route they're looking at.
    # The highlightOptions make it so that when a user is hovering on a line, it is highlighted white so they know 
    # exactly which route they're looking at 
      
    addGeoJSONv2(geojson(), weight = 3, color = "#00a1e4", opacity = 1, 
                 fill = FALSE, labelProperty = "RTNAME", popupProperty = "LINENAME",
                 highlightOptions = highlightOptions(weight = 2, color='white', 
                                                     fillOpacity = 1, opacity = 1,
                                                     bringToFront = TRUE, sendToBack = TRUE))
  })
  
  # output$zipcodes takes in the leaflet map I've made and my UI (above) is able to identify it and display it 
  # in my tab panel "Bus Stops and Zipcode Boundaries"
  
  output$zipcodes <- renderLeaflet({
    
    # I am using leaflet to make this map widget 
    
    leaflet() %>%
      
      # like in the previous map, addProviderTiles(providers$Esri.WorldImagery) adds the OpenStreetMap
      # tiles, so that it looks like users are viewing a satelite image
      
      addProviderTiles(providers$Esri.WorldImagery) %>%
      
      # Like in the previous map, setView is set on the lng and lat of Miami and zoomed into 10 so that users can 
      # see it from a place where it's viewable enough and information on the map isn't missing 
      
      setView(lng=-80.191788, lat=25.761681, zoom = 10) %>%
      
      # addCircleMarkers adds the bus stops to my map. I have set them to be white with blue dots filled in and 
      # to be small enough so that they do no appear as an overwhelming amount on the map 
  
      addCircleMarkers(data = res_stops, radius = 5, fillColor = "#00a1e4",
                       color = "white", fillOpacity = 10, opacity = .5,
                       stroke = TRUE, 
                       
                       # clusterOptions clusters my markers -- stops -- together 
                       # this makes it so that as the user zooms out, the bus stops cluster and 
                       # numbers appear at the center of each marker cluster, showing how many stops
                       # are in one cluster. If you zoom out all the way, there are 8,000 stops in one
                       # cluster. The more you zoom in, the more you can tell how many bus stops are in 
                       # a certain area. If you zoom in all the way, you can see the individual bus stops. 
                       
                       clusterOptions = markerClusterOptions(iconCreateFunction =
                                              JS("
                                                 function(cluster) {
                                                 return new L.DivIcon({
                                                 html: '<div style=\"background-color:rgba(255, 255, 255, 1)\"><span>' + cluster.getChildCount() + '</div><span>',
                                                 className: 'marker-cluster'
                                                 });
                                                 }"), maxClusterRadius = 100)) %>%
      
      # addPolygons adds the boundaries of each zipcode onto the map and colors them so 
      # that users can tell they are boundaries but that the map and the markers are still visible 
      # within the boundaries
      
      addPolygons(data=zip_boundary, opacity = 1, fillColor = "#00a1e4", 
                  weight = 3, color = "#2ab7ca ", fillOpacity = .25,
                  
                  # I used highlightOptions so that when a user is hovering within/on a zipcode boundary, 
                  # the zipcode is highlighted
                  
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  # this label makes it so that if a user is hovering within/on a zipcode boundary, the name 
                  # of the zipcode they are hovering over is seen 
                  
                  label = zip_boundary@data[["ZIPCODE"]], group = 'zips') %>%
      
      # addSearchFeatures is added to the map so that users can search a specific zipcode from the map widget 
      # and the map will take you to whichever one you've searched
      addSearchFeatures(targetGroups = 'zips', options = searchFeaturesOptions(zoom = 13, hideMarkerOnCollapse = TRUE)) %>%
      
      # addControl adds a hint to the bottom of the map so that users know that all Miami zipcodes begin with the number 
      # 3 and can start their search from there
      addControl("<P><B>Hint:</B> Start your search with 3 to see a list of all zipcodes in Miami!</P>",
                 position='bottomright')
  })
  
  # output$plots takes in the scatterplot plotly graphs that I've created and my UI (above) 
  # can display them in the tab panel "Visualized Data"
  
  output$plots <- renderLeaflet({
    
    # this graph will display if the checkbox for line of best fit is not clicked
    
    if(input$line == FALSE) {
      
      # this will display if "Income and Population" ("bus_stops") is not picked as the factor to 
      # see on the plot 
      
      if(input$x == "median_population" || input$x == "median_income" ||
         input$x == "Shape__Area") {
        
        # In my scatterplots, the bus stop count is on the y axis and the user's chosen input
        # for the factor on the graph is on the x axis. The graph is colored by Zipcode so that 
        # they all show up individually 
        
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "bus_stops", x = input$x, color = "ZIP")) +
                   
                   # if user chooses to see median income on their scatterplot, the tooltip 
                   # of the plot will display relevant, formatted information
                   # (such as zipcode, bus stop count, median income, and median pop. 
                   # when they hover over points 
                   # i included median population and median income so that users could explore if there
                   # exists a relationship between the two and their coverage of transportation
                   
                   geom_point(if(input$x == "median_income") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=",")))
                   }
                   
                   # if user chooses to see median population on their scatterplot, the tooltip 
                   # of the plot will display relevant, formatted information (the same as in 
                   # the median income scatterplot when they hover over points 
                   
                   else if (input$x == "median_population") {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=",")))
                   }
                   
                   # if the user chooses to see the zipcode area and whether the count of bus stops is influenced
                   # by area, then I will display the relevant information in the tooltip according to this
                   
                   else {
                     aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Zipcode Area: ", 
                                       format(((zip_csv$Shape__Area)), 
                                              nsmall=1, big.mark=",")))
                   }) + 
                   
                   # the labels of the scatterplots
                   # the axis will be labeled according to the variable the user visualizes 
                   # the y axis will be labeled total bus stops, since this variable does not change 
                   # the color legend is labeled Zipcodes 
                   
                   labs(x = names(options[which(options == input$x)]), 
                        y = "Total Bus Stops", 
                        color = "Zipcodes") + 
                   
                   # the scale of the x axis is fitted according to the variable
                   # the median income appears as the dollar scale
                   # the median population appears as thousands with a comma 
                   # the zip area appears as a log because the numbers are so large
                   
                   if (input$x == "median_income") {
                     scale_x_continuous(labels = scales::dollar)
                   } else if (input$x == "median_population") {
                     scale_x_continuous(labels = scales::comma)
                   } else {
                     scale_x_log10()
                   }, tooltip = "text")
      }
      
      # if the user chooses to visualize by both income (y axis) and population (x axis), 
      # with the legend according to the count of bus stops, this does that for them 
      
      else {
        ggplotly(ggplot(data = zip_csv, 
                        aes_string(y = "median_income", x = "median_population", color = "bus_stops")) +
                   
                   # this displays information in the tooltip when they hover over a point 
                   
                   geom_point(aes(text = paste0("Zipcode: ", ZIP, "<br>Bus Stops: ", bus_stops, 
                                       "<br>Median Income: $", 
                                       format(((zip_csv$median_income)), 
                                              nsmall=1, big.mark=","), 
                                       "<br>Median Population: ", 
                                       format(((zip_csv$median_population)), 
                                              nsmall=1, big.mark=",")))) + 
                   
                   # the labels of this graph on the x axis and y axis and the color legend
                   
                   labs(x = "Median Population", 
                        y = "Median Income", color = "Bus Stop Count") + 
                   
          # the scale of the x axis is in commas since it is showing population
          # the scale of the y axis is in dollars since it is displaying income 
            
          scale_x_continuous(labels = scales::comma) +
          scale_y_continuous(labels = scales::dollar), tooltip = "text")
      }
    }
    
    # if the user chooses to add a line of best fit and they have chosen to visualize
    # population, income or area, this does it for them 
    # all of this code is the same as the above code, except for geom_smooth, 
    # which adds the best fit line 
    
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
                   }) + 
                   
                   # line of best fit 
                   
                   geom_smooth(aes(group = "ZIP"), se = FALSE, method = "lm") + 
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
      
      # if the user chooses to add a line of best fit and they want to visualize 
      # income and population, this does it for them 
      # all the code is the same as if the user did not choose to add a line of best fit 
      # except, since they did, geom_smooth is added into the code 
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
                   
                   # line of best fit 
                   
                   geom_smooth(aes(group = "ZIP"), se = FALSE, method = "lm") +
                   labs(x = "Median Population", 
                        y = "Median Income", color = "Bus Stop Count") + 
                   scale_x_continuous(labels = scales::comma) +
                   scale_y_continuous(labels = scales::dollar), tooltip = "text")
      }
    }
  })
  output$regression_statement <- renderUI ({
    if(input$line == TRUE) {
      h5("Correlation Between Variables:")
    }
  })
  output$regression_table <- renderUI({
    if(input$line == TRUE) {
      as.character(cor(zip_csv[["median_population"]], zip_csv[[input$x]], use = "complete.obs"))
      }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

