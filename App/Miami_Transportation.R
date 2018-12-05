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
library(shinythemes)

    
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

ui <- fluidPage(theme = shinytheme("cerulean"),
              
   # Application title
   navbarPage("What Influences Public Transportation Coverage?",
              tabPanel("Summary", HTML('<center><img src = "https://www.anewteam.com/wp-content/uploads/miami.png"
                                        width = "85%" height = "85%"></center>'), tags$br(), tags$br(),
                       p("Throughout high school, I took the public bus almost everyday, and I noticed that 
                         the rate of people getting on and off would increase or decrease depending on areas of Miami, as 
                         well as the coverage of the bus stops. This struck my interest, and I created this app with the 
                         intention of exploring what factors (median income, median population, zipcode area, etc.) affect
                         the coverage of Miami's public transportation."), 
                       tags$br(), 
                       p("To explore this topic, I used data from", tags$a(href = "http://gis-mdc.opendata.arcgis.com/", "Miami's Open Data Hub"), 
                         "The links to the exact data I used are ", tags$a(href = "http://gis-mdc.opendata.arcgis.com/datasets/bus-route", "Bus Routes"), 
                         "and ", tags$a(href = "http://gis-mdc.opendata.arcgis.com/datasets/bus-stop", "Bus Stops"), 
                         ". Using this data, I mapped Miami's bus routes and bus stops. In my bus stop map, I added the layer of zipcode boundaries so that
                         the count of stops per zipcode is distinguishable. I then graphed the relationship between median income, median population, median income
                         and population, and zipcode area with the number of bus stops in all of Miami's zipcodes. The goal of this project was to determine whether 
                         there exists a correlation between public transportation and poverty levels or population levels in Miami’s neighborhoods."),
                       p("The other sources I used to create this project came from: ",
                         tags$br(),
                         tags$a(href = "http://www.miamidadematters.org/demographicdata/index/view?id=1469&localeTypeId=3", "Miami Dade Matters - Population Data per Zipcode"),
                         tags$br(),
                         tags$a(href = "http://www.miamidadematters.org/?module=demographicdata&controller=index&action=view&localeId=0&localeTypeId=3&tagFilter=0&id=2419", 
                                "Miami Dade Matters - Income Data per Zipcode"),
                         tags$br(),
                         tags$a(href = "https://www.google.com/url?sa=i&source=images&cd=&cad=rja&uact=8&ved=2ahUKEwjHmMWglIjfAhUxTt8KHQ0iBFAQjRx6BAgBEAU&url=https%3A%2F%2Fanewteam.com%2Flocations%2Fcorporate-team-building-miami%2F&psig=AOvVaw2lLJpEsJAHXV9r8kXrBzhb&ust=1544081036780092", 
                                "Miami Image")),
                       p("View the code I wrote to create this project on ", tags$a(href = "https://github.com/sarakvaska/poverty_and_transportation", "Github"))),
              
              tabPanel("Routes", h2("Miami Bus Route Coverage"), p("This map is designed so that you can take a look at all of the bus routes in the Miami area. In total, there 
                         are 112 bus routes. As the hint in the map says, if you hover over the routes, you can see the route number. If 
                                   if you click on the route, you can see its name. The names of the routes, for the most part, say what area of Miami the bus travels."), 
                       mainPanel(leafletOutput("map", height = 500, width = 600))),
              tabPanel("Bus Stops and Zipcode Boundaries", h2("Bus Stops in Miami Zipcodes"), p("This map is designed so that you can 
                                                                                                take a look at all of the bus stops in Miami, and using the zipcode boundaries,
                                                                                                poke around to see where there are a large amount of stops or where there are none. As 
                                                                                                the hint in the map says, every zipcode in Miami starts with a 3, so you can enter 3 in the search 
                                                                                                bar to see a list of all zipcodes."), 
                       mainPanel(leafletOutput("zipcodes", height = 500, width = 600))), 
              tabPanel("Visualized Data", 
                       h2("Exploring Factors Affecting Miami's Transportation Coverage"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("x", label = "View by Factor:", choices = c(options), 
                                       selected = "Median Income"),
                           checkboxInput("line", label = "Show Best Fit Line", value = FALSE), 
                           htmlOutput("correlation_statement"),
                           htmlOutput("correlation"),
                           htmlOutput("note_and_summary")), 
                         mainPanel(plotlyOutput("plots"))),
                       tags$br(),
                       p("Using these plots, I found that median population has the strongest correlation to bus stop count. Therefore, 
                         this is probably how the city of Miami decides where to place the most bus stops and routes covering the area."),
                       p("I came to this conclusion by visualizing factors of the data that I felt played the most role in 
                         affecting the count of bus stops for certain zipcodes in Miami: Income, Population, and Area.
                         When viewing by the Median Income factor, we can see that there seems to be a trend for a
                         higher bus stop count in areas with a less median income. To confirm whether a relationship exists 
                         between stop count and median income, I found the correlation: -0.291319779360426. Because this correlation 
                         is negative, it signifies that for stop count and income, an increase in stop count is correlated with a
                         decrease in median income for an area. Because the correlation is around -0.29, this indicates the relationship 
                         between these variables is moderately negatively correlated."),
                       p("The next factor is median population. Looking at this graph and the correlation we see between the bus stop count 
                         and the median population, we can see that it's very strong - stronger than that of median income. The correlation gives
                         us one, meaning that this relationship is perfectly positively correlated, so as the population increases, the bus stop
                         count in an area increases."), 
                      p("Next, we take a look at income and population. This scatterplot places median income on the y axis and median population
                        on the x axis and colors the points by the amount of bus stops. The point of this scatterplot is to determine whether 
                        there exists a correlation between the median income of a place and its population, which could provide insight for 
                        whether transportation coverage is concentrated in the areas that need it most, where there are the most people who 
                        make the least amount of money. This correlation was 0.533468158017823, signifying that there exists a moderately 
                        positive correlation between median income and median population."), 
                      p("The total zipcode area scatterplot intends to see whether area is a factor in the amount of bus stops. It aims to look at 
                        the correlation between area coverage of a zipcode and the corresponding number of stop in that area. The correlation between 
                        these variables is 0.0253083845097556, which is very small, so they are only correlated by a small amount - an amount too 
                        insignificant to draw any conclusions from.")
                       )))

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
      
    addFullscreenControl() %>%
      
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
                                                     bringToFront = TRUE, sendToBack = TRUE)) %>%
      addControl("<P><B>Hint:</B> Hovering on a route gives its number; clicking on a route gives the location it covers!</P>",
                 position='bottomright')
  })
  
  # output$zipcodes takes in the leaflet map I've made and my UI (above) is able to identify it and display it 
  # in my tab panel "Bus Stops and Zipcode Boundaries"
  
  output$zipcodes <- renderLeaflet({
    
    # I am using leaflet to make this map widget 
    
    leaflet() %>%
      
      # like in the previous map, addProviderTiles(providers$Esri.WorldImagery) adds the OpenStreetMap
      # tiles, so that it looks like users are viewing a satelite image
      
      addProviderTiles(providers$Esri.WorldImagery) %>%
      
      addFullscreenControl() %>%
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
  
  # add line below checkbox for line of best input that says what the correlation between 
  # the variables is 
  output$correlation_statement <- renderUI ({
    if(input$line == TRUE) {
      h5("Correlation Between Variables:")
    }
  })
  
  # output the correlation between the variables 
  output$correlation <- renderUI({
    if(input$line == TRUE) {
      as.character(cor(zip_csv[["median_population"]], zip_csv[[input$x]], use = "complete.obs"))
      }
  })
  
  output$note_and_summary <- renderUI({
    if(input$line == TRUE) {
      h6("Note: The correlation coefficient is a measure that determines how closely related the measurements
       of two variables are. When the correlation is greater than zero, it signifies that both variables 
         move in the same direction or are correlated. When the correlation is 1, it signifies 
         that when one variable moves higher or lower, the other variable moves in the same direction 
         with the same magnitude. The closer the value of the correlation to 1, the stronger the linear relationship;
         the farther the value of the correlation to 1, the weaker the linear relationship." %>%
      tags$br() %>% tags$br() %>%
      h6("Read more on ", tags$a(href = "https://www.investopedia.com/ask/answers/032515/what-does-it-mean-if-correlation-coefficient-positive-negative-or-zero.asp", "correlation")))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

