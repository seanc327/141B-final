#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# library(shiny)
# library(leaflet)
# library(dplyr)
# 
# # Example data
# locations <- data.frame(
#   name = c("Location A", "Location B", "Location C"),
#   lat = c(37.7749, 34.0522, 40.7128),
#   lon = c(-122.4194, -118.2437, -74.0060),
#   info = c("Info about Location A", "Info about Location B", "Info about Location C")
# )
# 
# # UI for the Shiny app
# ui <- fluidPage(
#   titlePanel("Hover over locations to get information"),
#   leafletOutput("map")
# )
# 
# # Server function
# server <- function(input, output, session) {
#   
#   # Render the map
#   output$map <- renderLeaflet({
#     
#     # Create a leaflet map
#     leaflet(locations) %>%
#       addTiles() %>%
#       addCircleMarkers(
#         lat = ~lat, lng = ~lon,   # Correctly use lng instead of lon
#         popup = ~info, 
#         label = ~name, 
#         radius = 8, 
#         color = "blue", 
#         stroke = FALSE, 
#         fillOpacity = 0.6
#       )
#   })
#   
#   # Add hover interaction with custom tooltips
#   observe({
#     leafletProxy("map", data = locations) %>%
#       clearMarkers() %>%
#       addCircleMarkers(
#         lat = ~lat, lng = ~lon,   # Correctly use lng instead of lon
#         popup = ~info, 
#         label = ~name, 
#         radius = 8, 
#         color = "blue", 
#         stroke = FALSE, 
#         fillOpacity = 0.6,
#         labelOptions = labelOptions(noHide = TRUE)  # This keeps the label visible during hover
#       )
#   })
# }
# 
# # Run the application
# shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)
library(dplyr)

# Locations in Davis, CA
locations <- data.frame(
  name = c("University of California, Davis", "Davis City Hall"),
  lat = c(38.5382, 38.5449),  # Latitude for UC Davis and City Hall
  lon = c(-121.7617, -121.7404),  # Longitude for UC Davis and City Hall
  info = c("University of California, Davis", "Davis City Hall")
)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Locations in Davis, CA"),
  leafletOutput("map")
)

# Server function
server <- function(input, output, session) {
  
  # Render the map centered on Davis, CA
  output$map <- renderLeaflet({
    
    leaflet(locations) %>%
      addTiles() %>%
      setView(lng = -121.7405, lat = 38.5449, zoom = 13) %>%  # Set map center to Davis, CA
      addCircleMarkers(
        lat = ~lat, lng = ~lon,   # Use lat and lon from the data
        popup = ~info, 
        label = ~name, 
        radius = 8, 
        color = "blue", 
        stroke = FALSE, 
        fillOpacity = 0.6
      )
  })
  
  # Add hover interaction with custom tooltips
  observe({
    leafletProxy("map", data = locations) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lat = ~lat, lng = ~lon,   # Use lat and lon from the data
        popup = ~info, 
        label = ~name, 
        radius = 8, 
        color = "blue", 
        stroke = FALSE, 
        fillOpacity = 0.6,
        labelOptions = labelOptions(noHide = TRUE)  # Keep the label visible during hover
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)





