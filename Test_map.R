#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

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





