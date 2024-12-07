library(shiny)
library(leaflet)
library(dplyr)

# Locations in Davis, CA
locations <- data.frame(
  name = c("Trader Joe's", "SaveMart", "Safeway A", "Safeway B", "Target", "Davis Co-op"),
  lat = c(38.5467537, 38.559804, 38.5620828, 38.5412369, 38.5539639, 38.5495574),
  lon = c(-121.7614973, -121.7584022, -121.7664724, -121.7229385, -121.6999147, -121.739849)
)

# Ingredient and price data
ingredients <- data.frame(
  stores = c("Trader Joe's", "SaveMart", "Safeway", "Target", "Davis Co-op"),
  butter = c(3.99, 5.99, 3.99, 4.49, 4.49),
  white_sugar = c(3.99, 3.99, 3.79, 3.39, 2.49),
  brown_sugar = c(0, 2.69, 3.79, 2.59, 4.69),
  eggs = c(4.99, 5.19, 2.99, 3.69, 3.49),
  vanilla_extract = c(7.99, 6.99, 3.99, 4.19, 5.29),
  baking_soda = c(0, 1.49, 1.49, 0.99, 0.99),
  all_purpose_flour = c(2.99, 6.49, 4.49, 2.89, 3.49),
  chocolate_chips = c(3.99, 6.99, 4.99, 2.79, 3.99),
  walnut = c(0, 6.19, 4.99, 4.49, 10.69),
  custom_sentence = c(
    "Trader Joe's offers the cheapest prices on unsalted butter alongside Trader Joe's but does not sell brown sugar, baking soda, or chopped walnuts - making it impossible to buy all ingredients needed at this store.",
    "SaveMart is the most expensive store if you want to buy all ingredients at one store and does not offer the cheapest prices on any ingredient.",
    "Safeway offers the cheapest prices on unsalted butter, eggs, and vanilla extract and they are the second cheapest store to stop at if you want to buy all ingredients at one store.",
    "Target is the cheapest store to shop at if you wish to buy all ingredients at one store. They offer the cheapest brown sugar, baking soda, all-purpose flour, semi-sweet chocolate chips, and chopped walnuts - offering the cheapest one-stop shop for all ingredients needed for the recipe.",
    "Davis Co-op is the second most expensive store to shop at but they do offer the cheapest granulated sugar and baking soda across all of the stores."
  )
)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Locations in Davis, CA"),
  leafletOutput("map"),
  uiOutput("store_info")  # Dynamic UI to display the store information and table
)

# Server function
server <- function(input, output, session) {
  # Render the map centered on Davis, CA
  output$map <- renderLeaflet({
    leaflet(locations) %>%
      addTiles() %>%
      setView(lng = -121.7405, lat = 38.5449, zoom = 13) %>%
      addCircleMarkers(
        lat = ~lat, lng = ~lon,
        radius = 8,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.6,
        layerId = ~name  # Unique layerId for every location
      ) %>%
      addLabelOnlyMarkers(
        lat = ~lat, lng = ~lon,
        label = ~name,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = TRUE,
          style = list("font-weight" = "bold", "color" = "black", "font-size" = "14px")
        )
      )
  })

  # React to clicks on the map and show the relevant store information
  observeEvent(input$map_marker_click, {
    store_id <- input$map_marker_click$id

    # Normalize "Safeway A" and "Safeway B" to "Safeway" for data purposes
    store_name <- ifelse(grepl("Safeway", store_id), "Safeway", store_id)

    # Check if store_name exists in the ingredients data
    if (store_name %in% ingredients$stores) {
      store_data <- ingredients %>% filter(stores == store_name)

      # Render the store sentence and table dynamically
      output$store_info <- renderUI({
        tagList(
          h3(paste("Welcome to", store_name, "!")),
          p(store_data$custom_sentence),  # Use the custom sentence for the store
          renderTable(store_data[, 2:10])  # Render the ingredient table, excluding the custom sentence column
        )
      })
    } else {
      # Handle cases where the store name does not exist in the ingredients data
      output$store_info <- renderUI({
        h3("Store not found!")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
