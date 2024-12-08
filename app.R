# Shiny App to display bar graphs comparing grocery data
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(leaflet)

# price data by store
stores = data.frame(
  ingredients = c("Butter","White Sugar","Brown Sugar","Eggs","Vanilla","Baking Soda",
                  "All-purpose Flour","Chocolate Chips","Walnuts"),
  tjs = c(3.99,3.99,0,4.99,7.99,0,2.99,3.99,0),
  svmrt = c(5.99,3.99,2.69,5.19,6.99,1.49,6.49,6.99,6.19),
  sfwy = c(3.99,3.79,3.79,2.99,3.99,1.49,4.49,4.99,4.99),
  tgt = c(4.49,3.39,2.59,3.69,4.19,.99,2.89,2.79,4.49),
  coop = c(4.49,2.49,4.69,3.49,5.29,.99,3.49,3.99,10.69)
)

# Locations in Davis, CA
locations <- data.frame(
  name = c("Trader Joe's", "SaveMart", "Safeway A", "Safeway B", "Target", "Davis Co-op"),
  lat = c(38.5467537, 38.559804, 38.5620828, 38.5412369, 38.5539639, 38.5495574),
  lon = c(-121.7614973, -121.7584022, -121.7664724, -121.7229385, -121.6999147, -121.739849)
)

# Ingredient price data by store
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
  walnuts = c(0, 6.19, 4.99, 4.49, 10.69),
  custom_sentence = c(
    "Trader Joe's offers the cheapest prices on unsalted butter alongside Trader Joe's but does not sell brown sugar, baking soda, or chopped walnuts - making it impossible to buy all ingredients needed at this store.",
    "SaveMart is the most expensive store if you want to buy all ingredients at one store and does not offer the cheapest prices on any ingredient.",
    "Safeway offers the cheapest prices on unsalted butter, eggs, and vanilla extract and they are the second cheapest store to stop at if you want to buy all ingredients at one store.",
    "Target is the cheapest store to shop at if you wish to buy all ingredients at one store. They offer the cheapest brown sugar, baking soda, all-purpose flour, semi-sweet chocolate chips, and chopped walnuts - offering the cheapest one-stop shop for all ingredients needed for the recipe.",
    "Davis Co-op is the second most expensive store to shop at but they do offer the cheapest granulated sugar and baking soda across all of the stores."
  )
)

# Reshape store data to long format
store_data_long <- stores %>%
  pivot_longer(cols = -ingredients,
               names_to = "store",
               values_to = "price")

# Calculate total price for each store
store_total_price <- store_data_long %>%
  group_by(store) %>%
  summarise(total_price = sum(price))

# Define UI for application that draws bar graphs
ui <- fluidPage(
  # Include custom CSS to adjust font sizes
  tags$head(
    tags$style(HTML("
      /* Title panel font size */
      .titlePanel h2 {
        font-size: 36px;
        font-weight: bold;
      }

      /* Tab panel font size */
      .nav-tabs .nav-item .nav-link {
        font-size: 20px;
      }

      /* Select input font size */
      .shiny-input-container select {
        font-size: 18px;
      }
    "))
  ),
  # Application title
  titlePanel("Grocery Store & Ingredient Data"),
  
  # create tabs
  tabsetPanel(
    # store tab
    tabPanel("Stores",
             h4("Total Cost by Store"),
             # output for selected store plot
             plotOutput("store_plot", height = "1000px", width = "100%")
    ),
    # ingredients tab
    tabPanel("Ingredients",
             # drop down to choose an ingredient
             selectInput("ingredient_graph",
                         "Choose an ingredient graph:",
                         choices = c("Butter","White Sugar",
                                     "Brown Sugar","Eggs",
                                     "Vanilla Extract","Baking Soda",
                                     "All-purpose Flour","Chocolate Chips",
                                     "Walnuts","All")
             ),
             plotOutput("ingredient_plot", height = "1000px", width = "100%")
    ),
    # map tab
    tabPanel("Map",
             leafletOutput("map"),
             uiOutput("store_info")
    )
  ))


# Define server logic required to draw bar graphs
# depending on user input
server <- function(input, output) {
  # store data graphs
  output$store_plot <- renderPlot({
    # bar graph with the height of each bar the total price
    # at each store
    # segmented by each individual product
    ggplot(store_data_long, aes(x = store, y = price, fill = ingredients, label = ifelse(price > 0, scales::dollar(price), ""))) +
      geom_bar(stat = "identity") +
      # Segment labels with increased contrast (white text on dark segments)
      geom_text(position = position_stack(vjust = 0.5), color = "black", size = 6, fontface = "bold") +
      # Total price labels with smaller size and white border for contrast
      geom_text(data = store_data_long %>%
                  group_by(store) %>%
                  summarize(total = sum(price)),
                aes(x = store, y = total + 0.5, label = scales::dollar(total)),
                inherit.aes = FALSE, size = 8, color = "black", fontface = "bold") +
      theme_minimal() +
      labs(title = "Prices of Ingredients by Store",
           x = "Store", y = "Price ($)") +
      scale_fill_brewer(palette = "Set3") +
      scale_x_discrete(labels = c("tjs" = "Trader Joe's", # change labels on the x-axis
                                  "svmrt" = "SaveMart",
                                  "sfwy" = "Safeway",
                                  "tgt" = "Target",
                                  "coop" = "Davis Co-op")) +
      theme( # increase font size, add margins, and rotate labels for better readability
        axis.text.x = element_text(angle = 45, hjust = 1,size=20),
        axis.title.x = element_text(margin = margin(t = 20),size=28),
        axis.title.y = element_text(margin = margin(r = 15),size=28),
        plot.title = element_text(size=36),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 24),
        axis.text.y = element_text(size=20)
      ) +
      guides(fill = guide_legend(title = "Ingredients")) # change legend title
    
  })
  # Ingredient data graph
  output$ingredient_plot <- renderPlot({
    if (input$ingredient_graph == "All") {
      ggplot(store_data_long, aes(x = ingredients, y = price, fill = store)) +
        geom_bar(stat = "identity", position = "dodge") +  # Create bars
        geom_text(aes(label = price),
                  position = position_dodge(width = 0.9),
                  angle = 90,  # Rotate the labels by 90 degrees
                  hjust = -.1,  # Horizontal alignment of the rotated labels
                  vjust = .5) +  # Place labels just above the top of the bars
        labs(title = "Prices of Ingredients by Store", x = "Ingredient", y = "Price") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        expand_limits(y = max(store_data_long$price) + 1) + # increase limits
        # change the names on the legend
        scale_fill_manual(values = c("tjs" = "red", "svmrt" = "blue", "sfwy" = "green", "tgt" = "purple", "coop" = "orange"),
                          labels = c("Trader Joe's", "SaveMart", "Safeway", "Target", "Davis Co-op")) +
        theme( # make text easier to read
          axis.text.x = element_text(angle = 45, hjust = 1,size=20),
          axis.title.x = element_text(margin = margin(t = 20),size=28),
          axis.title.y = element_text(margin = margin(r = 15),size=28),
          plot.title = element_text(size=36),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 24),
          axis.text.y = element_text(size=20)
        ) +
        guides(fill = guide_legend(title = "Stores")) # rename the legend
    } else { # handle individual ingredients
      ingredient_column <- tolower(gsub("[- ]", "_", input$ingredient_graph))
      ggplot(ingredients, aes(x = stores, y = .data[[ingredient_column]], fill = stores)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste(input$ingredient_graph, "Cost by Store"), x = "Store", y = "Price ($)") +
        theme( # make text easier to read
          axis.text.x = element_text(angle = 45, hjust = 1,size=20),
          axis.title.x = element_text(margin = margin(t = 20),size=28),
          axis.title.y = element_text(margin = margin(r = 15),size=28),
          plot.title = element_text(size=36),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 24),
          axis.text.y = element_text(size=20)
        ) +
        geom_text(aes(label=.data[[ingredient_column]]), size = 8,vjust=-.5) + # add label for the height of the bars
        guides(fill = guide_legend(title = "Stores"))
    }
  })
  # Map rendering
  output$map <- renderLeaflet({
    leaflet(locations) %>%
      addTiles() %>%
      setView(lng = -121.7405, lat = 38.5449, zoom = 13) %>% # view of Davis
      addCircleMarkers( # add markers for each store
        lat = ~lat, lng = ~lon,
        radius = 8, color = "blue",
        stroke = FALSE, fillOpacity = 0.6,
        layerId = ~name
      ) %>%
      addLabelOnlyMarkers( # add labels for each marker
        lat = ~lat, lng = ~lon,
        label = ~name,
        labelOptions = labelOptions(
          noHide = TRUE, direction = "top",
          textOnly = TRUE, style = list("font-weight" = "bold", "color" = "black", "font-size" = "14px")
        )
      )
  })
  # when someone clicks on a dot, the information pops up
  observeEvent(input$map_marker_click, {
    store_id <- input$map_marker_click$id
    store_name <- ifelse(grepl("Safeway", store_id), "Safeway", store_id)
    
    if (store_name %in% ingredients$stores) {
      store_data <- ingredients %>% filter(stores == store_name) # get ingredient info from store
      output$store_info <- renderUI({
        tagList(
          h3(paste("Welcome to", store_name, "!")), # greeting
          p(store_data$custom_sentence), # short blurb about the store
          tableOutput("store_table") # render ingredient info table
        )
      })
      output$store_table <- renderTable({
        store_data %>%
          select(-stores, -custom_sentence)
      })
    } else { # error
      output$store_info <- renderUI({
        h3("Store not found!")
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)