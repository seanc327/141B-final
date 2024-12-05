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

# price data by ingredient
ingredients = data.frame(
  stores = c("Trader Joe's","SaveMart","Safeway","Target","Davis Co-op"),
  butter = c(3.99,5.99,3.99,4.49,4.49),
  white = c(3.99,3.99,3.79,3.39,2.49),
  brown = c(0,2.69,3.79,2.59,4.69),
  eggs = c(4.99,5.19,2.99,3.69,3.49),
  vanilla = c(7.99,6.99,3.99,4.19,5.29),
  soda = c(0,1.49,1.49,.99,.99),
  flour = c(2.99,6.49,4.49,2.89,3.49),
  choc = c(3.99,6.99,4.99,2.79,3.99),
  nuts = c(0,6.19,4.99,4.49,10.69)
)

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

# Reshape store data to long format
store_data_long <- stores %>%
  pivot_longer(cols = -ingredients,
               names_to = "store",
               values_to = "price")

# Calculate total price for each store
store_total_price <- store_data_long %>%
  group_by(store) %>%
  summarise(total_price = sum(price))

# Define UI for application that draws a histogram
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
                                       "Vanilla","Baking Soda",
                                       "All-purpose Flour","Chocolate Chips",
                                       "Walnuts","All")
                           ),
               plotOutput("ingredient_plot", height = "1000px", width = "100%")
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
             x = "Store", y = "Price") +
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
    # ingredient data graphs
    # for individual ingredients, it is a simple bar plot with the stores on the x-axis
    # and the height of each bar representing the price of the ingredient
    # at a store
    output$ingredient_plot = renderPlot({
      if(input$ingredient_graph == "Butter"){
        # bar plot for butter
        ggplot(ingredients,aes(x=stores,y=butter,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=butter), size = 8,vjust=-.5) + # add label for height of bar
          labs(title="Butter Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # increase readability of all text
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) + # change the legend title
          guides(fill = guide_legend(title = "Stores"))
      } else if (input$ingredient_graph == "White Sugar"){
        # bar plot for white sugar
        ggplot(ingredients,aes(x=stores,y=white,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=white), size = 8,vjust=-.5) + # add label for height of bar
          labs(title="White Sugar Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # increase readability of all text
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) + # change the legend title
          guides(fill = guide_legend(title = "Stores"))
      } else if (input$ingredient_graph == "Brown Sugar"){
        # bar plot for brown sugar
        ggplot(ingredients,aes(x=stores,y=brown,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=brown), size = 8,vjust=-.5) + # add labels for height of bar
          labs(title="Brown Sugar Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for increased readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) + # change the legend title
          guides(fill = guide_legend(title = "Stores"))
      } else if (input$ingredient_graph == "Eggs"){
        # bar plot for eggs
        ggplot(ingredients,aes(x=stores,y=eggs,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=eggs), size = 8,vjust=-.5) + # add height labels for the bars
          labs(title="Eggs Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for better readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # change legend title
      } else if (input$ingredient_graph == "Vanilla"){
        # bar plot for vanilla extract
        ggplot(ingredients,aes(x=stores,y=vanilla,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=vanilla), size = 8,vjust=-.5) + # add labels for height of bar
          labs(title="Vanilla Extract Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for better readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # change legend title
      } else if (input$ingredient_graph == "Baking Soda"){
        # bar plot for baking soda
        ggplot(ingredients,aes(x=stores,y=soda,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=soda), size = 8,vjust=-.5) + # add height label for each bar
          labs(title="Baking Soda Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # change text for better readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # change legend title
      } else if (input$ingredient_graph == "All-purpose Flour"){
        # bar plot for flour
        ggplot(ingredients,aes(x=stores,y=flour,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=flour), size = 8,vjust=-.5) + # add labels for height of bar
          labs(title="All-purpose Flour Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for better readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # change legend title
      } else if (input$ingredient_graph == "Chocolate Chips"){
        # bar plot for chocolate chips
        ggplot(ingredients,aes(x=stores,y=choc,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=choc), size = 8,vjust=-.5) + # add label for height of bar
          labs(title="Chocolate Chips Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for better visibility
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # rename the legend title
      } else if (input$ingredient_graph == "Walnuts"){
        # bar plot for walnuts
        ggplot(ingredients,aes(x=stores,y=nuts,fill=stores)) +
          geom_bar(stat="identity") +
          geom_text(aes(label=nuts), size = 8,vjust=-.5) + # add label for the height of the bars
          labs(title="Chopped Walnuts Cost by Store",x="Store",y="Price ($)") +
          theme_minimal() +
          theme( # adjust text for better readability
            axis.text.x = element_text(angle = 45, hjust = 1,size=20),  # Rotate x-axis labels for better readability
            axis.title.x = element_text(margin = margin(t = 20),size=28),
            axis.title.y = element_text(margin = margin(r = 15),size=28),
            plot.title = element_text(size=36),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 24),
            axis.text.y = element_text(size=20)
          ) +
          guides(fill = guide_legend(title = "Stores")) # change the legend title
      } else if (input$ingredient_graph == "All"){
        # plot for all ingredients
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
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
