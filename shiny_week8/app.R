library(shiny)
library(tidyverse)

ui <- fluidPage(
    
    # Add a title
    titlePanel("Plot Displaying Score Correlation"),
    
    sidebarLayout(
        sidebarPanel(
          
          # Input select on gender
            selectInput("gender", label = "Select the Gender",
                        choices = c("Female", "Male", "All"),
                        selected = "All"),
          # Input select on including error band or not
            selectInput("errorband", label = "Display Error Band?",
                        choices = c("Display Error Band", "Suppress Error Band"),
                        selected = "Display Error Band"),
          # Input select on including data before July 1, 2017 or not
            selectInput("date", label = "Include data before July 1, 2017?",
                        choices = c("Include data", "Don't include data"),
                        selected = "Include data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    output$plot <- renderPlot({
      
      # read in dataset
      data_shiny <- readRDS("./data.rds")
      
      # data after selecting gender: if select "All", keep original data
      if(input$gender != "All"){
        data_shiny <- data_shiny %>%
          filter(gender == input$gender)
      }
      
      # data after selecting date: if select include, keep original data
      if (input$date == "Don't include data") {
        data_shiny <- data_shiny %>%
          filter(timeEnd >= ymd("2017-07-01"))
      }
      
      
      # plot with or without error band
      if(input$errorband == "Display Error Band") {
       ggplot(data_shiny, aes(x = mean_1to6, y = mean_8to10)) +
        geom_point() +
        geom_smooth(method = "lm", color = "purple", se = TRUE) +
        labs(x = "Mean Scores on Q1 to Q6",
             y = "Mean Scores on Q8 to Q10")
      } else if (input$errorband == "Suppress Error Band") {
        ggplot(data_shiny, aes(x = mean_1to6, y = mean_8to10)) +
          geom_point() +
          geom_smooth(method = "lm", color = "purple", se = FALSE) +
          labs(x = "Mean Scores on Q1 to Q6",
               y = "Mean Scores on Q8 to Q10")
      }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
