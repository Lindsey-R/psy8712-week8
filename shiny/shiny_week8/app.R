library(shiny)
library(tidyverse)

data <- readRDS("./data.rds")

ui <- fluidPage(

    titlePanel("Plot Displaying Average Scores"),

    sidebarLayout(
        sidebarPanel(
            selectInput("gender", label = "Select the Gender",
                        choices = c("Female", "Male", "All"),
                        selected = "All")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
