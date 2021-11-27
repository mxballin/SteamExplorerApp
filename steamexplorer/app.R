#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datateachr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Steam Games Explorer"),

    # Sidebar with a slider input for price of game
    sidebarLayout(
        sidebarPanel(
            sliderInput("priceInput",
                        "Price:",
                        min = 0,
                        max = 200,
                        c(10,50),
                        pre="$"),
        selectInput("genreInput", "Genre",
                        choices = c("Indie", "Action", "Adventure"))
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
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
