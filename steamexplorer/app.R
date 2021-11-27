#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

steam <- readRDS("ballin_steam.rds")

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
        checkboxGroupInput("genreInput", "Genres:",
                           c("Action","Adventure","Massively Multiplayer", "Strategy","Free to Play","RPG",
                                    "Indie" , "Early Access","Simulation","Racing","Casual","Sports")),
        selectInput("languageInput", "Language",
                    choices = c("English","French","Italian","German","Spanish - Spain","Japanese"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           br(), br(),
           tableOutput("results")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        filtered <-
            steam %>%
            filter(original_price >= input$priceInput[1],
                   original_price <= input$priceInput[2],
                   genre %in% input$genreInput,
                   languages == input$languageInput
            )
        ggplot(filtered, aes(impression)) +
            geom_histogram(stat="count")+
            coord_flip()+ #flipped axes make it easier to read the variable names
            ggtitle("The Distribution of Overall Impressions of Games on Steam")+
            ylab("Number of Games")+
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$results <- renderTable({
        filtered <-
            steam %>%
            filter(original_price >= input$priceInput[1],
                   original_price <= input$priceInput[2],
                   genre %in% input$genreInput,
                   languages == input$languageInput
            )
        filtered %>%
            select(-c(genre,languages))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
