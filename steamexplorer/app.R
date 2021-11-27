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
        selectInput("genreInput", "Genre",
                        choices = c("Action","Adventure","Massively Multiplayer", "Strategy","Free to Play","RPG",
                                    "Indie" , "Early Access","Simulation","Racing","Casual","Sports",
                                    "Violent","Gore","Valve","Nudity","Animation & Modeling","Design & Illustration",
                                    "Utilities","Sexual Content","Game Development","Education","Software Training",
                                    "Web Publishing","Video Production","Audio Production","Movie",
                                    "Photo Editing","Accounting","Documentary","Short", "360 Video","Tutorial","HTC"))
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
        ggplot(steam, aes(impression)) +
            geom_histogram(stat="count")+
            coord_flip()+ #flipped axes make it easier to read the variable names
            ggtitle("The Distribution of Overall Impressions of Games on Steam")+
            ylab("Number of Games")+
            theme(plot.title = element_text(hjust = 0.5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
