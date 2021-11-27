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
library(datateachr)
library(tidyverse)
library(bpa)

#create the dataset needed for the app
#isolating the original prices of the games
game_prices <- steam_games %>%
    pivot_longer(cols = c(original_price, discount_price),
                 names_to = "price_type",
                 values_to = "price",
                 values_drop_na = TRUE) %>%
    filter(!is.na(price_type), !is.na(price), price !="NaN")

original_game_prices <- game_prices %>%
    filter(price_type=="original_price")
#factoring the genres of the games
unnested_genres <- steam_games %>%
    mutate(genre = strsplit(as.character(genre), ",")) %>% #tells r to divide up the phrase based on where there are commas
    unnest(genre)

factored_game_genres <- unnested_genres %>%
    filter (!is.na(genre))%>%
    mutate(genre = factor(genre,
                          levels = c("Action","Adventure","Massively Multiplayer", "Strategy","Free to Play","RPG",
                                     "Indie" , "Early Access","Simulation","Racing","Casual","Sports",
                                     "Violent","Gore","Valve","Nudity","Animation & Modeling","Design & Illustration",
                                     "Utilities","Sexual Content","Game Development","Education","Software Training",
                                     "Web Publishing","Video Production","Audio Production","Movie",
                                     "Photo Editing","Accounting","Documentary","Short", "360 Video","Tutorial","HTC")
    )
    )
#bringing the original prices and game genre information together
game_genres_OP <- factored_game_genres %>%
    left_join(original_game_prices)

viewable <- game_genres_OP %>%
    select(id,name,recent_reviews, release_date,developer,genre,languages,original_price,url,recommended_requirements)

viewable_impression <- viewable %>%
    separate(recent_reviews,
             into=c("impression",NA),
             sep=",",
             extra="merge",
             remove=TRUE)%>%
    mutate(
        impression = factor(impression,
                            levels = c("Overwhelmingly Negative", "Very Negative", "Negative", "Mostly Negative",
                                       "Mixed",
                                       "Mostly Positive", "Positive", "Very Positive", "Overwhelmingly Positive")
        )#factor order is so that "Overwhelmingly positive" is shown at the top of a flipped axes plot
    ) %>%
    filter (!is.na(impression))

viewable_date <- viewable_impression %>%
    filter(!is.na(release_date), release_date !="NaN") %>% #remove NAs and NANs
    mutate(pattern=get_pattern(release_date))%>% #identify pattern for release_date value
    filter(str_detect(pattern, "9999")) %>%
    mutate(release_date=as.Date(lubridate::parse_date_time(release_date, c("%m%d%y","%m%y","%d%m%y", "%y", "%y%m","%y%m%d"))))#providing for a variety of date formats

viewable_languages <- viewable_date %>%
    mutate(languages = strsplit(as.character(languages), ",")) %>%
    unnest(languages)%>%
    mutate(languages=as.factor(languages))

viewable_requirements <-  viewable_languages %>%
    extract(recommended_requirements,
            into=c(NA,"operating_system", "processor","memory_ram","graphics","available_storage","additional_notes"),
            regex="(.*):,OS:,(.*),Processor:,(.*),Memory:,(.*) RAM,Graphics:,(.*),Storage:,(.*) available space,Additional Notes:,(.*)",
            remove=TRUE)

steam <- viewable_requirements %>% select(-c(id,pattern))


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Steam Games Explorer"),

    # Sidebar with a slider input for price of game
    sidebarLayout(
        sidebarPanel(
        checkboxGroupInput("genreInput", "Select Your Preferred Genre(s) to Begin!",
                           c("Action","Adventure","Massively Multiplayer", "Strategy","Free to Play","RPG",
                                    "Indie" , "Early Access","Simulation","Racing","Casual","Sports")),
        sliderInput("priceInput",
                    "Price:",
                    min = 0,
                    max = 200,
                    c(10,50),
                    pre="$"),
        selectInput("languageInput", "Language",
                    choices = c("English","French","Italian","German","Spanish - Spain","Japanese"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
            tabPanel("General Info",plotOutput("distPlot"),
            br(), br(),
            DT::dataTableOutput("results")),
            tabPanel("Recommended Requirements", DT::dataTableOutput("requirements"))
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    filtered <- reactive({steam %>%
        filter(original_price >= input$priceInput[1],
               original_price <= input$priceInput[2],
               genre %in% input$genreInput,
               languages == input$languageInput
        )
    })
    output$distPlot <- renderPlot({
        ggplot(filtered(), aes(impression)) +
            geom_histogram(stat="count")+
            ggtitle("The Distribution of Overall Impressions of Games on Steam")+
            xlab("Review Impression")+
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    output$results <- DT::renderDataTable({
        filtered() %>%
            select(-c(genre,languages,operating_system, processor,memory_ram,graphics,available_storage,additional_notes))%>%
            distinct(name, .keep_all = TRUE)
    })
    
    output$requirements <- DT::renderDataTable({
        filtered() %>%
        select(c(name,operating_system, processor,memory_ram,graphics,available_storage,additional_notes))%>%
        filter(!is.na(operating_system))%>%
        distinct(name, .keep_all = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
