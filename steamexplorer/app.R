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
#leveling factored genres
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

#subsetting the tibble to only include the desired rows
viewable <- game_genres_OP %>%
    select(id,name,recent_reviews, release_date,developer,genre,languages,original_price,url,recommended_requirements)

#isolating the "impression" 
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

#making sure the dates are recognizable as dates
viewable_date <- viewable_impression %>%
    filter(!is.na(release_date), release_date !="NaN") %>% #remove NAs and NANs
    mutate(pattern=get_pattern(release_date))%>% #identify pattern for release_date value
    filter(str_detect(pattern, "9999")) %>%
    mutate(release_date=as.Date(lubridate::parse_date_time(release_date, c("%m%d%y","%m%y","%d%m%y", "%y", "%y%m","%y%m%d"))))#providing for a variety of date formats

#unnesting languages
viewable_languages <- viewable_date %>%
    mutate(languages = strsplit(as.character(languages), ",")) %>%
    unnest(languages)%>%
    mutate(languages=as.factor(languages))

#extracting the recommended requirement information
viewable_requirements <-  viewable_languages %>%
    extract(recommended_requirements,
            into=c(NA,"operating_system", "processor","memory_ram","graphics","available_storage","additional_notes"),
            regex="(.*):,OS:,(.*),Processor:,(.*),Memory:,(.*) RAM,Graphics:,(.*),Storage:,(.*) available space,Additional Notes:,(.*)",
            remove=TRUE)

#subsetting the data
steam <- viewable_requirements %>% select(-c(id,pattern))


# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Steam Games Explorer"),

    # Sidebar with a slider input for price of game, preferred language and genres.
    sidebarLayout(
        #Feature: sidebar panel with multiple select, slider, and single select filters that allow for user filtering of the data. This enables users to select and view only the data that they are interested in.
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
        uiOutput("languageOutput")),

        # Feature: Two tabs offering general information about the game and its review impressions and recommended hardware/software requirements
        #Creating the two tabs enables the user to more easily navigate all of the data they are being presented in the tables.
        mainPanel(
            tabsetPanel(type = "tabs",
            tabPanel("General Info", h4("Basic Game Information"), "Don't see any information? Make sure you have selected at least one genre from the options on the left.", br(),br(),plotOutput("distPlot"),
            br(), br(),
            DT::dataTableOutput("results")),
            tabPanel("Recommended Requirements", h4("Recommended Requirements"), br(), br(),
                     "Note: If you do not see the requirements for a game you are interested in listed here, the developer did not provide this information to Steam. ",
                     br(), br(),DT::dataTableOutput("requirements"))
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
    #making the language select responsive to the data
    output$languageOutput <- renderUI({
        selectInput("languageInput", "Language",
                    sort(unique(steam$languages)),
                    selected = "English")
    })
    #passing the selected filters to filter the dataset
    filtered <- reactive({steam %>%
        filter(original_price >= input$priceInput[1],
               original_price <= input$priceInput[2],
               genre %in% input$genreInput,
               languages == input$languageInput
        )
    })
    #plotting the distribution of impressions
    output$distPlot <- renderPlot({
        ggplot(filtered(), aes(impression)) +
            geom_histogram(stat="count")+
            ggtitle("The Distribution of Review Impressions on Steam of Games in the Selected Genre(s)")+
            xlab("Review Impression")+
            ylab("Number of Games")+
            theme(plot.title = element_text(hjust = 0.5))
    })
    #Feature: Interactable data tables that allow the user to further sort the information they are being provided
    #This allows the user to prioritize certain aspects of the information in the tables without overwhelming them with too many filter options in the sidebar panel.
   #general info table
     output$results <- DT::renderDataTable({
        filtered() %>%
            select(-c(genre,languages,operating_system, processor,memory_ram,graphics,available_storage,additional_notes))%>%
            distinct(name, .keep_all = TRUE)
    })
    #Requirements table
    output$requirements <- DT::renderDataTable({
        filtered() %>%
        select(c(name,operating_system, processor,memory_ram,graphics,available_storage,additional_notes))%>%
        filter(!is.na(operating_system))%>%
        distinct(name, .keep_all = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
