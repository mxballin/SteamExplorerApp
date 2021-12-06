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
library(tidyverse)
library(bpa)
library(ggplot2)
library(plotly)
library(shinydashboard)

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
    
    #fixing some os requirement information for filtering purposes
    osfix <- viewable_requirements%>%
        mutate(pattern=get_pattern(operating_system))%>%
        mutate(operating_system=if_else(str_detect(pattern,"^99\\.99\\.|^99\\.9\\."), paste("macOS", operating_system),operating_system ))%>%
        mutate(operating_system=if_else(str_detect(operating_system,"^OS"), paste("Mac", operating_system),operating_system))
    
    #fixing some storage requirement information for filtering purposes
    storagefix <- osfix %>%
        mutate(storage=as.numeric(str_remove_all(available_storage,"GB|MB")))%>%
        mutate(storage=if_else(str_detect(available_storage,"GB"),storage*1024,storage))
    
    #hyperlinking URLs
    #source:https://stackoverflow.com/questions/43771148/how-to-rename-columns-in-a-shiny-rendertable
    url <- storagefix %>%
        mutate(url=paste0("<a href='",url,"'>",url,"</a>"))
    
    #subsetting the data
    steam <- url %>% select(-c(id,pattern))
    

# Define UI for application
# New Feature: Using shinydashboard to create a smoother looking UI
ui <- dashboardPage(
    # Application title
    dashboardHeader(title="Steam Games Explorer 2"),

    # Sidebar with filtering options as well as display tabs
    dashboardSidebar(
        sidebarMenu(
            menuItem("General Information", tabName = "general", icon=icon("info-sign", lib = "glyphicon")),
                    menuItem("Recommended Requirements", tabName = "requirements", icon=icon("cog", lib = "glyphicon")),
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
                    #New Feature: Offering the option to download the filtered data
                    "Want to explore the data on your own? You can download the filtered version of the data by clicking the button below.",br(),
                    downloadButton("filterdownload","Download the Filtered Data")
                    ),
    

        #Original Feature: Two tabs offering general information about the game and its review impressions and recommended hardware/software requirements
        #Creating the two tabs enables the user to more easily navigate all of the data they are being presented in the tables.
        dashboardBody(
            tabItems(
            tabItem(tabName = "general",
                    box(width=NULL, background="light-blue","Welcome to the Steam Games Explorer! We are here to help you find games that you would be interested in playing."),
                    h2("Basic Game Information"),
                    fluidRow(infoBoxOutput("resultsBox")),
                    fluidRow(
                    box(width = NULL, background = "purple",
                                  "Don't see any results? Make sure you have selected at least one genre from the options on the left.")),
                    box(title ="The Distribution of Review Impressions on Steam of Games in the Selected Genre(s)",
                        width = NULL, solidHeader = TRUE, plotlyOutput(outputId = "p")),
                    box(title = "About Review Impressions",width = NULL, solidHeader = TRUE,
                        "In order to help you identify games that are popular and of high quality, Steam has summarized the general response of a game.
                        Using data from submitted reviews, games are assigned an 'impression' that indicates what the overall character of reviews has been like. This scale runs from 'Overwhelmingly Negative' to 'Overwhelmingly Positive'."
                        ),
                    box(title = "Games that Match Your Preferences",
                        width = NULL, solidHeader = TRUE, DT::dataTableOutput("results"))),
        #New Feature: Additional filtering enabled on the Requirements Panel to enable more specific filtering for the type of information a user would be interested in
            tabItem(tabName="requirements",
                    h2("Recommended Requirements"),
                    box(title = "Filter for your system specs", width = NULL, status = "primary", solidHeader = TRUE,
                        checkboxGroupInput("osInput", "Select Your Preferred Operating System",
                            c("Windows","Mac","Linux", "Ubuntu","SteamOS"),
                            selected="Windows"),
                        sliderInput("storageInput",
                            "Minimum Available Storage:",
                             min = 0,
                             max = 87040,
                             c(0,51200),
                             post="MB"),
                             textOutput("GBconversion")),
                    box(width = NULL, background = "purple",
                     "Note: If you do not see the requirements for a game you are interested in listed here, the developer did not provide this information to Steam."),
                    box(title = "Games that Meet Your Requirement Specifications",
                        width = NULL, solidHeader = TRUE,
                        DT::dataTableOutput("requirements"))
        )
    )
))

# Define server logic
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
    

#Sidebar
    output$filterdownload <- downloadHandler(
        filename = function() {
            paste("steamdata-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(filtered(), file)
        }
    )
#Recommended Requirements Tab
    #results info box
    output$resultsBox <- renderInfoBox({
        count <- filtered()%>%nrow()
        infoBox("Results:", paste0(count, " games"), icon = icon("knight", lib="glyphicon"),
            color = "purple"
        )
    })
    #plotting the distribution of impressions
    #New Feature: Using Plotly to render the original ggplot in order to offer interaction with the plot
    output$p <- renderPlotly({
        counts <- filtered() %>% count(genre,impression,name="count")
        gg<-ggplot(counts, aes(x=impression,y=count,fill=genre,customdata=genre)) +
            geom_bar(position="stack", stat="identity")+
            xlab("Review Impression")+
            ylab("Number of Games")+
            theme_minimal()
        ggplotly(gg)
        
    })

    #Original Feature: Interactable data tables that allow the user to further sort the information they are being provided
    #This allows the user to prioritize certain aspects of the information in the tables without overwhelming them with too many filter options in the sidebar panel.
   #general info table
     output$results <- DT::renderDataTable({
        DT::datatable(
        filtered() %>%
            select(-c(genre,languages,operating_system, processor,memory_ram,graphics,available_storage,additional_notes,storage))%>%
            distinct(name, .keep_all = TRUE)%>%
        rename(Name=name, "Review Impression"=impression, "Release Date"=release_date,"Developer"=developer,"Original Price"=original_price,"URL"=url),
        escape = FALSE)
    })

#Recommended Requirements Tab
     #passing the selected requirements filters to filter the dataset
     reqs <- reactive({
         filtered() %>%
         select(c(name,operating_system, processor,memory_ram,graphics,available_storage,additional_notes,storage))%>%
         filter(grepl(input$osInput,operating_system),
                storage >= input$storageInput[1],
                storage <= input$storageInput[2])%>%
         distinct(name, .keep_all = TRUE)
})
     #conversion from MB to GB text for minimum available storage slider
     output$GBconversion <- renderText({ 
         paste("The current selection of available storage in GB is:",input$storageInput[1]/1024, "to", input$storageInput[2]/1024, "GB")
     })
     
    #Requirements table
    output$requirements <- DT::renderDataTable({reqs()%>%
        select(-c(storage))%>%
        rename(Name = name, "Operating System" = operating_system,"Processor"=processor,"RAM"=memory_ram,"Graphics Processor"=graphics,"Available Storage"=available_storage,"Additional Notes"=additional_notes)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
