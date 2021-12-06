# Ballin Assignment B3 & B4

This repository holds the files for Mya Ballin's project work for Assignments B3 - Option B and B4 — Option C for STAT545B: Exploratory Data Analysis at UBC. Option B for assignment B3 asked students to create their own shiny app with three features and deploy it and Option C for assignment B4 asked students to continue to add features to their app.

## Running Instance

If you would like to view and test out the application created for this project, it is available at the following link: https://mxballin.shinyapps.io/steamexplorer2/

The original version of the app created for Assignment B3 can be viewed here: https://mxballin.shinyapps.io/steamexplorer/

## Description of the Application

This application seeks to allow the user to explore the the 'Steam Games' dataset that is available through the 'datateachr' package and can also be viewed on Kaggle here: https://www.kaggle.com/trolukovich/steam-games-complete-dataset. It provides an opportunity for the user to select a genre or multiple genres of games they would be interested in playing and to refine their search using several different factors like language and preferred operating system.

The application offers two different displays, one that focuses on general information about the games that are within the user's criteria and the reviews that they have received on Steam and another that allows the user to view the recommended system requirements for games that have provided such information and meet the criteria.

#### Application Features

Created for Assignment B4
1. For this part of the assignment, the UI of the app was changed to utilize the features of the package 'shinydashboard'. Features such as boxes and info boxes as well as the sidebar menu were incorporated.
2. Existing featured were added to and expanded upon, such as the incorporation of a responsive graph using the package 'plotly' to replace the original, static ggplot. Additional filters were created on the recommended hardware/software requirements tab that allow users to filter that table based on their computer setup.
3. Another feature that was added for assignment B4 is the option for a user to download the filtered data as a csv file.

Created for Assignment B3

1. A sidebar panel with multiple select, slider, and single select filters that allow for user filtering of the data. This enables users to select and view only the data that they are interested in.
2. Two main panel tabs offering general information about the game and its review impressions and recommended hardware/software requirements on separate views. Creating the two tabs enables the user to more easily navigate all of the data they are being presented in the tables.
3. Interactable data tables that allow the user to further sort the information they are being provided. This allows the user to prioritize certain aspects of the information in the tables (for example price high to low or low to high).

## Documents

This repository contains the following folder:

steamexplorer2 - which contains the code for the new Assignment B4 Shiny application and deployment folder generated by the package rsconnect that is related to its hosting on shinyapps.io.
steamexplorer - which contains the code for the original Assignment B3 Shiny application and deployment folder generated by the package rsconnect that is related to its hosting on shinyapps.io.

## Running the code

The app.R documents within the steamexplorer and steamexplorer2 folders can be pulled to your local machine, modified, and run in Rstudio to create your own instance of the application. To deploy your own instance of the app, check out the information provided on the shinyapps.io 'Getting Started' page: https://shiny.rstudio.com/articles/shinyapps.html

=======
