library(shiny)
library(plotly)

shinyUI(fluidPage(

    titlePanel("IMDB Movie Data"),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("genreSelect")
        ),

        mainPanel(
            plotlyOutput("moviePlot"),
            tableOutput("movieInfo")
            
        )
    )
))
