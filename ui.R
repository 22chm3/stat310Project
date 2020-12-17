library(shiny)
library(plotly)

shinyUI(fluidPage(

    titlePanel("IMDB Movie Data"),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput("genreSelect"),
            uiOutput("countrySelect"),
            uiOutput("languageSelect"),
            checkboxInput("sizeControl",
                          "Show vote count as point size")
        ),

        mainPanel(
            plotlyOutput("moviePlot"),
            tableOutput("movieInfo1"),
            tableOutput("movieInfo2"),
            tableOutput("movieInfo3")
            
        )
    )
))
