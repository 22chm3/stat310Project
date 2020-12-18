library(shiny)
library(plotly)

shinyUI(fluidPage(
    tabsetPanel(
        tabPanel("Ratings", fluid=TRUE,
                 titlePanel("IMDB Movie Data"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("genreSelect"),
                         uiOutput("countrySelect"),
                         uiOutput("languageSelect"),
                         sliderInput("yearSelect",
                                     "Select year range:",
                                     min = 1894, 
                                     max = 2020,
                                     value = c(1894, 2020),
                                     sep=""),
                         sliderInput("votesSelect",
                                     "Minimum number of user votes:",
                                     min = 100,
                                     max = 10000,
                                     value = 100),
                         checkboxInput("sizeControl",
                                       "Show vote count as point size"),
                         checkboxInput("mustSee",
                                       "Only display 'must-see' movies")
                     ),
                     
                     mainPanel(
                         plotlyOutput("ratingsPlot"),
                         tableOutput("movieInfo1"),
                         tableOutput("movieInfo2"),
                         tableOutput("movieInfo3")
                         
                     )
                 )),
        
        tabPanel("Revenue", fluid=TRUE,
                 titlePanel("IMDB Movie Data"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         uiOutput("genreSelectRev"),
                         checkboxInput("breakEvenLine",
                                       "Show break-even line"),
                         checkboxInput("mustSeeRev",
                                       "Only display 'must-see' movies")
                     ),
                     
                     mainPanel(
                         plotlyOutput("revenuePlot")
                     )
                 )),
        
        tabPanel("About the App", fluid=TRUE,
                 titlePanel("About the Data"),
                 htmlOutput("dataDescription"),
                 titlePanel("About the Plots"),
                 htmlOutput("plotDescription")
                 )
    )
    
))
