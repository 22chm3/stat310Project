library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)

shinyServer(function(input, output, session) {
    movies <- read_csv("movies.csv")
    movieData <- reactive({
        movies %>%
            filter(grepl("USA", country, fixed=TRUE),
                   grepl("English", language, fixed=TRUE),
                   primaryGenre %in% input$genreSelect)
    })
    # Make sure to include tabs
    # 3-4 minute video narrating how to use the app, talk about anything interesting I find
    
    output$genreSelect <- renderUI({
        data <- movies %>% arrange(primaryGenre)
        selectInput("genreSelect", 
                    "Choose genres:",
                    choices = unique(data$primaryGenre),
                    selected = "Romance",
                    multiple=TRUE)
    })
    
    
    # Load genres into selection menu
    # observeEvent(input$genreSelect, {
    #     data <- movies %>% arrange(primaryGenre)
    #     updateSelectInput(session, "genreSelect", choices=unique(data$primaryGenre))
    # },
    # once = TRUE)

    output$moviePlot <- renderPlotly({
        # Filter the movies to display based on inputs
        
        #     filter(Year == input$year)
        # if(input$choices == "regions") {
        #     yearData <- yearData %>% filter(Region %in% input$region)
        # }
        # if(input$choices == "countries") {
        #     yearData <- yearData %>% filter(Country %in% input$country)
        # }
        
        # pal <- c("#003f5c", "#374c80", "#7a5195", "#bc5090", "#ef5675", "#ff764a", "#ffa600")
        # pal <- setNames(pal, c("East Asia & Pacific", 
        #                        "Middle East & North Africa", "North America", "South Asia",
        #                        "Sub-Saharan Africa"))
        
        p <- plot_ly(data=movieData(), 
                     source = "moviePlot",
                     key = ~imdb_title_id,
                     type="scatter",
                     mode="markers",
                     x=~year,
                     y=~avg_vote,
                     #size=~Population,
                     #sizes=c(5,50),
                     #marker=list(sizemode="diameter"),
                     color=~primaryGenre,
                     #colors=pal,
                     alpha = 0.5,
                     name=~primaryGenre,
                     text=~title,
                     hoverinfo="text"
                     ) %>%
            layout(title=list(text="Movies Over the Years by Ratings"),
                   xaxis=list(title="Year"),
                   yaxis=list(title="Rating", range=c(0,10)),
                   legend=list(x=0.5, y=-0.2, xanchor="center",
                               orientation="h", itemsizing="constant")
                   ) %>%
            config(displayModeBar = F)
        p

    })
    
    output$movieInfo <- renderTable({
        clickData <- event_data("plotly_click", source = "moviePlot")
        if(is.null(clickData)) {
            print("Click on a movie to view more information about it!")
        } else {
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                mutate(year = as.integer(year), 
                       duration = as.integer(duration),
                       votes = as.integer(votes)) %>%
                select(title, year, genre, duration, country, director, writer, avg_vote, votes,
                       budget, usa_gross_income, worlwide_gross_income, metascore, description)
            selectedMovie
        }
        
    })

})
