library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)

shinyServer(function(input, output, session) {
    movies <- read_csv("movies.csv")
    movieData <- reactive({
        req(input$countrySelect, input$languageSelect, input$genreSelect)
        data <- movies %>%
            # filter(grepl(input[[countrySelect]], country, fixed=TRUE),
            #        grepl(input[[languageSelect]], language, fixed=TRUE),
            filter(primaryGenre %in% input$genreSelect)
        if(input$countrySelect != "All Countries") {
            data <- data %>% filter(primaryCountry == input$countrySelect)
        }
        if(input$languageSelect != "All Languages") {
            data <- data %>% filter(primaryLanguage == input$languageSelect)
        }
        if(input$sizeControl == TRUE) {
            data$size <- data$votes
        }
        data
    })
    
    sizes <- reactive({
        if(input$sizeControl == TRUE) {
            c(3,50)
        } else {
            5
        }
    })
    # Make sure to include tabs
    # 3-4 minute video narrating how to use the app, talk about anything interesting I find
    
    # Allow filtering by number of votes
    
    # For tomorrow: first make smaller dataset with only data points with budget and revenue
    # Check if worldwide and US revenue are really different (they probably are for intl films)
    # Then make scatterplot of budget vs revenue, give some filtering options
    # Maybe put a couple buttons like "highlight cult classics" that color points and makes an annotation
    # pop up explaining what the button did, include "reset plot" button? Idk
    # Third tab: bar chart of most popular? Best and worst rated? Ratings over time histogram? For 
    # univariate stuff, just filter out everything that doesn't have nice values
    
    output$genreSelect <- renderUI({
        data <- movies %>% arrange(primaryGenre)
        selectInput("genreSelect", 
                    "Select genres:",
                    choices = unique(data$primaryGenre),
                    selected = c("Romance", "Film-Noir"),
                    multiple=TRUE)
    })
    
    output$countrySelect <- renderUI({
        data <- movies %>% arrange(primaryCountry) %>% select(primaryCountry) %>% drop_na()
        selectInput("countrySelect", 
                    "Select country:",
                    choices = c("All Countries", unique(data$primaryCountry)),
                    selected = "USA")
    })
    
    output$languageSelect <- renderUI({
        data <- movies %>% arrange(primaryLanguage) %>% select(primaryLanguage) %>% drop_na()
        selectInput("languageSelect", 
                    "Select language:",
                    choices = c("All Languages", unique(data$primaryLanguage)),
                    selected = "English")
    })

    output$moviePlot <- renderPlotly({
        # Filter the movies to display based on inputs
        
        #     filter(Year == input$year)
        # if(input$choices == "regions") {
        #     yearData <- yearData %>% filter(Region %in% input$region)
        # }
        # if(input$choices == "countries") {
        #     yearData <- yearData %>% filter(Country %in% input$country)
        # }
        
        pal <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', 
                 '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', 
                 '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9', '#000000')
        pal <- setNames(pal, c("Romance","Biography","Drama","Adventure","History","Crime","Western",
                               "Fantasy","Comedy","Horror","Family","Action","Mystery","Sci-Fi",
                               "Animation","Thriller","Musical","Music","War","Film-Noir","Sport"))
        
        p <- plot_ly(data=movieData(), 
                     source = "moviePlot",
                     key = ~imdb_title_id,
                     type="scatter",
                     mode="markers",
                     x=~year,
                     y=~avg_vote,
                     size=~size,
                     sizes=sizes(),
                     marker=list(sizemode="diameter"),
                     color=~primaryGenre,
                     colors=pal,
                     alpha = 0.5,
                     name=~primaryGenre,
                     text=~original_title,
                     hoverinfo="text"
                     ) %>%
            layout(title=list(text="Movies Over the Years by Ratings"),
                   xaxis=list(title="Year", range=c(1890, 2022)),
                   yaxis=list(title="Rating", range=c(0,10), zeroline=FALSE),
                   legend=list(x=0.5, y=-0.2, xanchor="center",
                               orientation="h", itemsizing="constant")
                   ) %>%
            config(displayModeBar = F)
        p

    })
    
    output$movieInfo1 <- renderTable({
        clickData <- event_data("plotly_click", source = "moviePlot")
        if(is.null(clickData)) {
            "Click on a movie to view more information about it!"
        } else {
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                mutate(year = as.integer(year), 
                       duration = as.integer(duration),
                       votes = as.integer(votes)) %>%
                select(original_title, year, genre, duration, country, director, writer) %>%
                rename(Title=original_title, Year=year, `Genre(s)`=genre, `Duration (minutes)`=duration,
                       Country=country, `Director(s)`=director, `Writer(s)`=writer)
            selectedMovie
        }
        
    })
    
    output$movieInfo2 <- renderTable({
        clickData <- event_data("plotly_click", source = "moviePlot")
        if(is.null(clickData)) {
            NULL
        } else {
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                mutate(year = as.integer(year), 
                       duration = as.integer(duration),
                       votes = as.integer(votes)) %>%
                select(avg_vote, votes, budget, usa_gross_income, worlwide_gross_income, metascore) %>%
                rename(`Average User Score`=avg_vote, `Number of Votes`=votes, Budget=budget,
                       `Gross Income (USA)`=usa_gross_income, 
                       `Gross Income (Worldwide)`=worlwide_gross_income, Metascore=metascore)
            selectedMovie
        }
    })
    
    output$movieInfo3 <- renderTable({
        clickData <- event_data("plotly_click", source = "moviePlot")
        if(is.null(clickData)) {
            NULL
        } else {
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                mutate(year = as.integer(year), 
                       duration = as.integer(duration),
                       votes = as.integer(votes)) %>%
                select(description) %>%
                rename(Description=description)
            selectedMovie
        }
    })

})










