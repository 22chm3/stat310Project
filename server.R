library(shiny)
library(tidyverse)
library(plotly)
library(rsconnect)

shinyServer(function(input, output, session) {
    movies <- read_csv("movies.csv")
    movieData <- reactive({
        req(input$countrySelect, input$languageSelect, input$genreSelect)
        data <- movies %>%
            filter(year >= input$yearSelect[1],
                   year <= input$yearSelect[2],
                   votes >= input$votesSelect)
        if(input$countrySelect != "All Countries") {
            data <- data %>% filter(primaryCountry == input$countrySelect)
        }
        if(input$languageSelect != "All Languages") {
            data <- data %>% filter(primaryLanguage == input$languageSelect)
        }
        if(!"All Genres" %in% input$genreSelect) {
            data <- data %>% filter(primaryGenre %in% input$genreSelect)
        }
        if(input$sizeControl == TRUE) {
            data$size <- data$votes
        }
        if(input$mustSee == TRUE) {
            data <- data %>% filter(mustSee == TRUE)
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
    
    output$genreSelect <- renderUI({
        data <- movies %>% arrange(primaryGenre)
        selectInput("genreSelect", 
                    "Select genres:",
                    choices = c("All Genres", unique(data$primaryGenre)),
                    selected = c("Romance", "Film-Noir", "Family"),
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

    output$ratingsPlot <- renderPlotly({
        # Generate color palette for genres
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
                     hovertext=paste0("<b>",movieData()$original_title, "</b>",
                                      "<br><b>Release Year: </b>", movieData()$year,
                                      "<br><b>Average User Rating: </b>", movieData()$avg_vote),
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
            # Select the movie the user clicked on and a subset of variables
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
            # Select the movie the user clicked on and a subset of variables
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                mutate(votes = scales::comma(votes),
                       avg_vote = as.character(round(avg_vote, 1)),
                       metascore = as.integer(metascore)) %>%
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
            # Select the movie the user clicked on and a subset of variables
            selectedMovie <- movieData() %>%
                filter(imdb_title_id == clickData$key) %>%
                select(description) %>%
                rename(Description=description)
            selectedMovie
        }
    })
    
    output$genreSelectRev <- renderUI({
        data <- movies %>% arrange(primaryGenre)
        selectInput("genreSelectRev", 
                    "Select genres:",
                    choices = c("All Genres", unique(data$primaryGenre)),
                    selected = "All Genres",
                    multiple=TRUE)
    })
    
    output$revenuePlot <- renderPlotly({
        data <- movies
        newData <- data %>% drop_na(c(budgetNum, incomeNum)) %>%
                            filter(isDollar == TRUE) %>% 
                            mutate(incomeNum = as.numeric(incomeNum),
                                   budgetNum = as.numeric(budgetNum))
        if(!"All Genres" %in% input$genreSelectRev) {
            newData <- newData %>% filter(primaryGenre %in% input$genreSelectRev)
        }
        if(input$mustSeeRev == TRUE) {
            newData <- newData %>% filter(mustSee == TRUE)
        }
        # Create color palette for genres
        pal <- c('#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#42d4f4', 
                 '#f032e6', '#bfef45', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', 
                 '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#a9a9a9', '#000000')
        pal <- setNames(pal, c("Romance","Biography","Drama","Adventure","History","Crime","Western",
                               "Fantasy","Comedy","Horror","Family","Action","Mystery","Sci-Fi",
                               "Animation","Thriller","Musical","Music","War","Film-Noir","Sport"))
        
        p <- plot_ly(data=newData, 
                     source = "revenuePlot",
                     key = ~imdb_title_id,
                     type="scatter",
                     mode="markers",
                     x=~budgetNum,
                     y=~incomeNum,
                     color=~primaryGenre,
                     colors=pal,
                     alpha = 0.5,
                     name=~primaryGenre,
                     hovertext=paste0("<b>",newData$original_title, "</b> (", newData$year, ")",
                                         "<br><b>Budget: </b>", newData$budget,
                                         "<br><b>Revenue: </b>", newData$worlwide_gross_income),
                     hoverinfo="text",
                     height=700
        ) %>%
            layout(title=list(text="Movie Budgets vs Revenue"),
                   xaxis=list(title="Budget", range=c(0,350000000)),
                   yaxis=list(title="Worldwide Revenue", range=c(0,2830000000)),
                   legend=list(x=0.5, y=-0.15, xanchor="center",
                               orientation="h", itemsizing="constant")
            ) %>%
            config(displayModeBar = F)
        if(input$breakEvenLine == TRUE) {
            evenLine <- list(type="line", line=list(color="black"),xref="x",yref="y",
                             x0=0, x1=350000000, y0=0, y1=350000000)
            p <- p %>% layout(shapes = evenLine)
        } 
        p
    })
    
    output$dataDescription <- renderText(
        "The data used for this project comes from this 
        <a href=https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset>Kaggle project</a>.
        It was obtained by scraping IMDB's website, the largest and most complete internet database 
        of movies and TV shows. It contains 23 variables for 85,855 movies, including ratings, 
        main cast, description, runtime, genre, and budget/revenue. The dataset was last updated 
        in January 2020 and includes all movies with at least 100 user votes on the website."
    )
    
    output$plotDescription <- renderText(
        "The 'ratings' plot displays the average IMDB user rating for movies over the years. You can
        filter based on genre, country, language, year, and minimum number of user votes. Showing the 
        vote count as point size helps give an indication of movie popularity (good or bad), and clicking
        a movie displays more information about it below the plot. 'Must-see' movies are 
        <a href=https://www.metacritic.com/about-metascores>designated</a> by Metacritic, and
        are movies with a Metascore greater than 80 and more than 15 critic reviews. A movie's Metascore
        is calculated by compiling critic reviews and taking a normalized weighted average.<br>
        The 'revenue' plot plots movies' budget against their worldwide revenue. Movies above the 
        'break even' line made a profit, while those below were less successful."
    )

})










