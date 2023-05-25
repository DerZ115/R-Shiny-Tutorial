#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages 

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

# Get the data

file <- "https://github.com/rstudio-education/shiny-course/raw/main/movies.RData"
destfile <- "movies.RData"

download.file(file, destfile)

# Load data 

load("movies.RData")

n_total <- nrow(movies)
all_studios <- sort(unique(movies$studio))

# Define UI 

ui <- fluidPage(
  
  sidebarLayout(
    
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("IMDB Rating" = "imdb_rating", 
                    "IMDB Votes" = "imdb_num_votes", 
                    "Critics Score" = "critics_score", 
                    "Audience Score" = "audience_score", 
                    "Runtime" = "runtime"),
        selected = "imdb_rating"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("IMDB Rating" = "imdb_rating", 
                    "IMDB Votes" = "imdb_num_votes", 
                    "Critics Score" = "critics_score", 
                    "Audience Score" = "audience_score", 
                    "Runtime" = "runtime"),
        selected = "imdb_rating"
      ),
      # Select color variable
      selectInput(
        inputId = "z",
        label = "Color by...",
        choices = c("Type" = "title_type", 
                    "Genre" = "genre", 
                    "MPAA Rating" = "mpaa_rating", 
                    "Critics Rating" = "critics_rating", 
                    "Audience Rating" = "audience_rating"),
        selected = "mpaa_rating"
      ),
      # Set alpha value
      sliderInput(
        inputId = "alpha",
        label = "Opacity",
        min = 0, max = 1,
        value = 1
      ),
      # Show data table
      checkboxInput(
        inputId = "show_data",
        label = "Show data table", 
        value = FALSE),
      
      HTML(paste("Enter a value between 1 and", n_total)),
      
      numericInput(
        inputId = "n",
        label = "Sample size:",
        value = 30,
        step = 1,
        min = 1, max = n_total
      )
    ),
    
    # Output: Show scatterplot
    mainPanel(
      
      plotOutput(outputId = "scatterplot"),
      
      plotOutput(outputId = "densityplot", height = 200),
      
      DTOutput(outputId = "moviestable")
      
    )
  )
)

# Define server 

server <- function(input, output, session) {
  
  output$scatterplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x, y = input$y, col = input$z)) +
      geom_point(alpha = input$alpha)
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = movies, aes_string(x = input$x)) +
      geom_density()
  })
  
  # Print data table if checked 
  output$moviestable <- renderDT({
    req(input$n)
    if(input$show_data){
      datatable(data = movies %>% 
                  sample_n(input$n) %>%
                  select(title:studio),
                options = list(pageLength = 10),
                rownames = FALSE)
    }
  })
}

# Create a Shiny app object 

shinyApp(ui = ui, server = server)
