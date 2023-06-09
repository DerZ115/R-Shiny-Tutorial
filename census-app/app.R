library(shiny)
library(maps)
library(mapproj)

source("helpers.R")
counties <- readRDS("data/counties.rds")

# Define UI ----
ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographics maps with information from the 2010 US Census."),
      selectInput("var", label = "Choose a variable to display",
                  choices = list("Percent White",
                                 "Percent Black",
                                 "Percent Hispanic",
                                 "Percent Asian"),
                  selected = 1),
      sliderInput("range", "Range of interest:",
                  min = 0, max = 100, value = c(0, 100))
      ),
    
    mainPanel(plotOutput("map"))
  )
  
)

# Define server logic ----
server <- function(input, output) {
  
  output$map <- renderPlot({
    data <- switch(input$var, 
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var, 
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "orange",
                    "Percent Asian" = "darkmagenta")
    
    percent_map(var = data, 
                color = color, 
                legend.title = input$var, 
                max = input$range[2], 
                min = input$range[1])
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)