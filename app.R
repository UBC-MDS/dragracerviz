library(shiny)
library(dplyr)
library(ggplot2)

# read in data
data <- read_csv("data/drag.csv")

ui <- fluidPage(
  titlePanel(title = 'Drag Race Visualizer'),
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      'Filters',
      # Other Categories filter
      checkboxGroupInput(inputId = "other_categories", label = "Other Categories",
                         choices = c("Miss Congeniality", "Winner", "Finalist", "First Eliminated"),
                         selected = NULL)
    ),
    # main body (graphs)
    mainPanel(
      'Cool graphs',
      fluidRow(),
      fluidRow()
    )
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
