library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel(title = 'Drag Race Visualizer'),
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      'Filters',
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
