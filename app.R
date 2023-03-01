library(shiny)
library(dplyr)
library(ggplot2)

# read in data
drag_df <- read.csv("data/drag.csv")

ui <- fluidPage(
  titlePanel(title = 'Drag Race Visualizer'),
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      'Filters',
      selectInput(inputId = "season", label = "Season",
                  choices = unique(drag_df$season)),
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
  # reactive expression to filter data based on user selections
  filtered_data <- reactive({
    drag_filtered <- drag_df |>
      dplyr::filter(season == input$season) |>
      dplyr::filter(if ("Miss Congeniality" %in% input$other_categories) missc == 1 else TRUE) |>
      dplyr::filter(if ("Winner" %in% input$other_categories) winner == 1 else TRUE) |>
      dplyr::filter(if ("Finalist" %in% input$other_categories) finalist == 1 else TRUE) |>
      dplyr::filter(if ("First Eliminated" %in% input$other_categories) first_eliminated == 1 else TRUE)
    drag_filtered
  })

  # TBD: Create outcome tally table

}

shinyApp(ui, server)
