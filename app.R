library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

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
      selectizeInput(
        inputId = 'queens',
        label = "Queens",
        choices = unique(drag_df$contestant),
        multiple = TRUE
      ),
      # Other Categories filter
      checkboxGroupInput(inputId = "other_categories", label = "Other Categories",
                         choices = c("Miss Congeniality", "Winner", "Finalist", "First Eliminated"),
                         selected = NULL),
      
      # Age slider filter
      sliderInput(inputId = "age", label = "Age",
                  min = min(drag_df$age, na.rm = TRUE),
                  max = max(drag_df$age, na.rm = TRUE),
                  value = c(min(drag_df$age, na.rm = TRUE), max(drag_df$age, na.rm = TRUE)))
    ),
    # main body (graphs)
    mainPanel(
      'Cool graphs',
      fluidRow(),
      fluidRow(
        column(6,
          h3('Relative Rankings'),
          dataTableOutput('ranking')
        )
      )
    )
  )

)

server <- function(input, output, session) {
  # reactively changes selectable queens based on chosen season
  observe({
    req(input$season)
    
    # filter data based on chosen season and get unique names
    filtered_names <- drag_df |> 
      dplyr::filter(season == input$season) |> 
      dplyr::select(contestant) |> 
      unique()
    
    updateSelectizeInput(
      inputId = 'queens',
      choices = filtered_names
    )
  }) |> bindEvent(input$season)
  
  # reactive expression to filter data based on user selections
  filtered_data <- reactive({
    drag_filtered <- drag_df |>
      dplyr::filter(season == input$season) |>
      dplyr::filter(if ("Miss Congeniality" %in% input$other_categories) missc == 1 else TRUE) |>
      dplyr::filter(if ("Winner" %in% input$other_categories) winner == 1 else TRUE) |>
      dplyr::filter(if ("Finalist" %in% input$other_categories) finalist == 1 else TRUE) |>
      dplyr::filter(if ("First Eliminated" %in% input$other_categories) first_eliminated == 1 else TRUE) |>
      dplyr::filter(age >= input$age[1] & age <= input$age[2])
      
    if (!is.null(input$queens)) {
      drag_filtered <- drag_df |> 
        dplyr::filter(contestant %in% input$queens)
    }
    drag_filtered
  })

  # TBD: Create outcome tally table
  
  
  # ranking table
  output$ranking <- renderDT({

    if (nrow(filtered_data()) != 0) {
      filtered_data() |> 
        dplyr::filter(participant == 1) |> 
        dplyr::group_by(season, rank, contestant) |> 
        dplyr::summarise(challenges = n(), .groups = 'drop') |>
        dplyr::arrange(rank)
    } else { # don't do any filtering if there aren't rows
      filtered_data()
    }
    
  }, rownames = FALSE)
}

shinyApp(ui, server)
