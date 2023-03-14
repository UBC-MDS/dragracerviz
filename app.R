library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
library(leaflet)
library(thematic)

thematic::thematic_shiny()

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#FF1D8E",
  primary = "#FF1D8E",
  secondary = "#FF374B",
  heading_font = font_google("Lobster"),
  base_font = font_google("Signika Negative")
)

# read in data
drag_df <- read.csv("data/drag.csv")
ui <- fluidPage(
  theme = custom_theme,
  titlePanel(title = div(img(src ="logo.png", height = 100), 'Drag Race Visualizer')),
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      'Filters',
      width = 2,
      selectInput(inputId = "season", label = "Season",
                  choices = unique(sort(drag_df$season))),
      selectizeInput(
        inputId = 'queens',
        label = "Queens",
        choices = unique(drag_df$contestant),
        multiple = TRUE
      ),
      # Other Categories filter
      checkboxGroupInput(inputId = "other_categories", label = "Other Categories",
                         choices = c("Finalist", "Miss Congeniality", "Winner", "First Eliminated"),
                         selected = NULL),

      # Age slider filter
      sliderInput(inputId = "age", label = "Age",
                  min = min(drag_df$age, na.rm = TRUE),
                  max = max(drag_df$age, na.rm = TRUE),
                  value = c(min(drag_df$age, na.rm = TRUE), max(drag_df$age, na.rm = TRUE)))
    ),
    # main body (graphs)
    mainPanel(
      width = 10,
      fluidRow(
        column(7,
               h3("Hometown Map"),
               leafletOutput("hometown")
               ),
        column(5,
               h3("Queen Performance"),
               plotlyOutput("queen_challenge")
        ),
      fluidRow(
        column(6,
          h3('Relative Rankings'),
          'Ranking of the queen on their season and how many challenges they participated in on their season.',
          dataTableOutput('ranking')
        ),
        # Outcome tally table
        column(width=6,
               h3('Outcome Tallies'),
               'Total counts of each outcome over the season.',
               DT::DTOutput(outputId = 'outcome_table')
        )
      )
    )
  )
))


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
      dplyr::filter(if ("Finalist" %in% input$other_categories) finalist == 1 else TRUE) |>
      dplyr::filter(if ("Winner" %in% input$other_categories) winner == 1 else TRUE) |>
      dplyr::filter(if ("Miss Congeniality" %in% input$other_categories) missc == 1 else TRUE) |>
      dplyr::filter(if ("First Eliminated" %in% input$other_categories) first_eliminated == 1 else TRUE) |>
      dplyr::filter(age >= input$age[1] & age <= input$age[2])

    if (!is.null(input$queens)) {
      drag_filtered <- drag_df |>
        dplyr::filter(contestant %in% input$queens,
                      season == input$season)
    }
    drag_filtered
  })

  # ranking table
  output$ranking <- renderDT({

    if (nrow(filtered_data()) != 0) {
      clean_data <- filtered_data() |>
        dplyr::filter(participant == 1) 
        
    } else {
      clean_data <- filtered_data()
    }
    clean_data |>
      dplyr::group_by(season, rank, contestant) |>
      dplyr::summarise(Challenges = n(), .groups = 'drop') |>
      dplyr::arrange(rank) |>
      dplyr::rename(Queen = contestant,
                    Season = season,
                    Rank = rank) |>
      datatable(extensions = 'Scroller',
                #caption = 'Ranking of the queen on their season and how many challenges they participated in on their season.',
                options = list(deferRender = TRUE,
                               scrollX = 350,
                               scrollY = 350,
                               scroller = TRUE,
                               searching = FALSE
                ))
  })

  output$hometown <- renderLeaflet({
    if (nrow(filtered_data()) > 0){
    map_blank <- leaflet(data = filtered_data()) |>
      addTiles() |>
      addMarkers(
        ~lng,
        ~lat,
        popup = ~paste(contestant,
                       "<br>Hometown:", city, ",", state,
                       "<br>Age on Season:", age),
        label = ~as.character(contestant))
    } else {
      map_blank <- leaflet() |>
        addTiles() 
      }
    map_blank
      
})
  
  output$queen_challenge <- renderPlotly({
    plot_data <- filtered_data()  %>%
      dplyr::group_by(contestant, season, episode) %>%
      dplyr::summarise(
        outcome = if_else(outcome == "ELIM", "ELIMINATED", outcome)) %>%
      dplyr::arrange(season)
    
    plot_ly(plot_data, x = ~episode, 
            y = ~factor(outcome, levels= c("BTM", "LOW", "SAFE", "HIGH", "WIN")), 
            color = ~contestant, 
            type = "scatter", 
            mode = "lines+markers") %>%
      layout(title = "Performance of Queen over time",
             xaxis = list(title = "Episodes"),
             yaxis = list(title = "Performance"),
             legend = list(orientation = "h",
                           xanchor = "center"))
  })
  
  # Outcome tally table
  output$outcome_table <- renderDataTable({
    data <- filtered_data()
    data |>
      dplyr::group_by(contestant) |>
      dplyr::summarize(WIN = sum(outcome == "WIN", na.rm = TRUE),
                       HIGH = sum(outcome == "HIGH", na.rm = TRUE),
                       SAFE = sum(outcome == "SAFE", na.rm = TRUE),
                       LOW = sum(outcome == "LOW", na.rm = TRUE),
                       BOTTOM = sum(outcome == "BTM", na.rm = TRUE)) |>
      dplyr::rename(Queen = contestant) |>
      datatable(rownames = FALSE,
                #caption = 'Total counts of each outcome over the season.',
                extensions = 'Scroller',
                options = list(deferRender = TRUE,
                               scrollX = 350,
                               scrollY = 350,
                               scroller = TRUE,
                               searching = FALSE
                )
      )
  })


}
shinyApp(ui, server)
