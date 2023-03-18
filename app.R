library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(bslib)
library(leaflet)
library(thematic)
library(shinycssloaders)

thematic::thematic_shiny()

custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#FF1D8E",
  primary = "#FF1D8E",
  secondary = "#FF374B",
  heading_font = font_google("Lobster"),
  base_font = font_google("Montserrat")
)

rupaulIcon <- makeIcon(
  iconUrl = "rupaul.png",
  iconWidth = 60, iconHeight = 70,
  iconAnchorX = 22, iconAnchorY = 70
)

# read in data
drag_df <- read.csv("data/drag.csv")
ui <- fluidPage(
  theme = custom_theme,
  titlePanel(title = div(img(src ="logo.png", height = 100), 'Drag Race Visualizer', style = "font-size:55px;")),
  sidebarLayout(
    # sidebar (filters)
    sidebarPanel(
      width = 2,
      h3('Filters'),
      selectizeInput(inputId = "season", label = "Season",
                  choices = unique(sort(drag_df$season)),
                  multiple = TRUE,
                  options = list(plugins = list('remove_button'))),
      selectizeInput(
        inputId = 'queens',
        label = "Queens",
        choices = sort(unique(drag_df$contestant)),
        selected = c('Jinkx Monsoon'),
        multiple = TRUE,
        options = list(plugins = list('remove_button'))
      ),
      # Other Categories filter
      checkboxGroupInput(inputId = "other_categories", label = "Other Categories",
                         choices = c(
                           Finalist = 'finalist', 
                           `Miss Congeniality` = 'missc', 
                           `Winner` = 'winner', 
                           `First Eliminated` = 'first_eliminated'
                          ),
                         selected = NULL),

      # Age slider filter
      sliderInput(inputId = "age", label = "Age",
                  min = min(drag_df$age, na.rm = TRUE),
                  max = max(drag_df$age, na.rm = TRUE),
                  value = c(min(drag_df$age, na.rm = TRUE), max(drag_df$age, na.rm = TRUE)),
                  step = 1),
      
      # Reset button
      actionButton(inputId = "reset", label = "Reset Filters")
    ),
    # main body (graphs)
    mainPanel(
      width = 10,
      fluidRow(
        column(6,
               h3("Hometown Map"),
               'Click on markers for more information on the queens',
               withSpinner(leafletOutput("hometown"),
                           color = "#FF1D8E")
               ),
        column(6,
               h3("Queen Performance"),
               'Queen performance over the episodes (If number of queens > 10, top 10 displayed)',
               withSpinner(plotlyOutput("queen_challenge"),
                           color = "#FF1D8E")
        ),
      fluidRow(
        column(6,
          h3('Relative Rankings'),
          'Ranking of the queen and how many challenges they participated in.',
          withSpinner(dataTableOutput('ranking'),
                      color = "#FF1D8E")
        ),
        # Outcome tally table
        column(width=6,
               h3('Outcome Tallies'),
               'Total counts of each outcome over the season.',
               withSpinner(DT::DTOutput(outputId = 'outcome_table'),
                           color = "#FF1D8E")
        )
      )
    )
  )
))


server <- function(input, output, session) {
  # reactively changes selectable queens based on chosen season
  observe({
    # filter data based on chosen season and get unique names
    if (!is.null(input$season)) {
      seasoned <- drag_df |>
        dplyr::filter(season %in% input$season)
        
      filtered_names <- seasoned |> 
        dplyr::select(contestant) |>
        unique() |> 
        dplyr::arrange(contestant)
      
      selected_names <- seasoned |> 
        dplyr::select(contestant, rank) |> 
        unique() |> 
        dplyr::arrange(rank) |> 
        dplyr::select(contestant) |> 
        dplyr::slice(1:2) |> 
        dplyr::pull()
    } else {
      filtered_names <- sort(unique(drag_df$contestant))
      selected_names <- 'Jinkx Monsoon'
    }
      
    updateSelectizeInput(
      inputId = 'queens',
      choices = filtered_names,
      selected = selected_names
    )
  }) |> bindEvent(input$season, ignoreNULL = FALSE)

  # reactive expression to filter data based on user selections
  filtered_data <- reactive({
    drag_filtered <- drag_df
    # optional season filter
    if (!is.null(input$season)) {
      drag_filtered <- drag_filtered |> 
        dplyr::filter(season %in% input$season)
    }
    
    # optional categories (union of groups instead of intersection)
    if (!is.null(input$other_categories)) {
      drag_filtered <- drag_filtered |> 
        filter(if_any(input$other_categories, function(x) x == 1))
    }
    
    # age filter always applies
    drag_filtered <- drag_filtered |> 
      dplyr::filter(age >= input$age[1] & age <= input$age[2])
    
    # update the age slider with the min and max values of the filtered data
    updateSliderInput(session = session, 
                      inputId = "age",
                      min = min(drag_df$age, na.rm = TRUE),
                      max = max(drag_df$age, na.rm = TRUE),
                      value = c(min(drag_filtered$age, na.rm = TRUE), max(drag_filtered$age, na.rm = TRUE)))
    
    # name filter separate
    if (!is.null(input$queens)) {
      drag_filtered <- drag_df |>
        dplyr::filter(contestant %in% input$queens)
    }
    drag_filtered
  })

  # return filters to default with reset button
  observeEvent(input$reset, {
    updateSelectizeInput(session, "queens", selected = 'Jinkx Monsoon')
    updateSelectInput(session, "season", selected = NA)
    updateSliderInput(session, "age", min=21, max=52, value = c(21, 52))
    updateCheckboxGroupInput(session, "other_categories", selected = NA)
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
    map_blank <- leaflet(data = filtered_data() |>
                           distinct(lng, lat, contestant, .keep_all = TRUE)) |>
      leaflet::setView(lng = -95.7129, lat = 37.0902, zoom = 3) |>
      addTiles() |>
      addMarkers(
        ~lng,
        ~lat,
        icon = rupaulIcon,
        popup = ~paste(contestant,
                       "<br>Hometown:", city, ",", state,
                       "<br>Age on Season:", age),
        label = ~as.character(contestant),
        clusterOptions = markerClusterOptions())
    } else {
      map_blank <- leaflet() |>
        addTiles()
      }
    map_blank

})

  output$queen_challenge <- renderPlotly({
  
    # Filter the data to only include the top performers
    plot_data <- filtered_data()
      
    plot_data %>% 
      dplyr::group_by(contestant, season, episode) %>%
      dplyr::summarise(outcome = if_else(outcome == "ELIM", "ELIMINATED", outcome)) %>%
      dplyr::arrange(season) %>%
      dplyr::top_n(10, contestant) %>%
      plot_ly(x = ~episode,
            y = ~factor(outcome, levels= c("BTM", "LOW", "SAFE", "HIGH", "WIN")),
            color = ~contestant,
            type = "scatter",
            mode = "lines+markers") %>%
      layout(xaxis = list(title = ""),
             yaxis = list(title = "",
                          tickfont=list(size=10, 
                                    family=font_google("Signika Negative"),
                                    color="#FF1D8E")),
             legend = list(orientation = "h",
                           xanchor = "center",
                           x = 0.5,
                           y = -0.1,
                           itemsizing = "constant",
                           itemwidth = 40,
                           itemclick = "toggle",
                           itemdoubleclick = "toggleothers",
                           font=list(size=8, 
                                     family=font_google("Signika Negative"))))
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
                       BOTTOM = sum(outcome == "BTM", na.rm = TRUE),
                       .groups = 'drop') |>
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
