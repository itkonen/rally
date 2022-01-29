#' @import dplyr
#' @export
plot_history <- function(con = sqlite_connection()) {
  y <- read_db(con)
  plotly::plot_ly(y, x = ~time, y = ~temp_current, color = ~name,
                  type = "scatter", mode = "line") %>%
    plotly::layout(hovermode = "x unified",
                   title = list(text = "Room Temperature"),
                   xaxis = list(title = NA),
                   yaxis = list(title = NA, fixedrange = TRUE)) %>%
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
}


shiny_ui <- function(request) {
  fluidPage(
    titlePanel("rally"),
    sidebarLayout(
      sidebarPanel(
        checkboxInput(
          "daily",
          "Daily average"
        ),
        checkboxInput(
          "mean_devices",
          "Average of device"
        ),
        sliderInput(
          "date_slider",
          label = "Time Range",
          value = 1:2,
          min = 0,
          max = 2
        ),
        actionButton(
          "refresh",
          "Refresh"
        )
      ),
      mainPanel(
        plotly::plotlyOutput("temp_plot", height = "500px")
      )
    )
  )
}

shiny_server <- function(input, output, session) {

  con <- sqlite_connection()

  data <- reactive({
    input$refresh
    y <- read_db(con)

    max_time <- max(y$time)
    min_time <- min(y$time)

    updateSliderInput(
      session,
      "date_slider",
      min = min_time,
      max = max_time,
      value = max_time - lubridate::days(1:0),
      timeFormat = "%F %R"
    )

    y
  })

  data2 <- reactive({
    d <- data()
    if (input$daily) {
      d <- d %>%
        mutate(time = as.Date(time)) %>%
        group_by(name, time) %>%
        summarise(temp_current = mean(temp_current, na.rm = TRUE),
                  .groups = "drop")
    }
    if (input$mean_devices) {
      d <- d %>%
        group_by(time) %>%
        summarise(temp_current = mean(temp_current, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(name = "Device average")
    }
    d
  })

  output$temp_plot <- plotly::renderPlotly({
    req(data2(), cancelOutput = TRUE)
    data2() %>%
      filter(time <= input$date_slider[2],
             time >= input$date_slider[1]) %>%
      plotly::plot_ly(x = ~time, y = ~temp_current,
                      color = ~name, text = ~name,
                      type = "scatter", mode = "line",
                      hovertemplate = '<i>%{text}:</i> %{y:.1f}<extra></extra>'
                      ) %>%
      plotly::layout(
        hovermode = "compare",
        title = list(text = "Temperature"),
        xaxis = list(title = NA, fixedrange = TRUE),
        yaxis = list(title = NA, fixedrange = TRUE)) %>%
      plotly::config(displayModeBar = FALSE)
  })
}


#' @export
rally_app <- function(path = getOption("rally.db.path")) {

  options(rally.db.path = normalizePath(path))

  shiny::shinyApp(shiny_ui, shiny_server)
}
