shiny_ui <- function(request) {
  fluidPage(
    titlePanel("rally"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "aggregate", label = h3("Aggregate"),
          choices = list("Daily", "Hourly", "Raw"),
          selected = "Daily"
        ),
        radioButtons(
          "filter", label = h3("Filter"),
          choices = list("Last day" = 1, "Last week" = 7,
                         "Last month" = 31, "All time" = 0),
          selected = 1
        ),
        checkboxInput(
          "mean_devices",
          "Average of device"
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
    d <-
      read_db(con) |>
      select(name, time, temp = temp_current)
    f <- as.numeric(input$filter)
    if (f) {
      s <- as.integer(Sys.time()) - f * 86400L
      d <- filter(d, time > !!s)
    }
    d
  })

  daily_average <- reactive({
    data() |>
      mutate(time = datetime(time, "unixepoch", "localtime", "start of day")) |>
      group_by(name, time) |>
      summarize(temp = mean(temp, na.rm = TRUE), .groups = "drop") |>
      collect() |>
      mutate(time = as.Date(time))
  })

  hourly_average <- reactive({
    data() |>
      mutate(time = strftime("%Y-%m-%d %H:00:00", time,
                             "unixepoch", "localtime")) |>
      group_by(name, time) |>
      summarize(temp = mean(temp, na.rm = TRUE), .groups = "drop") |>
      collect() |>
      mutate(time = as.POSIXct(time))
  })

  raw_data <- reactive({
    data() |>
      mutate(time = datetime(time, "unixepoch", 'localtime')) |>
      collect() |>
      mutate(time = as.POSIXct(time))
  })

  output$temp_plot <- plotly::renderPlotly({
    d <- switch(input$aggregate,
                Daily = daily_average(),
                Hourly = hourly_average(),
                Raw = raw_data())

    if (input$mean_devices) {
      d <-
        d |> group_by(time) |>
        summarise(temp = mean(temp)) |>
        mutate(name = "Average of devices")
    }

    d |>
      plotly::plot_ly(x = ~time, y = ~temp,
                      color = ~name, text = ~name,
                      type = "scatter", mode = "line",
                      hovertemplate = '<i>%{text}:</i> %{y:.1f}<extra></extra>'
                      ) |>
      plotly::layout(
        hovermode = "x",
        title = list(text = "Temperature"),
        xaxis = list(title = NA, fixedrange = TRUE),
        yaxis = list(title = NA, fixedrange = TRUE)) |>
      plotly::config(displayModeBar = FALSE)
  })
}


#' @export
rally_app <- function(path = getOption("rally.db.path")) {

  options(rally.db.path = normalizePath(path))

  shiny::shinyApp(shiny_ui, shiny_server)
}

