library(rally)
library(dplyr)
library(magrittr)
library(shiny)
library(plotly)
library(lubridate)

con <- sqlite_connection()

shiny_ui <- function(request) {
  fluidPage(
    titlePanel("rally"),
    sidebarLayout(
      sidebarPanel(
        uiOutput("date_slider")
      ),
      mainPanel(
        plotlyOutput("temp_plot", height = "500px")
      )
    )
  )
}

shiny_server <- function(input, output, session) {

  data <- reactive({
    read_db(con)
  })

  output$date_slider <- renderUI({
    min_time <- floor_date(min(Sys.time() - days(1), min(data()$time)), unit = "hour")
    max_time <- ceiling_date(Sys.time(), unit = "hour")
    sliderInput("date_slider",
                label = h3("Time Range"),
                min = min_time,
                max = max_time,
                value = max_time - days(1:0),
                timeFormat = "%F %H:%M")
  })

  output$temp_plot <- renderPlotly({
    req(input$date_slider)
    data() %>%
      filter(time <= input$date_slider[2],
             time >= input$date_slider[1]) %>%
      plot_ly(x = ~time, y = ~temp_current,
              color = ~name, text = ~name,
              type = "scatter", mode = "line",
              hovertemplate = '<i>%{text}:</i> %{y:.1f}<extra></extra>'
              ) %>%
      layout(
        hovermode = "compare",
        title = list(text = "Temperature"),
        xaxis = list(title = NA, fixedrange = TRUE),
        yaxis = list(title = NA, fixedrange = TRUE)) %>%
      plotly::config(displayModeBar = FALSE)
  })
}

shiny::shinyApp(ui = ui, server = server)

