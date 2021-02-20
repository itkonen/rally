library(rally)
library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)

ui <- fluidPage(
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

server <- function(input, output, session) {

  con <- sqlite_connection()

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
      plot_ly(x = ~time, y = ~temp_current, color = ~name,
              type = "scatter", mode = "line") %>%
      layout(hovermode = "x unified",
             title = list(text = "Temperature"),
             xaxis = list(title = NA, fixedrange = TRUE),
             yaxis = list(title = NA, fixedrange = TRUE)) %>%
      plotly::config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)

