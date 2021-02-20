library(rally)
library(tidyverse)
library(shiny)
library(plotly)
library(lubridate)

ui <- fluidPage(
  titlePanel("rally"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date_slider", label = h3("Time Range"),
                  min = Sys.time() - days(2),
                  max = ceiling_date(Sys.time(), unit = "hour"),
                  value = c(Sys.time() - days(1),
                            ceiling_date(Sys.time(), unit = "hour")),
                  timeFormat = "%F %H:%M")
    ),
    mainPanel(
      plotlyOutput("temp_plot", height = "500px")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    d <- read_db()
    min_time <- floor_date(min(Sys.time() - days(1), min(d$time)), unit = "hour")
    max_time <- ceiling_date(Sys.time(), unit = "hour")
    updateSliderInput(session, "date_slider",
                      min = min_time, max = max_time,
                      value = c(Sys.time() - days(1), max_time),
                      timeFormat = "%F %H:%M")
    d
  })

  output$temp_plot <- renderPlotly({
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

