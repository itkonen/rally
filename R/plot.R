#' @import dplyr
#' @export
plot_history <- function(con = sqlite_connection()) {
  y <- read_db(con)
  plotly::plot_ly(y, x = ~time, y = ~temp, color = ~name,
                  type = "scatter", mode = "line") %>%
    plotly::layout(hovermode = "x unified",
                   title = list(text = "Room Temperature"),
                   xaxis = list(title = NA),
                   yaxis = list(title = NA, fixedrange = TRUE)) %>%
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
}

