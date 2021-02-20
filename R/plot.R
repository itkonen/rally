#' @import dplyr
#' @export
plot_history <- function(con = sqlite_connection()) {
  y <- read_db(con)
  plotly::plot_ly(y, x = ~time, y = ~temp_current, color = ~name,
                  type = "scatter", mode = "line") %>%
    plotly::layout(hovermode = "x unified",
                   title = list(text = "Temperature"),
                   xaxis = list(title = NA),
                   yaxis = list(title = NA, fixedrange = TRUE)) %>%
    plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))
}

#' @export
shiny_app <- function(path = getOption("rally.db.path")) {
  options(rally.db.path = normalizePath(path))

  app <- system.file("shiny", "app.R", package = "rally")
  if (app == "") {
    stop("Could not find example directory. Try re-installing `rally`.", call. = FALSE)
  }

  shiny::runApp(app, display.mode = "normal")
}
