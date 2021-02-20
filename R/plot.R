#' @import dplyr
#' @export
plot_history <- function() {
  y <-
    tbl(rally_cache$con, "danfoss") %>%
    collect() %>%
    mutate(across(c(time, active_time, create_time, update_time),
                  ~as.POSIXct(.x, origin = "1970-01-01", tz = "EET")))
  plotly::plot_ly(y, x = ~time, y = ~temp_current, color = ~name,
                  type = "scatter", mode = "line")
}

#' @export
shiny_app <- function(path = "danfoss-db.sqlite") {
  Sys.setenv(RALLY_DB_PATH = normalizePath(path))
  app <- system.file("shiny", "app.R", package = "rally")
  if (app == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(app, display.mode = "normal")
}
