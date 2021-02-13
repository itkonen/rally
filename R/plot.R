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
