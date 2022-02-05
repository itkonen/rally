#' Get device information and status from Danfoss Ally API
#'
#' @param id a string
#'
#' @return a list
#'
#' @import purrr dplyr
#' @export
get_devices <- function(id = NULL) {
  r <-
    httr::GET(
      "https://api.danfoss.com",
      path = c("ally/devices", id),
      httr::add_headers(Authorization = paste("Bearer", token())),
      httr::user_agent("https://github.com/itkonen/rally/"),
      httr::accept("application/json")
    )

  httr::stop_for_status(r, task = "get devices")

  x <- httr::content(r)

  ## To use same parser for individual devices
  if(!is.null(id)) x$result <- list(x$result)

  devices <-
    map_dfr(x$result, parse_result) |>
    mutate(
      across(c(active_time, create_time, update_time),
             ~as.POSIXct(.x, origin = "1970-01-01"), tz = "UTC")
    ) |>
    relocate(id, name)

  time <- as.POSIXct(x$t/1000, origin = "1970-01-01", tz = "UTC")

  l <- list(devices = devices,
            time = time)
  class(l) <- "rally_data"
  l
}

parse_result <- function(x) {
  x$status <-
    map(x$status, "value") |>
    setNames(map_chr(x$status, "code")) |>
    imap(~{
      if (grepl("(temp|humidity)", .y)) .x/10 else .x
    }) |>
    list()
  x
}

