#' Set target temperature of a device
#'
#' @import httr
#' @export
temp_set <- function(id, temperature) {
  value <- as.integer(round(temp*10))
  set_device(id, "temp_set", value)
}

#' Send commands to devices
#'
#' @export
set_device <- function(id, code, value) {
  b <- jsonlite::toJSON(list(commands=list(list(code = code, value = value))),
                        auto_unbox = TRUE)
  y <- httr::POST(
    "https://api.danfoss.com",
    path = c("ally/devices", id, "commands"),
    httr::add_headers(Authorization = paste("Bearer", token())),
    httr::user_agent("https://github.com/itkonen/rally/"),
    add_headers(`Content-Type` = "application/json"),
    body = as.character(b))
  httr::stop_for_status(y)
  TRUE
}
