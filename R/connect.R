#' Authorize app to use Danfoss Ally API
#'
#' To obtain the required key and secret, register at https://developer.danfoss.com/. A
#'
#' @param key, secret character strings
#'
#' @import httr
#' @export
authorize <- function(key, secret) {
  app <- oauth_app("danfoss", key, secret)
  ep <- oauth_endpoint(authorize = NULL,
                       access= "oauth2/token",
                       base_url = "https://api.danfoss.com")
  rally_cache$token <- oauth2.0_token(ep, app, client_credentials = TRUE,
                                      use_basic_auth = TRUE, cache = TRUE)
}

#' @import httr
request <- function(id = NULL) {
  if(is.null(rally_cache$token)) {
    stop("No access token found. Use `authorize(key, secret)` to set up the access.")
  }
  url <- modify_url("https://api.danfoss.com", path = c("ally/devices", id))
  g <- function() GET(url, config(token = rally_cache$token),
                      user_agent("https://github.com/itkonen/rally/"))

  tryCatch({
    stop_for_status(g(), task = "get devices")
  }, error = function(cnd) {
    rally_cache$token$init_credentials()
    g()
  })
}

#' @import purrr dplyr
#' @importFrom magrittr `%>%`
#' @export
get_devices <- function(id = NULL, name = NULL) {
  x <- httr::content(request(id))

  parse_result <- function(x) {
    x$status <-
      setNames(map(x$status, "value"), map_chr(x$status, "code"))
    x 
  }

  if(is.null(id)) {
    y <- map(x$result, parse_result)
  } else {
    y <- list(parse_result(x$result))
  }

  devices <-
    map(y, ~list_modify(.x, status=NULL)) %>%
    bind_rows() %>%
    mutate(
      across(c(active_time, create_time, update_time),
             ~as.POSIXct((.x), origin="1970-01-01", tz = "EET"))
    ) %>%
    select(id, name, everything())

  status <-
    map(y, "status") %>%
    setNames(devices$id) %>%
    bind_rows(.id = "id") %>%
    mutate(across(contains("temp"), ~.x/10)) %>%
    select(id, temp_current, mode, battery_percentage, window_state, lower_temp, upper_temp, everything())

  time <- as.POSIXct(x$t/1000, origin = "1970-01-01", tz = "EET")

  list(devices = devices, status = status, time = time)
}



#' @import httr
#' @export
temp_set <- function(id, temperature) {
  value <- as.integer(round(temp*10))
  set_device(id, "temp_set", value)
}

#' @export
set_device <- function(id, code, value) {
  b <- jsonlite::toJSON(list(commands=list(list(code = code, value = value))),
                        auto_unbox = TRUE)
  y <- httr::POST(
               modify_url("https://api.danfoss.com",
                          path = c("ally/devices", id, "commands")),
               config(token = rally_cache$token),
               add_headers(`Content-Type` = "application/json"),
               body = as.character(b))
  httr::stop_for_status(y)
  TRUE
}
