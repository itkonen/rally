
#' @import httr
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
  g <- function() GET(url, config(token = rally_cache$token))

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

  parse_result <- function(x){
    if(length(x$status)>0) {
      s <-
        x$status %>%
        map(~structure(list(.x$value), .Names = .x$code)) %>%
        flatten() %>%
        as_tibble()
      x$status <- NULL
      bind_cols(as_tibble(x), s)
    } else {
      x$status <- NULL
      as_tibble(x)
    }
  }

  y <- if(is.null(id)) map(x$result, parse_result) else parse_result(x$result)

  y %>%
    bind_rows() %>%
    mutate(
      across(contains("temp"), ~.x/10),
      across(c(active_time, create_time, update_time),
             ~as.POSIXct((.x), origin="1970-01-01", tz = "EET")),
      time = as.POSIXct(x$t/1000, origin = "1970-01-01", tz = "EET")) %>%
    select(id, name, time, temp_current, temp_set, everything()) %>%
    filter(if(is.null(!!name)) TRUE else name == !!name)
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
