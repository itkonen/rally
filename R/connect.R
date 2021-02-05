
#' @import httr
access_token <- function(key, secret) {
  app <- oauth_app("danfoss", key, secret)
  ep <- oauth_endpoint(authorize = NULL,
                       access= "oauth2/token",
                       base_url = "https://api.danfoss.com")
  assign("token",
         oauth2.0_token(ep, app, client_credentials = TRUE,
                        use_basic_auth = TRUE, cache = TRUE),
         envir = rally_cache
  )
}

#' @import httr
request <- function() {
  stopifnot(!is.null(rally_cache$token))
  g <- function() GET("https://api.danfoss.com/ally/devices",
                      config(token = rally_cache$token))
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
get_devices <- function() {
  x <- request() %>% httr::content()
  x$result %>%
    map(function(x){
      s <- x$status %>%
        map(~structure(list(.x$value), .Names = .x$code)) %>%
        flatten() %>%
        as_tibble()
      x$status <- NULL
      bind_cols(as_tibble(x), s)
    }) %>%
    bind_rows() %>%
    mutate(
      across(contains("temp"), ~.x/10),
      across(c(active_time, create_time, update_time),
             ~as.POSIXct((.x), origin="1970-01-01", tz = "EET")),
      time = as.POSIXct(x$t/1000, origin = "1970-01-01", tz = "EET")) %>%
    select(id, name, time, temp_current, temp_set, everything())
}
