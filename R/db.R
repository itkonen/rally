
#' @export
append_db <- function(y) {
  DBI::dbWriteTable(rally_cache$con, "danfoss", y, append = TRUE)
}

#' @import dplyr
#' @export
read_db <- function() {
  tbl(rally_cache$con, "danfoss") %>%
    collect() %>%
    mutate(across(c(time, active_time, create_time, update_time),
                  ~as.POSIXct(.x, origin = "1970-01-01", tz = "EET")))
}

#' @export
collector_deamon <- function() {
  repeat({
    y <- get_devices()
    append_db(y)
    Sys.sleep(5*60)
  })
}

#' @export
start_collecting <- function(key, secret) {
  access_token(key, secret)
  collector_deamon()
}
