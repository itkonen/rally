
connect_db <- function(path = "danfoss-db.sqlite") {
  rally_cache$con <- DBI::dbConnect(RSQLite::SQLite(), path)
}

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
collector <- function(sample_interval = 300, path = "danfoss-db.sqlite") {
  connect_db(path)
  repeat({
    tic <- Sys.time()
    append_db(get_devices())
    toc <- Sys.time()
    wait <- max(sample_interval - difftime(toc, tic, unit = "secs"), 5)
    Sys.sleep(wait)
  })
}

#' @export
collector_deamon <- function(key = NULL, secret = NULL, sample_interval = 300,
                             path = "danfoss-db.sqlite") {
  if(!is.null(key) && !is.null(secret)) access_token(key, secret)
  collector(sample_interval, path)
}
