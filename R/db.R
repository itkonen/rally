#' @export
sqlite_connection <- function(path = getOption("rally.db.path")) {
  DBI::dbConnect(RSQLite::SQLite(), path)
}

#' @export
append_db <- function(con = sqlite_connection(), rally_data = get_data()) {
  DBI::dbWriteTable(con, "rally_devices",
                    bind_cols(tibble(time = rally_data$time), rally_data$devices),
                    overwrite = TRUE)
  DBI::dbWriteTable(con, "rally_status",
                    bind_cols(tibble(time = rally_data$time), rally_data$status),
                    append = TRUE)
}

#' @import dplyr
#' @export
read_db <- function(con = sqlite_connection()) {
  r <-
    full_join(tbl(con, "rally_status"),
              tbl(con, "rally_devices"), by = "id") %>%
    mutate(time = coalesce(time.x, time.y)) %>%
    select(id, name, time, everything(), -time.x, -time.y)

  ## Workaround: The SQLite connection cannot handle datetime, so the data needs to be collected for the function transformation.
  if("tbl_SQLiteConnection" %in% class(r)) {
    r <-
      collect(r) %>%
      mutate(across(c(time, active_time, create_time, update_time),
                    ~as.POSIXct(.x, origin = "1970-01-01", tz = "EET")))
  }
  r
}

#' @export
recorder <- function(con = sqlite_connection(), sample_interval = 300) {
  ## con  <-  sqlite_connection(path)
  repeat({
    tic <- Sys.time()
    append_db(con)
    toc <- Sys.time()
    wait <- max(sample_interval - as.numeric(difftime(toc, tic, unit = "secs")), 5)
    Sys.sleep(wait)
  })
}

#' @export
start_recorder_daemon <- function(key, secret, path = "danfoss-db.sqlite",
                                  sample_interval = 300) {
  access_token(key, secret)
  recorder(sqlite_connection(path), sample_interval)
}
