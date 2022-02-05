#' @import DBI
#' @import RSQLite
#' @export
record <- function(con = sqlite_connection()) {

  rally_data <- get_devices()

  devices <-
    rally_data$devices |>
    select(-status)

  if (!"devices" %in% dbListTables(con)) {
    on.exit({
      dbExecute(con, "CREATE INDEX index_id ON devices (id)")
    }, add = TRUE)
  }
  dbWriteTable(con, "devices", devices, overwrite = TRUE)

  rally_data$devices |>
    filter(sub, online) |>
    select(id, status, device_type) |>
    group_by(device_type) |>
    group_walk(~{
      d <-
        tidyr::unnest_wider(.x, status) |>
        mutate(time = rally_data$time) |>
        relocate(id, time)
      name <- switch(.y$device_type,
                     "Danfoss Ally™ Room Sensor" = "room_sensor",
                     "Danfoss Ally™ Radiator Thermostat" = "radiator_thermostat")
      if (!name %in% dbListTables(con)) {
        on.exit({
          dbExecute(con, glue::glue("CREATE INDEX index_{name}_id ON {name} (id)"))
          dbExecute(con, glue::glue("CREATE INDEX index_{name}_time ON {name} (time)"))
        }, add = TRUE)
      }
      dbWriteTable(con, name, d, append = TRUE)
    })
  invisible(NULL)
}

#' @export
start_recorder <- function(path = getOption("rally.db.path"), sample_interval = 300) {
  con <- dbConnect(duckdb::duckdb(), dbdir = path)
  timestamp()
  repeat({
    tic <- Sys.time()
    try(record(con))
    toc <- Sys.time()
    wait <- max(sample_interval - as.numeric(difftime(toc, tic, unit = "secs")), 5)
    Sys.sleep(wait)
  })
}


## #' @export
## start_recorder_daemon <- function(key, secret,
##                                   con = sqlite_connection("danfoss-db.sqlite"),
##                                   sample_interval = 300) {
##   set_credentials(key, secret)
##   recorder(con, sample_interval)
## }
