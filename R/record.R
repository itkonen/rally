#' @export
record <- function(con) {

  d <- get_devices()

  devices <-
    d$devices |>
    select(-status)

  dbWriteTable(con, "devices", devices, overwrite = TRUE)

  status <-
    d$devices |>
    filter(device_type %in% c("Danfoss Ally™ Room Sensor",
                              "Danfoss Ally™ Radiator Thermostat")) |>
    select(id, status, device_type) |>
    mutate(time = d$time)

  status |>
    group_by(device_type) |>
    group_map(~{
      x <- tidyr::unnest(.x, status)
      dbWriteTable(con, .y$device_type, x, append = TRUE)
    })
  invisible(NULL)
}

#' @export
start_recorder <- function(path = "rally_db.duckdb", sample_interval = 300) {
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
