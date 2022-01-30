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
