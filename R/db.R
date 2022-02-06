#' @export
sqlite_connection <- function(path = getOption("rally.db.path"), read_only = FALSE) {
  DBI::dbConnect(
    RSQLite::SQLite(), path, extended_types = TRUE,
    flags = if (read_only) SQLITE_RO else SQLITE_RWC
  )
}


#' @import dplyr
#' @export
read_db <- function(con = sqlite_connection(read_only = TRUE),
                    device = c("radiator_thermostat", "room_sensor")) {
  left_join(tbl(con, match.arg(device)),
            tbl(con, "devices"), by = "id") |>
    relocate(id, name, time)
}
