#' @export
sqlite_connection <- function(path = getOption("rally.db.path")) {
  DBI::dbConnect(RSQLite::SQLite(), path, extended_types = TRUE)
}


#' @import dplyr
#' @export
read_db <- function(con = sqlite_connection(),
                    device = c("radiator_thermostat", "room_sensor")) {
  left_join(tbl(con, match.arg(device)),
            tbl(con, "devices"), by = "id") |>
    ## mutate(across(any_of(
    ##   c("switch", "child_lock", "factory_reset", "fault", "online", "sub")),
    ##   as.logical)) |>
    relocate(id, name, time)
}
