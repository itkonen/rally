rally_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  key <- Sys.getenv("RALLY_KEY")
  secret <- Sys.getenv("RALLY_SECRET")
  options(rally.key = key)
  options(rally.secret = secret)
  if(key != "" && secret != "") {
    cat("Found rAlly key and secret\n")
    access_token(key, secret)
  }
  rally_cache$con <- DBI::dbConnect(RSQLite::SQLite(), "danfoss-db.sqlite")
}

.onUnload <- function(libname) {
  DBI::dbDisconnect(rally_cache$con)
}
