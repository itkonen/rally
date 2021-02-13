rally_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  key <- Sys.getenv("RALLY_KEY")
  secret <- Sys.getenv("RALLY_SECRET")
  path <- Sys.getenv("RALLY_PATH")
  if(key != "" && secret != "") {
    cat("Found Ally key and secret\n")
    access_token(key, secret)
  }
  if(path != "") connect_db(path)
}

.onUnload <- function(libname) {
  DBI::dbDisconnect(rally_cache$con)
}
