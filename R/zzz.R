rally_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  key <- Sys.getenv("RALLY_KEY")
  secret <- Sys.getenv("RALLY_SECRET")
  db_path <- Sys.getenv("RALLY_DB_PATH")
  if(key != "" && secret != "") {
    cat("Found Ally key and secret\n")
    access_token(key, secret)
  }
  if(db_path != "") connect_db(path = db_path)
}

.onUnload <- function(libname) {
  if(!is.null(rally_cache$con)) DBI::dbDisconnect(rally_cache$con)
}
