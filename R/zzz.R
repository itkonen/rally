## rally_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  key <- Sys.getenv("RALLY_KEY")
  secret <- Sys.getenv("RALLY_SECRET")
  if(nzchar(key) && nzchar(secret)) {
    cat("Found Ally key and secret\n")
    set_credentials(key, secret)
  }
  if(nzchar(Sys.getenv("RALLY_DB_PATH"))) {
    options(rally.db.path = Sys.getenv("RALLY_DB_PATH"))
  } else {
    options(rally.db.path = "rally_db.sqlite")
  }
}

## .onUnload <- function(libname) {
##   if(!is.null(rally_cache$con)) DBI::dbDisconnect(rally_cache$con)
## }
