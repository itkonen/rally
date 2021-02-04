rally_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  key <- Sys.getenv("RALLY_KEY")
  secret <- Sys.getenv("RALLY_SECRET")
  options(rally.key = key)
  options(rally.secret = secret)
  if(key != "" && secret != "") {
    authenticate(key, secret)
  }
}
