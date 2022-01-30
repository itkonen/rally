
#' @export
set_credentials <- function(key, secret) {
  options(
    rally.key = key,
    rally.secret = secret
  )
}

get_access_token <- function(key = getOption("rally.key"),
                             secret = getOption("rally.secret")) {
  if (is.null(key) || is.null(secret)) {
    stop("Credentials not found. Please use `set_credentials()`.", call. = FALSE)
  }
  r <- httr::POST("https://api.danfoss.com/oauth2/token",
                  httr::authenticate(key, secret),
                  httr::content_type("application/x-www-form-urlencoded"),
                  httr::accept("application/json"),
                  body = "grant_type=client_credentials"
                  )
  content <- httr::content(r)
  date <- httr::headers(r)$date |> httr::parse_http_date()
  list(
    token = content$access_token,
    valid_until = date + as.numeric(content$expires_in)
  )
}

token <- function() {
  token <- getOption("rally.access.token")
  if (is.null(token) || token$valid_until < Sys.time()) {
    options(rally.access.token = get_access_token())
    token <- getOption("rally.access.token")
  }
  token$token
}
