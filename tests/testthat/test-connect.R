skip_if_no_auth <- function() {
  if (identical(Sys.getenv("RALLY_SECRET"), "")) {
    skip("No authentication available")
  }
}
test_that("Connection works", {
  skip_if_no_auth()
  expect_s3_class(authorize(Sys.getenv("RALLY_KEY"), Sys.getenv("RALLY_SECRET")), "Token2.0")
  x <- get_devices()
  expect_s3_class(x, "tbl_df")
  expect_s3_class(get_devices(x$id[1]), "tbl_df")
})
