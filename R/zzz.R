# nocov start
.onLoad <- function(...) {
  register_all_s3_methods()
  invisible()
}
# nocov end
