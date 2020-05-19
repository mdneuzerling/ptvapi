new_ptvapi_tibble <- function(response, parsed) {
  assertthat::assert_that(is.data.frame(parsed))

  ptvapi_tibble <- tibble::new_tibble(
    parsed,
    nrow = nrow(parsed),
    class = "ptvapi",
    request = response$request,
    retrieved = response$retrieved,
    status_code = response$status_code,
    content = response$content
  )

  tibble::validate_tibble(ptvapi_tibble)
  ptvapi_tibble
}

# This print function hides the custom attributes that we attach the objects
# returned by the api functions. In particular, the "content" attribute contains
# the raw content returned by the API, and is very ugly to print. The request
# and status code are printed at the top, and everything else is printed as
# normal.
#' @export
print.ptvapi <- function(x, ...) {
  if (!is.null(attr(x, "request"))) {
    cat("Request:", attr(x, "request"), "\n")
  }
  if (!is.null(attr(x, "retrieved"))) {
    cat("Retrieved:", attr(x, "retrieved"), "\n")
  }
  if (!is.null(attr(x, "status_code"))) {
    cat("Status code:", attr(x, "status_code"), "\n")
  }
  NextMethod()
}
