#' Assert that the API has returned the expected attributes
#'
#' The attributes returned by the API calls are usually converted to column
#' names in a tibble. This function compares that tibble against a vector of
#' expected column names, and returns an error if the two do not match.
#' Unfortunately, there is no easy fix for this error: the package developer(s)
#' must be notified, so that they can align the functions against the API
#' schema.
#'
#' @param data A tibble parsed from the content returned by a PTV API call.
#' @param expected_colnames A character vector of expected column names, in
#'   order.
#'
#' @return An error if the column names are not as expected.
#'
#' @keywords internal
#'
assert_correct_colnames <- function(data, expected_colnames) {
  assertthat::assert_that(
    identical(colnames(data), expected_colnames),
    msg = paste(
      "The attributes returned by the API were not as expected.",
      "Please contact the package developer.",
      "Expected", paste(expected_colnames, collapse = ", "),
      "but got", paste(colnames(data), collapse = ", ")
    )
  )
}
