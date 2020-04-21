#' Assert that the API has returned the expected attributes
#'
#' The attributes returned by the API calls should be follow the API schema.
#' This function compares received attributes against a vector of expected
#' attributes, and returns an error if the two do not match. Unfortunately,
#' there is no easy fix for this error: the package developer(s) must be
#' notified, so that they can align the functions against the API schema.
#'
#' @param received_attributes A character vector of attributes, in order.
#' @param expected_colnames A character vector of expected attributes, in
#'   order.
#'
#' @return An error if the column names are not as expected.
#'
#' @keywords internal
#'
assert_correct_attributes <- function(received_attributes,
                                      expected_attributes) {
  assertthat::assert_that(
    identical(received_attributes, expected_attributes),
    msg = paste(
      "The attributes returned by the API were not as expected.",
      "Please contact the package developer.",
      "Expected", paste(expected_attributes, collapse = ", "),
      "but got", paste(received_attributes, collapse = ", ")
    )
  )
}
