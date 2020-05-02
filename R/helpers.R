#' Convert a datetime returned by the PTV API into Melbourne time
#'
#' @param datetime A datetime returned by the PTV API
#'
#' @return A datetime in the Melbourne timezone.
#'
#' @keywords internal
#'
convert_to_melbourne_time <- function(datetime) {

  if (is.null(datetime)) {
    return(lubridate::as_datetime(NA))
  }

  lubridate::ymd_hms(
    datetime,
    tz = "Australia/Melbourne",
    quiet = TRUE
  )
}

#' Map and rbind a list of data frames
#'
#' This function is a simple combination of `purrr::map()` and `purrr::reduce()`
#' using `rbind`. This differs from `purrr::map_dfr()`, which uses
#' `dplyr::map_dfr()` and therefore introduces `dplyr` as a dependency. If the
#' provided list is empty, then an empty tibble will be returned.
#'
#' @param .x A list of data frames or tibbles.
#' @param .f A function.
#' @param ... Additional arguments passed to the function.
#'
#' @return A data frame or tibble.
#'
#' @keywords internal
#'
map_and_rbind <- function(.x, .f, ...) {
  if (length(.x) == 0) {
    return(tibble())
  }
  purrr::reduce(purrr::map(.x, .f, ...), rbind)
}

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
