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

#' Convert a datetime returned by the PTV API into Melbourne time
#'
#' @param datetime A datetime returned by the PTV API
#'
#' @return A datetime in the Melbourne timezone.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' convert_to_melbourne_time("2020-05-09T06:38:47.3194196+00:00")
#' }
convert_to_melbourne_time <- function(datetime) {

  if (is.null(datetime)) {
    NA_datetime <- as.POSIXct(NA)
    attr(NA_datetime, "tzone") <- "Australia/Melbourne"
    return(NA_datetime)
  }

  converted <- as.POSIXct(
    datetime,
    tz = "GMT",
    format = "%Y-%m-%dT%H:%M:%OS"
  )
  attr(converted, "tzone") <- "Australia/Melbourne"
  converted
}

#' Convert a POSIXct or character datetime to a format ready for a URL
#'
#' Datetimes accepted by the API need to be given in UTC. This function will
#' accept a datetime or a character with a suitable datetime format, and output
#' a character that is suitable for a URL. All URL input and output in this
#' package should be in Melbourne time, and the UTC conversion should happen
#'
#' The API seems to accept both "%Y-%m-%dT%H:%M:%OS" and "%Y-%m-%d %H:%M:%OS",
#' but we opt for the former.
#'
#' @param datetime POSIXct or Character.
#'
#' @return Character.
#'
#' @keywords internal
#'
to_datetime <- function(datetime) {
  if (is.character(datetime)) {
    datetime <- as.POSIXct(
      datetime,
      tz = "Australia/Melbourne",
      tryFormats = c("%Y-%m-%dT%H:%M:%OS",
                     "%Y-%m-%d %H:%M:%OS",
                     "%Y/%m/%d %H:%M:%OS",
                     "%Y-%m-%d %H:%M",
                     "%Y/%m/%d %H:%M",
                     "%Y-%m-%d",
                     "%Y/%m/%d")
    )
  }

  assertthat::assert_that(
    inherits(datetime, "POSIXct"),
    msg = "datetime must be POSIXct, or character that parses as POSIXct"
  )

  datetime
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
    return(tibble::tibble())
  }
  do.call(rbind, lapply(.x, .f, ...))
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


#' Strictly convert an object to an integer.
#'
#' R does not have a built-in function to determine if a value is an integer
#' (`is.integer()` will check if the class of an object is "integer", but
#' `is.integer(3)` will return FALSE). This helper function fills that gap. It
#' will attempt to convert the input to an integer, but will error on any input
#' that cannot be confidently expressed as an integer. It serves as a stricter
#' version of `as.integer`. For example, the function will convert `3` or `"3"`
#' to integers, but will error on `3.5` or `"three"`.
#'
#' @param x An input of any type.
#'
#' @return An integer interpretation of `x`, if possible.
#'
#' @keywords internal
#'
to_integer <- function(x) {
  if (missing(x)) {
    stop("Error in to_integer() : argument \"x\" is missing, with no default")
  }

  if (!(typeof(x) %in% c("integer", "double", "character"))) {
    stop(paste("Cannot convert", typeof(x), "to an integer"))
  }

  error_message <- paste("Cannot convert", x, "to an integer")

  # Try numeric first, then cast to integer
  x_numeric <- suppressWarnings(as.numeric(x))
  if (is.na(x_numeric)) stop(error_message)
  x_integer <- as.integer(x_numeric)
  if (x_integer != x_numeric) stop(error_message)
  x_integer
}
