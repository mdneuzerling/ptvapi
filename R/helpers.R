#' Convert a datetime returned by the PTV API into Melbourne time
#'
#' @param datetime A datetime returned by the PTV API
#'
#' @return
#' @export
#'
#' @examples
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
