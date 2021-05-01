#' Convert a single geopath to a tibble
#'
#' This function is designed to parse the `geopath` content returned by the
#' interior steps of some functions. If geopath data is requested, that content
#' will contain a list of `geopaths` for each route. This function is designed
#' to parse _one_ of those geopaths into a tibble.
#'
#' @param route A `geopaths` object, as a list
#'
#' @return A tibble of routes, with the following columns:
#' \itemize{
#'   \item `direction_id`
#'   \item `valid_from`
#'   \item `valid_to`
#'   \item `paths`
#' }
#'
#' @keywords internal
#'
geopath_to_tibble <- function(geopath) {
  if (is.null(geopath)) {
    return(
      tibble::tibble(
        direction_id = integer(),
        valid_from = as.Date(NULL),
        valid_to = as.Date(NULL),
        paths = list()
      )
    )
  }
  tibble::tibble(
    direction_id = geopath$direction_id,
    valid_from = as.Date(geopath$valid_from),
    valid_to = as.Date(geopath$valid_to),
    paths = list(geopath$paths)
  )
}
