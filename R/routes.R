#' Retrieve a data frame of all routes
#'
#' @details
#' All timestamps returned by this function are in Melbourne time.
#'
#' @inheritParams PTVGET
#'
#' @return A data frame of routes and information, with the following columns:
#' \itemize{
#'   \item route_id
#'   \item route_gtfs_id
#'   \item route_name
#'   \item route_type
#'   \item route_number
#'   \item service_status
#'   \item service_status_timestamp
#' }
#' @export
#'
#' @examples \dontrun{routes()}
routes <- function(user_id = determine_user_id(),
                   api_key = determine_api_key()) {
  response <- PTVGET(
    request = "routes",
    user_id = user_id,
    api_key = api_key
  )
  content <- response$content

  route_to_df <- function(route) {
    tibble(
      route_id = route$route_id,
      route_gtfs_id = route$route_gtfs_id,
      route_name = route$route_name,
      route_type = route$route_type,
      route_number = ifelse(
        # Not a number, eg. "745a"
        route$route_number == "", NA_character_, route$route_number
      ),
      service_status = route$route_service_status$description,
      service_status_timestamp = lubridate::ymd_hms(
        route$route_service_status$timestamp,
        tz = "Australia/Melbourne",
        quiet = TRUE
      )
    )
  }

  purrr::map_dfr(content$routes, route_to_df)
}

#' Retrieve a translation from route type number to name
#'
#' Route types (tram, train, etc.) are provided to the PTV API by number. This
#' function retrieves a named vector in which the values are the route type
#' descriptions, and the names of the vector are the route type numbers. Note
#' that "Night Bus" is a separate route type.
#'
#' @inheritParams PTVGET
#'
#' @return A named vector in which the values are the route type descriptions,
#' and the names of the vector are the route type numbers.
#' @export
#'
#' @examples \dontrun{route_types()}
route_types <- function(user_id = determine_user_id(),
                        api_key = determine_api_key()) {
  response <- PTVGET(
    request = "route_types",
    user_id = user_id,
    api_key = api_key
  )
  content_route_types <- response$content$route_types

  route_type_names <- purrr::map_chr(
    content_route_types,
    function(x) x$route_type_name
  )
  route_type_numbers <- purrr::map_chr(
    content_route_types,
    function(x) x$route_type
  )
  names(route_type_names) <- route_type_numbers
  route_type_names
}
