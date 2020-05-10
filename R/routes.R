#' Retrieve route information
#'
#' @details
#' All timestamps returned by this function are in Melbourne time.
#'
#' @param route_id Integer. Optionally filter results to a particular route ID.
#' @inheritParams PTVGET
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{routes()}
routes <- function(route_id = NULL,
                   user_id = determine_user_id(),
                   api_key = determine_api_key()) {
  request <- "routes"
  if (!is.null(route_id)) {
    route_id <- to_integer(route_id)
    request <- glue::glue("{request}/{route_id}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  # Slightly different output for single route, which we coerce to the same
  # column names as if multiple routes were returned
  if (!is.null(route_id)) {
    assert_correct_attributes(names(content), c("route", "status"))
    if (is.null(content$route)) {
      return(tibble::tibble())
    }
    assert_correct_attributes(
      names(content$route),
      c("route_service_status", "route_type", "route_id", "route_name",
        "route_number", "route_gtfs_id")
    )
    parsed <- route_to_tibble(content$route)
  } else {
    # We'll naively parse a single route to make sure all of the attributes are
    # as expected. Simply passing a single route to `as_tibble` will create two
    # rows, as `route_service_status` is nested, but this is fine for checking
    # attributes.
    assert_correct_attributes(names(content), c("routes", "status"))
    routes_tibble_example <- tibble::as_tibble(content$routes[[1]])
    assert_correct_attributes(
      colnames(routes_tibble_example),
      c("route_service_status", "route_type", "route_id", "route_name",
        "route_number", "route_gtfs_id")
    )
    parsed <- map_and_rbind(content$routes, route_to_tibble)
  }

  new_ptvapi_tibble(response, parsed)

}

#' Convert a single route to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `routes` function. Service status information is only included
#' if it is available.
#'
#' @param route A route, as a list, returned by the `routes` API call.
#'
#' @return A tibble of routes, with the following columns:
#' \itemize{
#'   \item route_id
#'   \item route_gtfs_id
#'   \item route_name
#'   \item route_type
#'   \item route_number
#'   \item service_status
#'   \item service_status_timestamp
#' }
#'
#' @keywords internal
#'
route_to_tibble <- function(route) {
  route_tibble <- tibble::tibble(
    route_id = route$route_id,
    route_gtfs_id = route$route_gtfs_id,
    route_name = route$route_name,
    route_type = route$route_type,
    route_number = ifelse(
      # Not a number, eg. "745a"
      route$route_number == "", NA_character_, route$route_number
    )
  )

  if ("route_service_status" %in% names(route)) {
    route_tibble$service_status <- route$route_service_status$description
    route_tibble$service_status_timestamp <- convert_to_melbourne_time(
      route$route_service_status$timestamp
    )
  }

  route_tibble

}
