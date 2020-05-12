#' Retrieve route information
#'
#' @details
#' All timestamps returned by this function are in Melbourne time.
#'
#' @param route_id Integer. Optionally filter results to a particular route ID.
#' @inheritParams stops_nearby
#' @param route_name Character. Optionally filter by route name. Partial matches
#'   are accepted, and the matches are not case sensitive.
#' @inheritParams PTVGET
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{routes()}
routes <- function(route_id = NULL,
                   route_types = NULL,
                   route_name = NULL,
                   user_id = determine_user_id(),
                   api_key = determine_api_key()) {
  if (!is.null(route_id)) route_id <- to_integer(route_id)
  if (!is.null(route_types)) {
    route_types <- purrr::map_int(route_types, translate_route_type)
  }
  if (!is.null(route_name)) assertthat::assert_that(is.character(route_name))

  request <- add_parameters(
    "routes",
    route_id = route_id,
    route_types = route_types,
    route_name = route_name
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  # Slightly different output for single route, which we coerce to the same
  # column names as if multiple routes were returned
  if (!is.null(route_id)) {
    assert_correct_attributes(names(content), c("route", "status"))
    parsed <- route_to_tibble(content$route) # may be an empty tibble
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
#' steps of the `routes` function. Service status information may be `NA`,
#' depending on the API call that was used to populate the information.
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
    route_id = integer(),
    route_gtfs_id = character(),
    route_name = character(),
    route_type = integer(),
    route_number = character(),
    service_status = character(),
    service_status_timestampe = as.POSIXct(NA)
  )

  if (is.null(route)) {
    return(route_tibble)
  }

  rbind(
    route_tibble,
    tibble::tibble(
      route_id = route$route_id,
      route_gtfs_id = route$route_gtfs_id,
      route_name = route$route_name,
      route_type = route$route_type,
      route_number = ifelse(
        # Not a number, eg. "745a"
        route$route_number == "", NA_character_, route$route_number
      ),
      service_status = ifelse(
        is.null(route$route_service_status$description),
        NA_character_,
        route$route_service_status$description
      ),
      service_status_timestampe = convert_to_melbourne_time(
        route$route_service_status$timestamp
      )
    )
  )

}
