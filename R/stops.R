#' Information for a given stop (metropolitan and V/Line stations only)
#'
#' This function can be used when integer stop ID is already known. This can be
#' searched for with either the \code{\link{stops_on_route}} or
#' \code{\link{stops_nearby}} functions.
#'
#' @param stop_id Integer stop ID.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @return A single-row tibble with the following columns: \itemize{
#' \item{`stop_id`}
#' \item{`stop_name`}
#' \item{`route_type`}
#' \item{`route_type_description`}
#' \item{`station_details_id`}
#' \item{`station_type`}
#' \item{`station_description`}
#' \item{`point_id`}
#' \item{`mode_id`}
#' \item{`operating_hours`}
#' \item{`flexible_stop_opening_hours`}
#' \item{`stop_contact`}
#' \item{`stop_ticket`}
#' \item{`stop_location`}
#' \item{`stop_amenities`}
#' \item{`stop_accessibility`}
#' \item{`stop_staffing`}
#' \item{`disruption_ids`}
#' }
#' @export
#'
stop_information <- function(stop_id,
                             route_type,
                             user_id = determine_user_id(),
                             api_key = determine_api_key()) {
  stop_id <- to_integer(stop_id)
  route_type <- translate_route_type(
    route_type,
    user_id = user_id,
    api_key = api_key
  )
  request <- add_parameters(
    glue::glue("stops/{stop_id}/route_type/{route_type}"),
    stop_location = TRUE,
    stop_amenities = TRUE,
    stop_accessibility = TRUE,
    stop_contact = TRUE,
    stop_ticket = TRUE,
    stop_staffing = TRUE,
    stop_disruptions = TRUE
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(names(content), c("stop", "disruptions", "status"))
  stop <- content$stop

  as_tibble_null_to_na <- function(x, prefix = "") {
    tib <- tibble::as_tibble(
      purrr::map(x, ~ifelse(is.null(.x), NA, .x))
    )
    colnames(tib) <- paste0(prefix, colnames(tib))
    tib
  }
  major <- tibble::tibble(
    stop_id = stop$stop_id,
    stop_name = trimws(stop$stop_name),
    route_type = stop$route_type,
    route_type_description = purrr::map_chr(
      stop$route_type,
      describe_route_type,
      user_id = user_id,
      api_key = api_key
    ),
    station_details_id = stop$station_details_id,
    station_type = stop$station_type,
    station_description = stop$station_description,
    point_id = stop$point_id,
    mode_id = stop$mode_id,
    operating_hours = stop$operating_hours,
    flexible_stop_opening_hours = ifelse(
      stop$flexible_stop_opening_hours == "",
      NA_character_,
      stop$flexible_stop_opening_hours
    ),
    disruption_ids = list(stop$disruption_ids)
  )
  contact <- as_tibble_null_to_na(stop$stop_contact, prefix = "contact_")
  ticket <- as_tibble_null_to_na(stop$stop_ticket, prefix = "ticket_")
  location <- as_tibble_null_to_na(stop$stop_location, prefix = "location_")
  amenities <- as_tibble_null_to_na(stop$stop_amenities, prefix = "amenity_")
  accessibility <- as_tibble_null_to_na(
    stop$stop_accessibility,
    prefix = "accessibility_"
  )
  staffing <- as_tibble_null_to_na(stop$stop_staffing, prefix = "staffing_")

  parsed <- tibble::as_tibble(
    cbind(
      major, contact, ticket, location, amenities, accessibility, staffing
    )
  )
  new_ptvapi_tibble(response, parsed)
}

#' Stops on a given route and route type
#'
#' @inheritParams directions_on_route
#' @param direction_id Optionally filter by a direction ID. These can be
#'   obtained with the \code{\link{directions_on_route}} function.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' stops_on_route(6, route_type = "Train")
#' stops_on_route(6, route_type = 0)
#' }
#'
stops_on_route <- function(route_id,
                           route_type,
                           direction_id = NULL,
                           user_id = determine_user_id(),
                           api_key = determine_api_key()) {
  route_id <- to_integer(route_id)
  route_type <- translate_route_type(
    route_type,
    user_id = user_id,
    api_key = api_key
  )
  if (!is.null(direction_id)) direction_id <- to_integer(direction_id)
  request <- add_parameters(
    glue::glue("stops/route/{route_id}/route_type/{route_type}"),
    direction_id = direction_id,
    stop_disruptions = TRUE
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  parsed <- map_and_rbind(content$stops, stop_to_tibble)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Stops near a given location
#'
#' @param latitude Numeric. Latitude in decimal degrees. For example, Flinders
#'   Street Station is at approximately -37.8183 latitude.
#' @param longitude Numeric. Longitude in decimal degrees. For example, Flinders
#'   Street Station is at approximately 144.9671 longitude.
#' @param max_distance Integer. Optionally filter by maximum distance from the
#'   given location, in metres.
#' @param route_types Integer or character vector. Optionally filter by a vector
#'   of route types. A route type can be provided either as a non-negative
#'   integer code, or as a character: "Tram", "Train", "Bus", "Vline" or "Night
#'   Bus". Character inputs are not case-sensitive. Use the
#'   \code{\link{route_types}} function to extract a vector of all route types.
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' stops_nearby(latitude = -37.8183, longitude = 144.9671)
#' stops_nearby(latitude = -37.8183, longitude = 144.9671, max_distance = 1000)
#' stops_nearby(
#'   latitude = -37.8183,
#'   longitude = 144.9671,
#'   route_types = c("Train", "Tram")
#' )
#'
#' stops_nearby(
#'   latitude = -37.8183,
#'   longitude = 144.9671,
#'   route_types = 0
#'  )
#' }
stops_nearby <- function(latitude,
                         longitude,
                         max_distance = NULL,
                         route_types = NULL,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {

  assertthat::assert_that(is.numeric(latitude))
  assertthat::assert_that(is.numeric(longitude))
  if (!is.null(max_distance)) max_distance <- to_integer(max_distance)

  if (!is.null(route_types)) {
    route_types <- purrr::map_int(route_types, translate_route_type)
  }

  request <- add_parameters(
    glue::glue("stops/location/{latitude},{longitude}"),
    max_distance = max_distance,
    route_types = route_types,
    stop_disruptions = TRUE
  )

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  parsed <- map_and_rbind(content$stops, stop_to_tibble)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Convert a single stop to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the \code{\link{stops_on_route}} and \code{\link{stops_nearby}}
#' functions.
#'
#' @param stop A stop, as a list, returned by a stops API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item{`stop_id`}
#' \item{`stop_name`}
#' \item{`stop_suburb`}
#' \item{`route_type`}
#' \item{`route_type_description`}
#' \item{`stop_sequence`}
#' \item{`stop_latitude`}
#' \item{`stop_longitude`}
#' \item{`disruption_ids`}
#' }
#'
#' @keywords internal
#'
stop_to_tibble <- function(stop) {
  tibble::tibble(
    stop_id = stop$stop_id,
    stop_name = trimws(stop$stop_name),
    stop_suburb = stop$stop_suburb,
    route_type = stop$route_type,
    route_type_description = NA_character_,
    stop_sequence = ifelse(
      stop$stop_sequence == 0, NA_integer_, stop$stop_sequence
    ),
    stop_latitude = stop$stop_latitude,
    stop_longitude = stop$stop_longitude,
    disruption_ids = list(stop$disruption_ids)
  )
}
