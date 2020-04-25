#' Stop information (metropolitan and V/Line stations only)
#'
#' Stops can be searched for by exactly one of the following methods: \itemize{
#' \item Both a `stop_id` and `route_type` \item Both a `route_id` and
#' `route_type` \item Both a `latitude` and `longitude` }
#'
#' @section Swagger documentation:
#'   \url{http://timetableapi.ptv.vic.gov.au/swagger/ui/index#/Stops}
#'
#' @param stop_id Integer. These can be searched for with either
#' `stops_on_route` or `stops_nearby`.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @return A single-row tibble with the following columns: \itemize{
#' \item{`stop_id`}
#' \item{`stop_name`}
#' \item{`route_type`}
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

  route_type <- translate_route_type(route_type)
  request <- glue::glue("stops/{stop_id}/route_type/{route_type}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(names(content), c("stop", "disruptions", "status"))
  stop <- content$stop

  null_to_char_na <- function(x) ifelse(is.null(x), NA_character_, x)
  tibble::tibble(
    stop_id = stop$stop_id,
    stop_name = trimws(stop$stop_name),
    route_type = stop$route_type,
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
    stop_contact = null_to_char_na(stop$stop_contact),
    stop_ticket = null_to_char_na(stop$stop_ticket),
    stop_location = null_to_char_na(stop$stop_location),
    stop_amenities = null_to_char_na(stop$stop_amenities),
    stop_accessibility = null_to_char_na(stop$stop_accessibility),
    stop_staffing = null_to_char_na(stop$stop_staffing),
    disruption_ids = list(stop$disruption_ids)
  )
}

#' Stop information with route_id and route_type
#'
#' @inheritSection stop_information Swagger documentation
#'
#' @inheritParams route_directions
#' @param direction Optionally filter by a direction ID. These can be obtained
#'   with the `route_directions` function.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
stops_on_route <- function(route_id,
                           route_type,
                           direction = NULL,
                           user_id = determine_user_id(),
                           api_key = determine_api_key()) {

  route_type <- translate_route_type(route_type)
  request <- glue::glue("stops/route/{route_id}/route_type/{route_type}")
  if (!is.null(direction)) {
    request <- glue::glue(request, "?direction_id={direction}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  purrr::map_dfr(content$stops, stop_to_tibble)
}

#' Search for stops near a location
#'
#' @inheritSection stop_information Swagger documentation
#'
#' @param latitude Numeric. Latitude in decimal degrees. For example, Flinders
#'   Street Station is at approximately -37.8183 latitude.
#' @param longitude Numeric. Longitude in decimal degrees. For example, Flinders
#'   Street Station is at approximately 144.9671 longitude.
#' @param route_types Optionally filter by a vector of route types. A route type
#'   can be provided either as a non-negative integer code, or as a character:
#'   "Tram", "Train", "Bus", "Vline" or "Night Bus". Character inputs are not
#'   case-sensitive. Use the `route_types` function to extract a vector of all
#'   route types.
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' stops_nearby(latitude = -37.8183, longitude = 144.9671)
#' }
stops_nearby <- function(latitude,
                         longitude,
                         route_types = NULL,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {

  assertthat::assert_that(is.numeric(latitude))
  assertthat::assert_that(is.numeric(longitude))

  request <- glue::glue("stops/location/{latitude},{longitude}")
  if (!is.null(route_types)) {
    route_types <- purrr::map_int(route_types, translate_route_type)
    route_types_html_list <- paste(route_types, collapse = "%2C")
    request <- add_parameter(request, "route_types", route_types_html_list)
  }

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  purrr::map_dfr(content$stops, stop_to_tibble)
}

#' Convert a single stop to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `stops_on_route` and `stop_nearby` functions.
#'
#' @param route A stop, as a list, returned by the `stops` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item{`stop_id`}
#' \item{`stop_name`}
#' \item{`stop_suburb`}
#' \item{`route_type`}
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
    stop_sequence = ifelse(
      stop$stop_sequence == 0, NA_integer_, stop$stop_sequence
    ),
    stop_latitude = stop$stop_latitude,
    stop_longitude = stop$stop_longitude,
    disruption_ids = list(stop$disruption_ids)
  )
}
