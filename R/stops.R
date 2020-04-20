#' Stop information (metropolitan and V/Line stations only)
#'
#' Stops can be searched for by exactly one of the following methods: \itemize{
#' \item Both a `stop_id` and `route_type` \item Both a `route_id` and
#' `route_type` \item Both a `latitude` and `longitude` }
#'
#' @section Swagger documentation:
#'   \url{http://timetableapi.ptv.vic.gov.au/swagger/ui/index#/Stops}
#'
#' @param stop_id Integer. These can be searched for with either `stops_on_route` or `stops_nearby`.
#' @param route_id Integer. These can be listed and described with the `routes`
#' function.
#' @inheritParams translate_route_types
#' @inheritParams PTVGET
#'
#' @return Stops
#' @export
#'
stop_information <- function(stop_id,
                             route_type,
                             user_id = determine_user_id(),
                             api_key = determine_api_key()) {


}

#' Stop information with route_id and route_type
#'
#' @inheritSection stop_information Swagger documentation
#'
#' @inheritParams stop_information
#' @inheritParams translate_route_types
#' @inheritParams PTVGET
#'
#' @return Tibble
#'
#' @keywords internal
#'
stops_on_route <- function(route_id,
                           route_type,
                           direction = NULL,
                           user_id = determine_user_id(),
                           api_key = determine_api_key()) {

  route_type <- translate_route_types(route_type)
  request <- glue::glue("stops/route/{route_id}/route_type/{route_type}")
  if (!is.null(direction)) {
    request <- glue::glue(request, "?direction_id={direction}")
  }
  response <- PTVGET(request)
  content <- response$content

  stop_to_tibble <- function(stop) {
    tibble::tibble(
      stop_id = stop$stop_id,
      stop_name = stop$stop_name,
      stop_suburb = stop$stop_suburb,
      route_type = stop$route_type,
      stop_sequence = ifelse(
        stop$stop_sequence == 0, NA_integer_, stop$stop_sequence
      ),
      stop_latitude = stop$stop_latitude,
      stop_longitude = stop$stop_longitude
    )
  }

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
#' @param route_types Optionally filter by a route type. A route type can be
#'   provided either as a non-negative integer code, or as a character: "Tram",
#'   "Train", "Bus", "Vline" or "Night Bus". Character inputs are not
#'   case-sensitive. Use the `route_types` function to extract a vector of all
#'   route types.
#' @inheritParams PTVGET
#'
#' @return
#' @export
#'
#' @examples \dontrun{
#' stops_nearby(latitude = -37.8183, longitude = 144.9671)
#' }
stops_nearby <- function(latitude,
                         longitude,
                         route_types,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {


}
