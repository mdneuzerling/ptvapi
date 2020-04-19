#' Stop information (metropolitan and V/Line stations only)
#'
#' Stops can be searched for by exactly one of the following methods:
#' \itemize{
#'   \item Both a `stop_id` and `route_type`
#'   \item Both a `route_id` and `route_type`
#'   \item Both a `latitude` and `longitude`
#' }
#'
#' @section Swagger documentation:
#'   \url{http://timetableapi.ptv.vic.gov.au/swagger/ui/index#/Stops}
#'
#' @param stop_id
#' @param route_id
#' @param route_type
#' @param latitude
#' @param longitude
#' @inheritParams PTVGET
#'
#' @return Stops
#' @export
#'
#' @examples
stops <- function(stop_id,
                  route_id,
                  route_type,
                  latitude,
                  longitude,
                  user_id = determine_user_id(),
                  api_key = determine_api_key()) {

  present <- c(
    "stop_id" = !missing(stop_id),
    "route_id" = !missing(route_id),
    "route_type" = !missing(route_type),
    "latitude" = !missing(latitude),
    "longitude" = !missing(longitude)
  )

  if (xor(present["latitude"], present["longitude"])) {
    stop("If using latitude and longitude, both must be provided")
  }

  if (sum(present[c("stop_id", "route_id", "latitude")]) != 1) {
    stop(
      "Provide either a stop_id, route_id, or both of latitude and longitude"
    )
  }

}

#' Stop information with stop_id and route_type
#'
#' @inheritSection stops Swagger documentation
#'
#' @inheritParams PTVGET
#' @inheritParams stops
#'
#' @return Stops
#'
#' @keywords internal
#'
stops_with_stop_id <- function(stop_id,
                               route_type,
                               user_id = determine_user_id(),
                               api_key = determine_api_key()) {

}
