# This function ostensibly supports a date_utc function. It appears to change
# the day on which departures are returned. However, only the earliest 7
# departures are shown for the day corresponsing to date_utc.

#' Retrieve the stopping pattern of a run
#'
#' A pattern consists of all departures, stops, routes, runs, directions and
#' disruptions associated with a particular run ID. This is returned as a list
#' of tibbles, with output corresponding to their respective API calls.
#'
#' The `stops` tibble has an output similar to that returned by
#' `stops_on_route`. The `routes` tibble does not contain service status
#' information.
#'
#' @details Departures: The API seems to return the earliest 7 departures. While
#' the PTV Timetable API supports filtering patterns by datetimes, the
#' behaviour of this argument is not reliable --- it appears to filter by day
#' only, returning the earliest 7 departures of a different day. It is
#' recommended that departures are retrieved via the `departures()` function.
#'
#' @inheritParams run_information
#' @inheritParams translate_route_type
#' @inheritParams disruptions_on_route
#' @param departs POSIXct or character. Optionally filter by date. See Details.
#'   Characters are automatically converted to departs, and are assumed to be
#'   given as Melbourne time. The behaviour of the API is unpredictable when
#'   using this argument --- see details. Defaults to the current system time.
#' @inheritParams PTVGET
#'
#' @return An object of class "ptvapi", which is effectively a list with the
#'   following names: \itemize{ \item `departures` \item `stops` \item `routes`
#'   \item `runs` \item `directions` \item `disruptions` }
#'
#' @export
#'
#' @examples \dontrun{
#' patterns(run_id = 1, route_type = 0)
#' patterns(run_id = 1, route_type = "Train")
#' }
#'
patterns <- function(run_id,
                     route_type,
                     stop_id = NULL,
                     departs = Sys.time(),
                     user_id = determine_user_id(),
                     api_key = determine_api_key()) {
  run_id <- to_integer(run_id)
  route_type <- translate_route_type(route_type)
  if (!is.null(stop_id)) stop_id <- to_integer(stop_id)
  departs <- to_datetime(departs)
  url_departs <- format(departs, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  request <- add_parameters(
    glue::glue("pattern/run/{run_id}/route_type/{route_type}"),
    expand = "all",
    stop_id = stop_id,
    date_utc = url_departs
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "departures", "stops", "routes", "runs", "directions",
      "status")
  )

  structure(
    list(
      departures = map_and_rbind(content$departures, departure_to_tibble),
      stops = map_and_rbind(content$stops, stop_to_tibble),
      routes = map_and_rbind(content$routes, route_to_tibble),
      runs = map_and_rbind(content$runs, run_to_tibble),
      directions = map_and_rbind(content$directions, tibble::as_tibble),
      disruptions = list(content$disruptions)
    ),
    class = "ptvapi",
    request = response$request,
    status_code = response$status_code,
    content = response$content
  )
}
