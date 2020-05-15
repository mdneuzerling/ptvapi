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
#' @inheritParams run_information
#' @inheritParams translate_route_type
#' @inheritParams disruptions_on_route
#' @inheritParams departures
#' @inheritParams PTVGET
#'
#' @return An object of class "ptvapi", which is effectively a list with the
#'   following names: \itemize{
#' \item `departures`
#' \item `stops`
#' \item `routes`
#' \item `runs`
#' \item `directions`
#' \item `disruptions`
#' }
#'
#' @export
#'
#' @examples \dontrun{
#' patterns(run_id = 1, route_type = 0)
#' patterns(run_id = 1, route_type = "Train")
#' patterns(run_id = 1, route_type = "Train", datetime = "2020-03-01T16:41:50")
#' }
#'
patterns <- function(run_id,
                     route_type,
                     stop_id = NULL,
                     datetime = NULL,
                     user_id = determine_user_id(),
                     api_key = determine_api_key()) {
  run_id <- to_integer(run_id)
  route_type <- translate_route_type(route_type)
  if (!is.null(stop_id)) stop_id <- to_integer(stop_id)
  if (!is.null(datetime)) datetime <- to_datetime(datetime)

  request <- add_parameters(
    glue::glue("pattern/run/{run_id}/route_type/{route_type}"),
    expand = "all",
    stop_id = stop_id,
    date_utc = datetime
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
