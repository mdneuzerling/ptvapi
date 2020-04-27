#' Retrieve the stopping pattern of a run
#'
#' A pattern consists of all departures, stops, routes, runs, directions and
#' disruptions associated with a particular run ID. This is returned as a list
#' of tibbles, with output corresponding to their respective API calls.
#'
#' The `stops` tibble has an output similar to that returned by `stops_on_route`.
#' The `routes` tibble does not contain service status information.
#'
#' @inheritParams run_information
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @return A tibble with the following columns: \itemize{
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
patterns <- function(run_id,
                     route_type,
                     user_id = determine_user_id(),
                     api_key = determine_api_key()) {
  request <- glue::glue("pattern/run/{run_id}/route_type/{route_type}")
  request <- add_parameter(request, "expand", "all")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "departures", "stops", "routes", "runs", "directions",
      "status")
  )

  list(
    departures = purrr::map_dfr(content$departures, departure_to_tibble),
    stops = purrr::map_dfr(content$stops, stop_to_tibble),
    routes = purrr::map_dfr(content$routes, route_to_tibble),
    runs = purrr::map_dfr(content$runs, run_to_tibble),
    directions = purrr::map_dfr(content$directions, tibble::as_tibble),
    disruptions = list(content$disruptions)
  )
}
