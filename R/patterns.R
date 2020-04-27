#' Retrieve patterns on a run
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
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("distruptions", "departures", "stops", "routes", "runs", "directions",
      "status")
  )

  tibble::tibble(
    departures = list(purrr::map_dfr(content$departures, departure_to_tibble)),
    stops = list(purrr::map_dfr(content$stops, stop_to_tibble)),
    routes = list(purrr::map_dfr(content$routes, route_to_tibble)),
    runs = list(purrr::map_dfr(content$runs, run_to_tibble)),
    directions = list(purrr::map_dfr(content$directions, tibble::as_tibble)),
    disruptions = list(departures)
  )
}
