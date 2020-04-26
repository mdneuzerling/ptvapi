#' Retrieve a tibble of all runs on a particular route
#'
#' @inheritParams route_directions
#' @inheritParams directions
#' @inheritParams PTVGET
#'
#' @inherit run_to_tibble return
#'
#' @export
#'
runs_on_route <- function(route_id,
                          route_type = NULL,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  request <- glue::glue("runs/route/{route_id}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(route_type)
    request <- glue::glue("{request}/route_type/{route_type}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  purrr::map_dfr(content$runs, run_to_tibble)
}

#' Convert a single run to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `runs_on_route` and `run_information` functions.
#'
#' @param route A run, as a list, returned by the `runs` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item `run_id`
#' \item `route_id`
#' \item `route_type`
#' \item `direction_id`
#' \item `run_sequence`
#' \item `final_stop_id `
#' \item `destination_name`
#' \item `status`
#' \item `express_stop_count`
#' \item `vehicle_position`
#' \item `vehicle_descriptor`
#' }
#'
#' @keywords internal
#'
run_to_tibble <- function(run) {
  tibble(
    run_id = run$run_id,
    route_id = run$route_id,
    route_type = run$route_type,
    direction_id = run$direction_id,
    run_sequence = run$run_sequence,
    final_stop_id = run$final_stop_id,
    destination_name = trimws(run$destination_name),
    status = run$status,
    express_stop_count = run$express_stop_count,
    vehicle_position = list(run$vehicle_position),
    vehicle_descriptor = list(run$vehicle_descriptor)
  )
}
