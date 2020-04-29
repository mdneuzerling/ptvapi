#' Retrieve departures at a stop
#'
#' @details
#' All timestamps returned by this function are in Melbourne time.
#'
#' @param stop_id An integer stop ID returned by the `stops_on_route` or
#'   `stops_nearby` functions.
#' @inheritParams translate_route_type
#' @param route_id Optionally filter by a route ID. These can be obtained with
#'   the `routes` function.
#' @inheritParams PTVGET
#'
#' @export
#'
departures <- function(stop_id,
                       route_type,
                       route_id = NULL,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {

  route_type <- translate_route_type(route_type)
  request <- glue::glue("departures/route_type/{route_type}/stop/{stop_id}")

  if (!is.null(route_id)) {
    assertthat::assert_that(is.integer(route_id))
    request <- glue::glue("{request}/route/{route_id}")
  }

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("departures", "stops", "routes", "runs", "directions", "disruptions",
      "status")
  )

  purrr::map_dfr(content$departures, departure_to_tibble)
}

#' Convert a single departure to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `departures` functions.
#'
#' @param departure A departure, as a list, returned by the `departures` API
#' call.
#'
#' @return A tibble consisting of the following columns: \itemize{
#' \item `stop_id`
#' \item `route_id`
#' \item `run_id`
#' \item `direction_id`
#' \item `disruption_ids`
#' \item `scheduled_departure`
#' \item `estimated_departure`
#' \item `at_platform`
#' \item `platform_number`
#' \item `flags`
#' \item `departure_sequence`
#' }
#'
#' @keywords internal
#'
departure_to_tibble <- function(departure) {
  tibble::tibble(
    direction_id = departure$direction_id,
    stop_id = departure$stop_id,
    route_id = departure$route_id,
    run_id = departure$run_id,
    platform_number = ifelse(
      is.null(departure$platform_number),
      NA_character_,
      departure$platform_number
    ),
    at_platform = departure$at_platform,
    departure_sequence = departure$departure_sequence,
    scheduled_departure = convert_to_melbourne_time(
      departure$scheduled_departure_utc
    ),
    estimated_departure = convert_to_melbourne_time(
        departure$estimated_departure_utc
    ),
    flags = departure$flags,
    disruption_ids = list(departure$disruption_ids)
  )
}
