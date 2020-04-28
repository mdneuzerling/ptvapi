#' Disruptions on a given route
#'
#' @inheritParams translate_route_type
#' @param stop_id Integer. Optionally filter results to a specific stop ID.
#'   These can be searched for with the `stops_on_route()` and `stops_nearby()`
#'   functions.
#' @inheritParams PTVGET
#'
#' @inherit disruption_to_tibble return
#'
#' @export
#'
disruptions_on_route <- function(route_id,
                                 stop_id = NULL,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {
  request <- glue::glue("disruptions/route/{route_id}")
  if (!is.null(stop_id)) {
    assertthat::assert_that(is.integer(stop_id))
    request <- glue::glue("{request}/stop/{stop_id}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  purrr::map_dfr(content$disruptions$metro_train, disruption_to_tibble)
}

#' Convert a single disruptions to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `disruptions_on_route` and `disruptions_at_stop` functions.
#'
#' @param disruption A disruption, as a list, returned by the `disruptions` API
#'   call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item `disruption_id`
#' \item `title`
#' \item `url`
#' \item `description`
#' \item `disruption_status`
#' \item `disruption_type`
#' \item `published_on`
#' \item `last_updated`
#' \item `from_date`
#' \item `to_date`
#' \item `routes`
#' \item `stops`
#' \item `colour`
#' \item `display_on_board`
#' \item `display_status`
#' }
#' @export
#'
disruption_to_tibble <- function(disruption) {
  tibble::tibble(
    disruption_id = disruption$disruption_id,
    title = disruption$title,
    url = disruption$url,
    description = disruption$description,
    disruption_status = disruption$disruption_status,
    disruption_type = disruption$disruption_type,
    published_on = disruption$published_on,
    last_updated = disruption$last_updated,
    from_date = disruption$from_date,
    to_date = disruption$to_date,
    routes = purrr::map_dfr(disruption$routes, route_to_tibble),
    stops = disruption$stops,
    colour = disruption$colour,
    display_on_board = disruption$display_on_board,
    display_status = disruption$display_status
  )
}
