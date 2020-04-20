#' Directions for a given route
#'
#' @param route_id Integer. These can be listed and described with the `routes`
#' function.
#' @inheritParams PTVGET
#'
#' @return directions
#' @export
#'
directions <- function(route_id,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {
  request <- glue::glue("directions/route/{route_id}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  purrr::map_dfr(content$directions, tibble::as_tibble)
}
