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
  parsed <- purrr::map_dfr(content$directions, tibble::as_tibble)

  assert_correct_attributes(
    parsed,
    c("route_direction_description", "direction_id", "direction_name",
      "route_id", "route_type")
  )

  parsed

}
