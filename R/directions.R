#' Directions for a given direction ID
#'
#' @param direction_id Integer. Try searching for a direction_id on a particular
#'   route with `route_direction`.
#' @param route_type Optionally filter results by a route type. A route type can
#'   be provided either as a non-negative integer code, or as a character:
#'   "Tram", "Train", "Bus", "Vline" or "Night Bus". Character inputs are not
#'   case-sensitive. Use the `route_types` function to extract a vector of all
#'   route types.
#' @inheritParams PTVGET
#'
#' @inherit parse_directions_content return
#' @export
#'
directions <- function(direction_id,
                       route_type = NULL,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {
  request <- glue::glue("directions/{direction_id}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(route_type)
    request <- glue::glue(request, "/route_type/{route_type}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  parse_directions_content(content)
}

#' Directions for a given route_id
#'
#' @param route_id Integer. These can be listed and described with the `routes`
#'   function.
#' @inheritParams PTVGET
#'
#' @inherit parse_directions_content return
#' @export
#'
route_directions <- function(route_id,
                             user_id = determine_user_id(),
                             api_key = determine_api_key()) {
  request <- glue::glue("directions/route/{route_id}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  parse_directions_content(content)
}

#' Parse content of directions API call
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `directions` function.
#'
#' @param directions_content A direction, as a list, returned by the
#'   `directions` API call.
#'
#' @return A tibble consisting of the following columns: \itemize{
#' \item `direction_id`
#' \item `direction_name`,
#' \item `route_id`
#' \item `route_type`
#' \item `route_direction_description`
#' }
#'
#' @export
#'
parse_directions_content <- function(directions_content) {
  assert_correct_attributes(
    names(directions_content),
    c("directions", "status")
  )
  parsed <- purrr::map_dfr(directions_content$directions, tibble::as_tibble)

  assert_correct_attributes(
    colnames(parsed),
    c("route_direction_description", "direction_id", "direction_name",
      "route_id", "route_type")
  )

  parsed[c("direction_id", "direction_name", "route_id", "route_type",
           "route_direction_description")]
}
