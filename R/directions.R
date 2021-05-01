#' Directions for a given direction ID
#'
#' This function returns all directions with a given ID. Directions that share
#' an ID are not necessarily related, especially if not filtering by route type.
#' It's advised to use to the \code{\link{directions_on_route}} function to
#' search for directions of interest.
#'
#' @param direction_id Integer.
#' @param route_type Optionally filter results by a route type. A route type can
#'   be provided either as a non-negative integer code, or as a character:
#'   "Tram", "Train", "Bus", "Vline" or "Night Bus". Character inputs are not
#'   case-sensitive. Use the \code{\link{route_types}} function to extract a
#'   vector of all route types.
#' @inheritParams PTVGET
#'
#' @inherit parse_directions_content return
#' @export
#'
#' @examples \dontrun{
#' directions(direction_id = 5)
#' directions(direction_id = 5, route_type = "Train")
#' directions(direction_id = 5, route_type = 0)
#' }
#'
directions <- function(direction_id,
                       route_type = NULL,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {
  direction_id <- to_integer(direction_id)
  request <- glue::glue("directions/{direction_id}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(
      route_type,
      user_id = user_id,
      api_key = api_key
    )
    request <- glue::glue(request, "/route_type/{route_type}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  parsed <- parse_directions_content(content)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Directions on a given route
#'
#' @param route_id Integer. These can be listed and described with the
#'   \code{\link{routes}} function.
#' @inheritParams PTVGET
#'
#' @inherit parse_directions_content return
#'
#' @export
#'
#' @examples \dontrun{
#' directions_on_route(6)
#' }
directions_on_route <- function(route_id,
                                user_id = determine_user_id(),
                                api_key = determine_api_key()) {
  route_id <- to_integer(route_id)
  request <- glue::glue("directions/route/{route_id}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  parsed <- parse_directions_content(content)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Parse content of directions API call
#'
#' This function is designed to parse the content returned by the interior
#' steps of the \code{\link{directions}} function.
#'
#' @param directions_content A direction, as a list, returned by the
#'   `directions` API call.
#'
#' @return A tibble consisting of the following columns: \itemize{
#' \item `direction_id`
#' \item `direction_name`,
#' \item `route_id`
#' \item `route_type`
#' \item `route_type_description`
#' \item `route_direction_description`
#' }
#'
#' @keywords internal
#'
parse_directions_content <- function(directions_content) {
  assert_correct_attributes(
    names(directions_content),
    c("directions", "status")
  )
  parsed <- map_and_rbind(directions_content$directions, tibble::as_tibble)

  assert_correct_attributes(
    colnames(parsed),
    c("route_direction_description", "direction_id", "direction_name",
      "route_id", "route_type")
  )

  parsed$route_type_description <- NA_character_

  parsed[c("direction_id", "direction_name", "route_id", "route_type",
           "route_type_description", "route_direction_description")]
}
