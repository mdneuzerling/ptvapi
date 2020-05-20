# There's only one API call here: search. It uses the given search term to find
# routes, stops or outlets. I can't implement a function with the name "search"
# because that's a pretty important base R function. I've decided here to split
# it into three functions: `search_routes`, `search_stops`, and
# `search_outlets`. This way, the results will be predictable and consistent
# with other functions, and we don't pollute the namespace with the internal
# `search` function.

#' Use a character term to search for routes, stops, and outlets.
#'
#' There's only one search API call, and it covers stops, routes, and outlets.
#' This function will return the response of this generic search. It is
#' expected that other functions will take on of these three categories of
#' search results, parse them, and return them to the user as a tibble.
#'
#' If the search term is numeric and/or less than 3 characters, the API will
#' return only routes. By default, as little matching is done as possible, and
#' as little as possible is returned. We rely on functions that call on this
#' function to specify what is needed.
#'
#' @param search_term Character. Term used to perform search.
#' @inheritParams stops_nearby
#' @param include_outlets Boolean. Optional. Affects search results.
#' @param match_stop_by_suburb Boolean. Optional. Affects search results.
#' @param match_route_by_suburb Boolean. Optional. Affects search results.
#' @param match_stop_by_gtfs_stop_id Boolean. Optional. Affects search results.
#' @inheritParams PTVGET
#'
#' @return The response of the `search` API call.
#'
#' @keywords internal
#'
ptv_search <- function(search_term,
                       latitude = NULL,
                       longitude = NULL,
                       max_distance = NULL,
                       route_types = NULL,
                       include_outlets = FALSE,
                       match_stop_by_suburb = FALSE,
                       match_route_by_suburb = FALSE,
                       match_stop_by_gtfs_stop_id = FALSE,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {

  search_term <- make_url_friendly(search_term)
  if (!is.null(latitude)) assertthat::assert_that(is.numeric(latitude))
  if (!is.null(longitude)) assertthat::assert_that(is.numeric(longitude))
  if (xor(is.null(latitude), is.null(longitude))) {
    stop("If searching near a location, both latitude and longitude must be ",
         "provided")
  }
  if (!is.null(max_distance) && is.null(longitude)) {
    stop("Trying to limit search results with a maximum distance, but a ",
         "latitude and longitude wasn't provided")
  }
  if (!is.null(max_distance)) max_distance <- to_integer(max_distance)
  if (!is.null(route_types)) {
    route_types <- purrr::map_int(route_types, translate_route_type)
  }

  request <- add_parameters(
    glue::glue("search/{search_term}"),
    latitude = latitude,
    longitude = longitude,
    max_distance = max_distance,
    route_types = route_types,
    include_outlets = include_outlets,
    match_stop_by_suburb = match_stop_by_suburb,
    match_route_by_suburb = match_route_by_suburb,
    match_stop_by_gtfs_stop_id = match_stop_by_gtfs_stop_id
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("stops", "routes", "outlets", "status")
  )
  response
}

#' Search for routes using text
#'
#' This function will search routes in which the search term can be found in
#' one of many fields, such as `route_id`, `route_gtfs_id`, or `route_name`.
#' The search is case-insensitive. Unlike `search_stops` and
#' `search_outlets`, this function supports searching for numerics, and has
#' no minimum character requirement for `search_term`.
#'
#' @inheritParams ptv_search
#' @inheritParams stops_nearby
#' @inheritParams PTVGET
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_routes("Pakenham")
#' search_routes("Pakenham", route_types = c("Train", "Tram"))
#' search_routes("Pakenham", route_types = 1)
#'
#' search_routes(
#'   "Pakenham",
#'   latitude = -38.077877,
#'   longitude = 145.484751
#' )
#' search_routes(
#'   "Pakenham",
#'   latitude = -38.077877,
#'   longitude = 145.484751,
#'   max_distance = 100
#' )
#' }
search_routes <- function(search_term,
                          latitude = NULL,
                          longitude = NULL,
                          max_distance = NULL,
                          route_types = NULL,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  response <- ptv_search(
    search_term = search_term,
    latitude = latitude,
    longitude = longitude,
    max_distance = max_distance,
    route_types = route_types,
    user_id = user_id,
    api_key = api_key,
    match_route_by_suburb = TRUE
  )
  content <- response$content

  parsed <- map_and_rbind(content$routes, route_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Search for stops using text
#'
#' This function will search stops in which the search term can be found in
#' either the stop name or the stop suburb. The search is case-insensitive.
#' The search term must contain at least 3 characters, and cannot be numeric.
#'
#' @inheritParams ptv_search
#' @inheritParams stops_nearby
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_stops("Ascot Vale")
#' search_stops("Ascot Vale", route_types = c("Train", "Tram"))
#' search_stops("Ascot Vale", route_types = 1)
#'
#' search_stops(
#'   "Ascot Vale",
#'   latitude = -37.774240,
#'   longitude = 144.915518
#' )
#' search_stops(
#'   "Ascot Vale",
#'   latitude = -37.774240,
#'   longitude = 144.915518,
#'   max_distance = 100
#' )
#' }
search_stops <- function(search_term,
                         latitude = NULL,
                         longitude = NULL,
                         max_distance = NULL,
                         route_types = NULL,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {
  if (is.numeric(search_term) || nchar(search_term) <= 3) {
    stop("Search term cannot be numeric and must be at least 3 characters")
  }
  response <- ptv_search(
    search_term = search_term,
    latitude = latitude,
    longitude = longitude,
    max_distance = max_distance,
    route_types = route_types,
    user_id = user_id,
    api_key = api_key,
    match_stop_by_suburb = TRUE,
    match_stop_by_gtfs_stop_id = TRUE
  )
  content <- response$content

  parsed <- map_and_rbind(content$stops, stop_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Search for outlets using text
#'
#' This function will search outlets in which the search term can be found in
#' either the outlet name, outlet business or outlet suburb. The search is
#' case-insensitive. The search term must contain at least 3 characters, and
#' cannot be numeric.
#'
#' @inheritParams ptv_search
#' @inheritParams stops_nearby
#' @inheritParams PTVGET
#'
#' @inherit outlet_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_outlets("St Kilda")
#' search_outlets("St Kilda", route_types = c("Train", "Tram"))
#' search_outlets("St Kilda", route_types = 1)
#'
#' search_outlets(
#'   "St Kilda",
#'   latitude = -37.867647,
#'   longitude = 144.976809
#' )
#' search_outlets(
#'   "St Kilda",
#'   latitude = -37.867647,
#'   longitude = 144.976809,
#'   max_distance = 100
#' )
#' }
search_outlets <- function(search_term,
                           latitude = NULL,
                           longitude = NULL,
                           max_distance = NULL,
                           route_types = NULL,
                           user_id = determine_user_id(),
                           api_key = determine_api_key()) {
  if (is.numeric(search_term) || nchar(search_term) <= 3) {
    stop("Search term cannot be numeric and must be at least 3 characters")
  }
  response <- ptv_search(
    search_term = search_term,
    latitude = latitude,
    longitude = longitude,
    max_distance = max_distance,
    route_types = route_types,
    user_id = user_id,
    api_key = api_key,
    include_outlets = TRUE
  )
  content <- response$content

  parsed <- map_and_rbind(content$outlets, outlet_to_tibble)
  new_ptvapi_tibble(response, parsed)
}
