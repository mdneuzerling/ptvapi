# There's only one API call here: search. It uses the given search term to find
# routes, stops or outlets. I can't implement a function with the name "search"
# because that's a pretty important base R function. I've decided here to split
# it into three functions: `search_routes()`, `search_stops()`, and
# `search_outlets()`. This way, the results will be predictable and consistent
# with other functions, and we don't pollute the namespace with the internal
# `search()` function.

#' Use a character term to search for routes, stops, and outlets.
#'
#' There's only one search API call, and it covers stops, routes, and outlets.
#' This function will return the response of this generic search. It is
#' expected that other functions will take on of these three categories of
#' search results, parse them, and return them to the user as a tibble.
#'
#' If the search term is numeric and/or less than 3 characters, the API will
#' return only routes.
#'
#' @param search_term Character. Term used to perform search.
#' @inheritParams PTVGET
#'
#' @return The response of the `search` API call.
#'
#' @keywords internal
#'
ptv_search <- function(search_term,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {
  search_term <- make_url_friendly(search_term)
  request <- glue::glue("search/{search_term}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("stops", "routes", "outlets", "status")
  )
  response
}

#' Use a character term to search for routes.
#'
#' This function will search routes in which the search term can be found in
#' one of many fields, such as `route_id`, `route_gtfs_id`, or `route_name`.
#' The search is case-insensitive. Unlike `search_stops()` and
#' `search_outlets()`, this function supports searching for numerics, and has
#' no minimum character requirement for `search_term`.
#'
#' @inheritParams ptv_search
#' @inheritParams PTVGET
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_routes("Pakenham")
#' }
search_routes <- function(search_term,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  response <- ptv_search(
    search_term = search_term,
    user_id = user_id,
    api_key = api_key
  )
  content <- response$content

  parsed <- map_and_rbind(content$routes, route_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Use a character term to search for stops.
#'
#' This function will search stops in which the search term can be found in
#' either the stop name or the stop suburb. The search is case-insensitive.
#' The search term must contain at least 3 characters, and cannot be numeric.
#'
#' @inheritParams ptv_search
#' @inheritParams PTVGET
#'
#' @inherit stop_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_routes("Ascot Vale")
#' }
search_stops <- function(search_term,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {
  if (is.numeric(search_term) || nchar(search_term) <= 3) {
    stop("Search term cannot be numeric and must be at least 3 characters")
  }
  response <- ptv_search(
    search_term = search_term,
    user_id = user_id,
    api_key = api_key
  )
  content <- response$content

  parsed <- map_and_rbind(content$stops, stop_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Use a character term to search for outlets.
#'
#' This function will search outlets in which the search term can be found in
#' either the outlet name, outlet business or outlet suburb. The search is
#' case-insensitive. The search term must contain at least 3 characters, and
#' cannot be numeric.
#'
#' @inheritParams ptv_search
#' @inheritParams PTVGET
#'
#' @inherit outlet_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' search_outlets("St Kilda")
#' }
search_outlets <- function(search_term,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {
  if (is.numeric(search_term) || nchar(search_term) <= 3) {
    stop("Search term cannot be numeric and must be at least 3 characters")
  }
  response <- ptv_search(
    search_term = search_term,
    user_id = user_id,
    api_key = api_key
  )
  content <- response$content

  parsed <- map_and_rbind(content$outlets, outlet_to_tibble)
  new_ptvapi_tibble(response, parsed)
}
