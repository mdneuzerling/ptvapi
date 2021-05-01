#' Information for a given route
#'
#' @inheritParams directions_on_route
#' @inheritParams PTVGET
#' @param include_geopath Logical. Whether to populate the `geopath` column.
#'   Defaults to FALSE.
#' @param geopath_utc Date, or character that can be converted to a date. The
#'   UTC date for which the geopaths are effective. Defaults to the current
#'   date. Has no effect if `include_geopath = FALSE`. It's uncertain how much
#'   historical or future-dated data is available.
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' route_information(6)
#' route_information(6, include_geopath = TRUE)
#' route_information(6, include_geopath = TRUE, geopath_utc = "2020-07-01")
#' }
#'
route_information <- function(route_id,
                              include_geopath = FALSE,
                              geopath_utc = NULL,
                              user_id = determine_user_id(),
                              api_key = determine_api_key()) {
  route_id <- to_integer(route_id)
  if (!include_geopath && !is.null(geopath_utc)) {
    warning("`geopath_utc` is ignored when `include_geopath` is `TRUE`")
    geopath_utc <- NULL
  }
  if (!is.null(geopath_utc)) {
    geopath_utc <- as.Date(geopath_utc)
  }

  request <- add_parameters(
    glue::glue("routes/{route_id}"),
    include_geopath = include_geopath,
    geopath_utc = geopath_utc
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  assert_correct_attributes(names(content), c("route", "status"))
  parsed <- route_to_tibble(content$route) # may be an empty tibble
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Information for all routes
#'
#' @inheritParams stops_nearby
#' @param route_name Character. Optionally filter by route name. Partial matches
#'   are accepted, and the matches are not case sensitive.
#' @inheritParams PTVGET
#'
#' @inherit route_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' routes()
#' routes(route_types = "Train")
#' routes(route_types = 0)
#' routes(route_types = c("Train", "Tram"))
#' routes(route_name = "Frankston")
#' routes(route_name = "Craigie")
#' routes(route_name = "werribee")
#' }
routes <- function(route_types = NULL,
                   route_name = NULL,
                   user_id = determine_user_id(),
                   api_key = determine_api_key()) {
  if (!is.null(route_types)) {
    route_types <- purrr::map_int(
      route_types,
      translate_route_type,
      user_id = user_id,
      api_key = api_key
    )
  }
  if (!is.null(route_name)) assertthat::assert_that(is.character(route_name))

  request <- add_parameters(
    "routes",
    route_types = route_types,
    route_name = route_name
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  parsed <- map_and_rbind(content$routes, route_to_tibble)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Convert a single route to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `routes` function. Service status information may be `NA`,
#' depending on the API call that was used to populate the information.
#'
#' @param route A route, as a list, returned by the `routes` API call.
#'
#' @return A tibble of routes, with the following columns:
#' \itemize{
#'   \item `route_id`
#'   \item `route_gtfs_id`
#'   \item `route_name`
#'   \item `route_type`
#'   \item `route_type_description`
#'   \item `route_number`
#'   \item `geopath`
#'   \item `service_status`
#'   \item `service_status_timestamp`
#' }
#'
#' @keywords internal
#'
route_to_tibble <- function(route) {
  if (is.null(route)) {
    return(
      tibble::tibble(
        route_id = integer(),
        route_gtfs_id = character(),
        route_name = character(),
        route_type = integer(),
        route_type_description = character(),
        route_number = character(),
        geopaths = list(),
        service_status = character(),
        service_status_timestamp = as.POSIXct(NA)
      )
    )
  }
  tibble::tibble(
    route_id = route$route_id,
    route_gtfs_id = route$route_gtfs_id,
    route_name = route$route_name,
    route_type = route$route_type,
    route_type_description = NA_character_,
    route_number = ifelse(
      # Not always a number, eg. "745a"
      route$route_number == "", NA_character_, route$route_number
    ),
    geopath = if (is.null(route$geopath)) {
      list()
    } else {
      list(purrr::map_dfr(route$geopath, geopath_to_tibble))
    },
    service_status = ifelse(
      is.null(route$route_service_status$description),
      NA_character_,
      route$route_service_status$description
    ),
    service_status_timestamp = convert_to_melbourne_time(
      route$route_service_status$timestamp
    )
  )
}
