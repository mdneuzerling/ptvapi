#' Information for a given run
#'
#' Run IDs are not unique across the network. If you are interested in a
#' specific run, consider supplying a value to the optional `route_type`
#' argument.
#'
#' @param run_ref A character run reference. This supersedes the integer
#'   `run_id`. For backwards compatibility and since most run references are
#'   integers, this function will attempt to convert an the argument to a
#'   character. Run references may be retrieved from the `departures` or
#'   `runs_on_route` functions.
#' @param date_utc Date, or character that can be converted to a date. The
#'   UTC date for which the results are effective. Defaults to the current date.
#'   It's uncertain how much historical or future-dated data is available. This
#'   argument is experimental and seems to not be functioning.
#' @inheritParams directions
#' @inheritParams PTVGET
#' @inheritParams route_information
#'
#' @inherit run_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' run_information("100")
#' run_information("100", include_geopath = TRUE)
#' run_information("100", include_geopath = TRUE, geopath_utc = "2020-07-01")
#' run_information("100", date_utc = "2020-07-01")
#' }
#'
run_information <- function(run_ref,
                            route_type = NULL,
                            include_geopath = FALSE,
                            geopath_utc = NULL,
                            date_utc = NULL,
                            user_id = determine_user_id(),
                            api_key = determine_api_key()) {
  run_ref <- as.character(run_ref)
  if (!is.null(date_utc)) date_utc <- as.character(as.Date(date_utc))
  if (!include_geopath && !is.null(geopath_utc)) {
    warning("`geopath_utc` is ignored when `include_geopath` is `TRUE`")
    geopath_utc <- NULL
  }
  if (!is.null(geopath_utc)) {
    geopath_utc <- as.Date(geopath_utc)
  }

  request <- glue::glue("runs/{run_ref}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(
      route_type,
      user_id = user_id,
      api_key = api_key
    )
    request <- glue::glue("{request}/route_type/{route_type}")
  }
  request <- add_parameters(
    request,
    include_geopath = include_geopath,
    geopath_utc = geopath_utc,
    date_utc = date_utc
  )

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  # Internally, the API response has a "run" attribute if a route type is
  # specified (as runs are unique up to route type) and "runs" otherwise
  if (!is.null(route_type)) {
    assert_correct_attributes(names(content), c("run", "status"))
    parsed <- run_to_tibble(content$run)
  } else {
    assert_correct_attributes(names(content), c("runs", "status"))
    parsed <- map_and_rbind(content$runs, run_to_tibble)
  }

  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Runs on a given route
#'
#' @inheritParams directions_on_route
#' @inheritParams directions
#' @inheritParams PTVGET
#' @inheritParams run_information
#'
#' @inherit run_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' runs_on_route(6)
#' runs_on_route(6, route_type = "Train")
#' runs_on_route(6, route_type = 0)
#' }
#'
runs_on_route <- function(route_id,
                          route_type = NULL,
                          date_utc = NULL,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  route_id <- to_integer(route_id)
  if (!is.null(date_utc)) date_utc <- as.character(as.Date(date_utc))
  request <- glue::glue("runs/route/{route_id}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(
      route_type,
      user_id = user_id,
      api_key = api_key
    )
    request <- glue::glue("{request}/route_type/{route_type}")
  }
  request <- add_parameters(request, date_utc = date_utc)
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content

  parsed <- map_and_rbind(content$runs, run_to_tibble)
  parsed$route_type_description <- purrr::map_chr(
    parsed$route_type,
    describe_route_type,
    user_id = user_id,
    api_key = api_key
  )
  new_ptvapi_tibble(response, parsed)
}

#' Convert a single run to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `runs_on_route` and `run_information` functions.
#'
#' @param route A run, as a list, returned by the `runs` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item `run_id` (deprecated, use `run_ref` instead)
#' \item `run_ref`
#' \item `route_id`
#' \item `route_type`
#' \item `route_type_description`
#' \item `direction_id`
#' \item `run_sequence`
#' \item `final_stop_id`
#' \item `destination_name`
#' \item `status`
#' \item `express_stop_count`
#' \item `vehicle_position`
#' \item `vehicle_descriptor`
#' \item `geopath`
#' }
#'
#' @keywords internal
#'
run_to_tibble <- function(run) {
  tibble::tibble(
    run_id = run$run_id,
    run_ref = run$run_ref,
    route_id = run$route_id,
    route_type = run$route_type,
    route_type_description = NA_character_,
    direction_id = run$direction_id,
    run_sequence = run$run_sequence,
    final_stop_id = run$final_stop_id,
    destination_name = trimws(run$destination_name),
    status = run$status,
    express_stop_count = run$express_stop_count,
    vehicle_position = list(run$vehicle_position),
    vehicle_descriptor = list(run$vehicle_descriptor),
    geopath = if (is.null(run$geopath)) {
      list()
    } else {
      list(purrr::map_dfr(run$geopath, geopath_to_tibble))
    },
  )
}
