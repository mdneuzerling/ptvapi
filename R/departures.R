# We don't provide the `date_utc` and `max_results` arguments to the API, as the
# behaviour of the API is unpredictable and contrary to its documentation. We
# also don't use the `route_id` request, because the results are not always
# filtered to the given route ID:
#
# * Requesting a maximum of `n` results does not guarantee that the returned
# tibble will be `n` rows per route ID and departure, and often fewer than `n`
# departures are returned.
#
# * When providing a value to the `route_id` argument as well as to
# `max_results`, the results may contain route IDs other than the given
# argument. For example, running `departures(1071, "Train", route_id = 1,
# max_results = 1)` will return departures for route 1 (Alamein) with direction
# ID 0, but also route 7 (Glen Waverley) with direction ID 1.
#
# * As documented, the API should provide the upcoming departures after the
# given `date_utc` argument (defaults to the current time). In practice, the API
# returns results _around_ the given `date_utc` argument, roughly 10--15 minutes
# before and after.
#
# * The API does support a `look_backwards` parameter (defaults to `FALSE`) but
# it appears to have no  effect at all.
#
# Instead, we apply a filter in R to the results to ensure that `departs` and
# `max_results` are respected, and that `max_results` applies per route ID. We
# also ignore the option to filter by route_id via the API, and filter in R
# instead. This is all performed with the `filter_departures` function.


#' Departures from a given stop
#'
#' `departures` retrieves all upcoming departures for a given stop ID and route
#' type.
#'
#' @details Filtering departures: The API supports filtering by departure time,
#'   to show the departures after the given time. However, its behaviour is
#'   unpredictable, returning departures around the given time, both before and
#'   after. We apply an additional filter once the results are retrieved to
#'   ensure that only departures at or after the given `departs` datetime are
#'   shown.
#'
#'   It's not clear what functionality `look_backwards` has. It's included here
#'   regardless. Moreover, it's not clear how the API treats `route_id` or
#'   `max_results`. We filter the results after retrieval, to ensure that
#'   `departs`, `max_results`, and `route_id` are respected. This additional
#'   validation can be disabled by setting `validate_results = TRUE`.
#'
#' @param stop_id An integer stop ID returned by the `stops_on_route` or
#'   `stops_nearby` functions.
#' @inheritParams translate_route_type
#' @param route_id Optionally filter by a route ID. These can be obtained with
#'   the `routes` function.
#' @inheritParams stops_on_route
#' @param platform_numbers Character vector. Optionally filter results by
#'   platform number. Despite the name, these are characters.
#' @param departs POSIXct or Character. Optionally filter results to departures
#'   on or after the given value, according to either scheduled or estimated
#'   departure time. Characters are automatically converted to datetimes, and
#'   are assumed to be given as Melbourne time. Defaults to the current system
#'   time.
#' @param look_backwards Boolean. Whether to look before `departs`. Use with
#'   caution (see Details). Defaults to `FALSE`.
#' @param max_results Integer. The maximum number of departures to return for
#'   each route_id. Departures are ordered by estimated departure time, when
#'   available, and scheduled departure time otherwise. When set to 0, all
#'   departures after the given `departs` for the entire day are shown, and
#'   potentially some in the early hours of the next morning. Defaults to 5.
#' @param include_cancelled Logical. Whether results should be returned if they
#'   have been cancelled. Metropolitan train services only. Defaults to FALSE.
#' @param validate_results Boolean. If TRUE (the default), will apply additional
#'   filters to ensure that the arguments to `departs`, `max_results`, and
#'   `route_id` are respected if given.
#' @inheritParams PTVGET
#'
#' @export
#'
#' @examples \dontrun{
#' departures(stop_id = 1071, route_type = "Train")
#' departures(stop_id = 1071, route_type = 0)
#'
#' departures(
#'   stop_id = 1071,
#'   route_type = "Train",
#'   platform_numbers = c(4, 5)
#' )
#'
#' departures(
#'   stop_id = 1071,
#'   route_type = "Train",
#'   route_id = 6
#' )
#'
#' departures(
#'   stop_id = 1071,
#'   route_type = "Train",
#'   departs = "2020-06-23 17:05:00"
#' )
#'
#' }
#'
departures <- function(stop_id,
                       route_type,
                       route_id = NULL,
                       direction_id = NULL,
                       platform_numbers = NULL,
                       departs = Sys.time(),
                       look_backwards = FALSE,
                       max_results = 5,
                       include_cancelled = FALSE,
                       validate_results = TRUE,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {

  # if max_results is unset or 0, entire day's results will be returned
  # Suggestion: we check if the given departs is non-null and contains a
  # time component. If max_results is NULL, raise a warning/

  stop_id <- to_integer(stop_id)
  route_type <- translate_route_type(route_type)
  if (!is.null(route_id)) route_id <- to_integer(route_id)
  if (!is.null(max_results)) max_results <- to_integer(max_results)
  if (!is.null(platform_numbers)) {
    platform_numbers <- purrr::map_int(platform_numbers, to_integer)
  }
  departs <- to_datetime(departs)
  url_departs <- format(departs, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  request <- add_parameters(
    glue::glue("departures/route_type/{route_type}/stop/{stop_id}"),
    route_type = route_type,
    route_id = route_id,
    direction_id = direction_id,
    platform_numbers = platform_numbers,
    look_backwards = look_backwards,
    max_results = max_results, # results for the whole day. We'll filter later.
    date_utc = url_departs,
    include_cancelled = include_cancelled
  )

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("departures", "stops", "routes", "runs", "directions", "disruptions",
      "status")
  )

  parsed <- map_and_rbind(content$departures, departure_to_tibble)
  if (validate_results) {
    parsed <- filter_departures(
      parsed,
      departs = departs,
      route_id = route_id,
      max_results = max_results
    )
  }
  new_ptvapi_tibble(response, parsed)
}


#' Filter parsed departures content according to user input
#'
#' The departures API call isn't always reliable. This function will take a
#' tibble of parsed departures content and filter it according to the following
#' inputs, if they are not `NULL`: \itemize{
#' \item Only departures after the given `departs`
#' \item Only departures on the given route ID
#' \item The next max_results departures per route ID, if `max_results` is not
#'   0.
#' }
#'
#' @param parsed A tibble of parsed departures content.
#' @param departs POSIXct in the "Australia/Melbourne" time zone.
#' @param route_id Integer.
#' @param max_results Integer max results.
#'
#' @return A filtered tibble.
#'
#' @keywords internal
#'
filter_departures <- function(parsed,
                              departs = NULL,
                              route_id = NULL,
                              max_results = NULL) {

  # Everything is done according to estimated departure if available, and
  # otherwise scheduled departure.
  # coalesce2 from https://stackoverflow.com/a/19254510
  coalesce2 <- function(...) {
    Reduce(function(x, y) {
      i <- which(is.na(x))
      x[i] <- y[i]
      x},
      list(...))
  }
  parsed["departure"] <- coalesce2(
    parsed$estimated_departure,
    parsed$scheduled_departure
  )

  if (!is.null(departs)) {
    parsed <- subset(parsed, departure >= departs)
  }

  if (!is.null(route_id)) {
    input_route_id <- route_id # have to copy because there's no data masking
    parsed <- subset(parsed, route_id == input_route_id)
  }

  if (!is.null(max_results) && max_results > 0) {
    subset_order_and_head <- function(route_id_part) {
      sub_route_id <- subset(parsed, route_id == route_id_part)
      utils::head(
        sub_route_id[order(sub_route_id$departure), ],
        max_results
      )
    }

    parsed <- map_and_rbind(unique(parsed$route_id), subset_order_and_head)
  }

  parsed <- parsed[order(parsed$departure), ]
  parsed[, names(parsed) != "departure"]
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
