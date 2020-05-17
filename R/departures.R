# We don't provide the `date_utc` and `max_results` arguments to the API, as the
# behaviour of the API is unpredictable and contrary to its documentation. We also
# don't use the `route_id` request, because the results are not always filtered
# to the given route ID:
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
# Instead, we always supply `max_results = 0` to the API, which returns all
# departures for the entire day. We then filter the results in R.


#' Retrieve departures at a stop
#'
#' @details All timestamps returned by this function are in Melbourne time.
#'
#' @param stop_id An integer stop ID returned by the `stops_on_route` or
#'   `stops_nearby` functions.
#' @inheritParams translate_route_type
#' @param route_id Optionally filter by a route ID. These can be obtained with
#'   the `routes()` function.
#' @inheritParams stops_on_route
#' @param platform_numbers Character vector. Optionally filter results by
#' platform number. Despite the name, these are characters.
#' @param datetime POSIXct or Character. Optionally filter results to a
#'   datetime. Characters are automatically converted to datetimes, and are
#'   assumed to be given as Melbourne time. Defaults to the current date and
#'   time.
#' @param max_results Integer. The maximum number of departures to return for
#'   each route_id. Defaults to 5.
#' @param include_cancelled Logical. Whether results should be returned if they
#'   have been cancelled. Metropolitan train services only. Defaults to FALSE.
#' @inheritParams PTVGET
#'
#' @export
#'
departures <- function(stop_id,
                       route_type,
                       route_id = NULL,
                       direction_id = NULL,
                       platform_numbers = NULL,
                       datetime = NULL,
                       max_results = 5,
                       include_cancelled = FALSE,
                       user_id = determine_user_id(),
                       api_key = determine_api_key()) {

  # if max_results is unset or 0, entire day's results will be returned
  # Suggestion: we check if the given datetime is non-null and contains a
  # time component. If max_results is NULL, raise a warning/

  stop_id <- to_integer(stop_id)
  route_type <- translate_route_type(route_type)
  # base R doesn't use data masking, so we need to rename route_id
  if (!is.null(route_id)) input_route_id <- to_integer(route_id)
  if (!is.null(max_results)) max_results <- to_integer(max_results)
  if (!is.null(platform_numbers)) {
    platform_numbers <- purrr::map_int(platform_numbers, to_integer)
  }
  if (!is.null(datetime)) {
    datetime <- as.POSIXct(
      datetime,
      tz = "Australia/Melbourne",
      tryFormats = c("%Y-%m-%dT%H:%M:%OS",
                     "%Y-%m-%d %H:%M:%OS",
                     "%Y/%m/%d %H:%M:%OS",
                     "%Y-%m-%d %H:%M",
                     "%Y/%m/%d %H:%M",
                     "%Y-%m-%d",
                     "%Y/%m/%d")
    )
  }
  url_datetime <- if (is.null(datetime)) {
    NULL
  } else {
    format(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  }

  request <- add_parameters(
    glue::glue("departures/route_type/{route_type}/stop/{stop_id}"),
    direction_id = direction_id,
    platform_numbers = platform_numbers,
    max_results = 0, # results for the whole day. We'll filter later.
    date_utc = url_datetime,
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

  if (!is.null(datetime)) {
    # from https://stackoverflow.com/a/19254510
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
    parsed <- subset(parsed, departure >= datetime)
    parsed <- parsed[, names(parsed) != "departure"]
  }

  if (!is.null(route_id)) {
    parsed <- subset(parsed, route_id == input_route_id)
  }

  if (max_results > 0) {
    parsed <- map_and_rbind(
      unique(parsed$route_id),
      function(x) utils::head(subset(parsed, route_id == x), max_results)
    )
  }

  new_ptvapi_tibble(response, parsed)
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
