#' Calculate a fare estimate between zones.
#'
#' @param min_zone Integer. Minimum zone travelled through.
#' @param max_zone Integer. Maximum zone travelled through.
#' @param journey_touch_on POSIXct or Character. Optionally filter results to a
#'   journey time. If providing a value to this variable, a value must also be
#'   provided to `journey_touch_off`. Characters are automatically converted to
#'   datetimes, and are assumed to be given as Melbourne time.
#' @param journey_touch_off POSIXct or Character. Optionally filter results to a
#'   journey time. If providing a value to this variable, a value must also be
#'   provided to `journey_touch_ofn`. Characters are automatically converted to
#'   datetimes, and are assumed to be given as Melbourne time.
#' @param journey_in_free_tram_zone Boolean. Defaults to `FALSE`.
#' @param travelled_route_types Integer or character vector. Optionally filter
#'   by a vector of route types. A route type can be provided either as a
#'   non-negative integer code, or as a character: "Tram", "Train", "Bus",
#'   "Vline" or "Night Bus". Character inputs are not case-sensitive. Use the
#'   `route_types` function to extract a vector of all route types.
#' @inheritParams PTVGET
#'
#' @return
#' @export
#'
fare_estimate <- function(min_zone,
                          max_zone,
                          journey_touch_on = NULL,
                          journey_touch_off = NULL,
                          journey_in_free_tram_zone = FALSE,
                          travelled_route_types = NULL,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  min_zone <- to_integer(min_zone)
  max_zone <- to_integer(max_zone)

  if (xor(is.null(journey_touch_on), is.null(journey_touch_off))) {
    stop("If providing journey touch on/off times, both must be provided")
  }
  if (!is.null(journey_touch_on)) {
    journey_touch_on <- to_datetime(journey_touch_on)
    journey_touch_off <- to_datetime(journey_touch_off)
    journey_touch_on_url <- format(
      journey_touch_on, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
    )
    journey_touch_off_url <- format(
      journey_touch_off, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
    )
  } else {
    journey_touch_on_url <- NULL
    journey_touch_off_url <- NULL
  }

  request <- add_parameters(
    glue::glue("fare_estimate/min_zone/{min_zone}/max_zone/{max_zone}"),
    journey_touch_on_utc = journey_touch_on_url,
    journey_touch_off_utc = journey_touch_off_url,
    is_journey_in_free_tram_zone = journey_in_free_tram_zone,
    travelled_route_types = travelled_route_types
  )

  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("FareEstimateResultStatus", "FareEstimateResult")
  )

  parsed <- parse_fare_estimate_content(content$FareEstimateResult)
  new_ptvapi_tibble(response, parsed)
}

parse_fare_estimate_content <- function(fare_estimate_content) {
  assert_correct_attributes(
    names(fare_estimate_content),
    c("IsEarlyBird", "IsJourneyInFreeTramZone", "IsThisWeekendJourney",
      "ZoneInfo", "PassengerFares")
  )

  cbind(
    tibble::tibble(
      min_zone = fare_estimate_content$ZoneInfo$MinZone,
      max_zone = fare_estimate_content$ZoneInfo$MaxZone,
      unique_zones = list(unlist(fare_estimate_content$ZoneInfo$UniqueZones)),
      early_bird = fare_estimate_content$IsEarlyBird,
      free_tram_zone = fare_estimate_content$IsJourneyInFreeTramZone,
      weekend_journey = fare_estimate_content$IsThisWeekendJourney
    ),
    map_and_rbind(fare_estimate_content$PassengerFares, tibble::as_tibble)
  )
}
