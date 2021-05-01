#' Calculate a fare estimate between zones
#'
#' Retrieve fare information for a journey through the given zones. Also
#' supports journey touch on and off times, to accommodate for discounts.
#'
#' @param min_zone Integer. Minimum zone travelled through.
#' @param max_zone Integer. Maximum zone travelled through.
#' @param journey_touch_on,journey_touch_off POSIXct or Character. Optionally
#'   filter results to a journey time. Values to both must be provided.
#'   Characters are automatically converted to datetimes, and are assumed to be
#'   given as Melbourne time.
#' @param journey_in_free_tram_zone Boolean. Defaults to `FALSE`.
#' @param travelled_route_types Integer or character vector. Optionally filter
#'   by a vector of route types. A route type can be provided either as a
#'   non-negative integer code, or as a character: "Tram", "Train", "Bus",
#'   "Vline" or "Night Bus". Character inputs are not case-sensitive. Use the
#'   `route_types` function to extract a vector of all route types.
#' @inheritParams PTVGET
#'
#' @inherit parse_fare_estimate_content return
#'
#' @export
#'
#' @examples \dontrun{
#' fare_estimate(min_zone = 1, max_zone = 2)
#'
#' fare_estimate(min_zone = 1, max_zone = 1, journey_in_free_tram_zone = TRUE)
#'
#' fare_estimate(
#'   min_zone = 1,
#'   max_zone = 2,
#'   travelled_route_types = c("Train", "Tram")
#' )
#'
#' fare_estimate(
#'   min_zone = 1,
#'   max_zone = 2,
#'   journey_touch_on = "2020-06-21 07:31:00",
#'   journey_touch_off = "2020-06-21 08:45:00"
#'  )
#' }
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

  if (!is.null(travelled_route_types)) {
    travelled_route_types <- purrr::map_int(
      travelled_route_types,
      translate_route_type,
      user_id = user_id,
      api_key = api_key
    )
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

#' Parse content of fare estimates API call
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `fare_estimate` function.
#'
#' @param fare_estimate_content A direction, as a list, returned by the
#'   `fare_estimate` API call.
#'
#' @return A data frame consisting of one row for each `passenger_type`, and the
#' following columns: \itemize{
#' \item `min_zone`
#' \item `max_zone`
#' \item `unique_zones`
#' \item `early_bird`
#' \item `free_tram_zone`
#' \item `weekend_journey`
#' \item `passenger_type`
#' \item `fare_2_hour_peak`
#' \item `fare_2_hour_off_peak`
#' \item `fare_daily_peak`
#' \item `fare_daily_off_peak`
#' \item `pass_7_days`
#' \item `pass_28_to_69_day_per_day`
#' \item `pass_70_plus_day_per_day`
#' \item `weekend_cap`
#' \item `holiday_cap`
#' }
#'
#' @keywords internal
#'
parse_fare_estimate_content <- function(fare_estimate_content) {
  assert_correct_attributes(
    names(fare_estimate_content),
    c("IsEarlyBird", "IsJourneyInFreeTramZone", "IsThisWeekendJourney",
      "ZoneInfo", "PassengerFares")
  )

  base_columns <- tibble::tibble(
    min_zone = fare_estimate_content$ZoneInfo$MinZone,
    max_zone = fare_estimate_content$ZoneInfo$MaxZone,
    unique_zones = list(unlist(fare_estimate_content$ZoneInfo$UniqueZones)),
    early_bird = fare_estimate_content$IsEarlyBird,
    free_tram_zone = fare_estimate_content$IsJourneyInFreeTramZone,
    weekend_journey = fare_estimate_content$IsThisWeekendJourney
  )
  passenger_fares <- map_and_rbind(
    fare_estimate_content$PassengerFares,
    function(x) {
      tibble::tibble(
        passenger_type = x$PassengerType,
        fare_2_hour_peak = x$Fare2HourPeak,
        fare_2_hour_off_peak = x$Fare2HourOffPeak,
        fare_daily_peak = x$FareDailyPeak,
        fare_daily_off_peak = x$FareDailyOffPeak,
        pass_7_days = x$Pass7Days,
        pass_28_to_69_day_per_day = x$Pass28To69DayPerDay,
        pass_70_plus_day_per_day = x$Pass70PlusDayPerDay,
        weekend_cap = x$WeekendCap,
        holiday_cap = x$HolidayCap
      )
    }
  )

  # cbind converts to data.frame, but this shouldn't matter
  cbind(base_columns, passenger_fares)
}
