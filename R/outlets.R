#' Retrieve outlet information
#'
#' @details The business hours are reported as characters. Usually they take on
#' a format of "8.00AM - 10.00PM", but there variants such as "7.30AM - 11.00AM
#' and 1.30PM - 6.00PM". For days on which an outlet is closed, the opening
#' hours are usually reported as "CLOSED", but can also be an empty character.
#' Some opening hours are "24 Hours". These fields are also filled with missing
#' values and empty characters.
#'
#' @inheritParams PTVGET
#'
#' @inherit outlet_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{outlets()}
#'
outlets <- function(user_id = determine_user_id(),
                    api_key = determine_api_key()) {
  request <- "outlets"
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("outlets", "status")
  )

  parsed <- map_and_rbind(content$outlets, outlet_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Retrieve outlet information near a given location
#'
#' @inherit outlets details
#'
#' @inheritParams stops_nearby
#' @inheritParams PTVGET
#'
#' @inherit outlet_to_tibble return
#'
#' @export
#'
#' @examples \dontrun{
#' outlets_nearby(latitude = -37.8183, longitude = 144.9671)
#' }
#'
outlets_nearby <- function(latitude,
                           longitude,
                           user_id = determine_user_id(),
                           api_key = determine_api_key()) {
  request <- glue::glue("outlets/location/{latitude},{longitude}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("outlets", "status")
  )

  parsed <- map_and_rbind(content$outlets, outlet_to_tibble)
  new_ptvapi_tibble(response, parsed)
}

#' Convert a single outlet to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `outlets` and `outlets_nearby` functions.
#'
#' @param outlet An outlet, as a list, returned by the `outlets` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item {`outlet_slid_spid`}
#' \item {`outlet_name`}
#' \item {`outlet_business`}
#' \item {`outlet_latitude`}
#' \item {`outlet_longitude`}
#' \item {`outlet_suburb`}
#' \item {`outlet_postcode`}
#' \item {`outlet_business_hour_mon`}
#' \item {`outlet_business_hour_tue`}
#' \item {`outlet_business_hour_wed`}
#' \item {`outlet_business_hour_thu`}
#' \item {`outlet_business_hour_fri`}
#' \item {`outlet_business_hour_sat`}
#' \item {`outlet_business_hour_sun`}
#' \item {`outlet_notes`}
#' }
#'
#' @keywords internal
#'
outlet_to_tibble <- function(outlet) {
  null_to_na_char <- function(x) ifelse(is.null(x), NA_character_, x)
  tibble::tibble(
    outlet_slid_spid = outlet$outlet_slid_spid,
    outlet_name = outlet$outlet_name,
    outlet_business = outlet$outlet_business,
    outlet_latitude = outlet$outlet_latitude,
    outlet_longitude = outlet$outlet_longitude,
    outlet_suburb = outlet$outlet_suburb,
    outlet_postcode = outlet$outlet_postcode,
    outlet_business_hour_mon = null_to_na_char(outlet$outlet_business_hour_mon),
    outlet_business_hour_tue = null_to_na_char(outlet$outlet_business_hour_tue),
    outlet_business_hour_wed = null_to_na_char(outlet$outlet_business_hour_wed),
    outlet_business_hour_thu = null_to_na_char(outlet$outlet_business_hour_thu),
    outlet_business_hour_fri = null_to_na_char(outlet$outlet_business_hour_fri),
    outlet_business_hour_sat = null_to_na_char(outlet$outlet_business_hour_sat),
    outlet_business_hour_sun = null_to_na_char(outlet$outlet_business_hour_sun),
    outlet_notes = null_to_na_char(outlet$outlet_notes)
  )
}
