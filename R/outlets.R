outlets <- function(user_id = determine_user_id(),
                    api_key = determine_api_key()) {
  request <- "outlets"
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("outlets", "status")
  )

  map_and_rbind(content$outlets, outlet_to_tibble)
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
  tibble::tibble(
    outlet_slid_spid = outlet$outlet_slid_spid,
    outlet_name = outlet$outlet_name,
    outlet_business = outlet$outlet_business,
    outlet_latitude = outlet$outlet_latitude,
    outlet_longitude = outlet$outlet_longitude,
    outlet_suburb = outlet$outlet_suburb,
    outlet_postcode = outlet$outlet_postcode,
    outlet_business_hour_mon = outlet$outlet_business_hour_mon,
    outlet_business_hour_tue = outlet$outlet_business_hour_tue,
    outlet_business_hour_wed = outlet$outlet_business_hour_wed,
    outlet_business_hour_thu = outlet$outlet_business_hour_thu,
    outlet_business_hour_fri = outlet$outlet_business_hour_fri,
    outlet_business_hour_sat = outlet$outlet_business_hour_sat,
    outlet_business_hour_sun = outlet$outlet_business_hour_sun,
    outlet_notes = ifelse(
      is.null(outlet$outlet_notes), NA_character_, outlet$outlet_notes
    )
  )
}


