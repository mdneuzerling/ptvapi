# nolint start
#' ptvapi: A package for accessing the Public Transport Victoria Timetable API
#'
#' @description
#' \if{html}{\figure{ptvapi.png}{options: alt='logo'}}
#'
#' Accessing the Public Transport Victoria Timetable API reqiures a user ID
#' (also called a `devid`) and an API key. These can be accessed by contacting
#' Public Transport Victoria. See
#' \url{https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/}
#'
#' The user ID and API key can be entered directly into all functions.
#' Alternatively, all functions will pick up on the PTV_USER_ID and API_KEY
#' environment variables, if defined.
#'
#' All API requests use SSL by default. To disable this, and to use the `http`
#' API endpoints rather than the `https` API endpoints, set the option:
#' ```
#' options(use_insecure_ptv_connection = TRUE)
#' ```
#'
#' @details
#' This is an unofficial wrapper of the Public Transport Victoria Timetable API.
#' The author(s) of this package are unaffiliated with Public Transport
#' Victoria.
#'
#' @examples \dontrun{
#' # tibble of all routes
#' routes()
#'
#' # Search for routes by name (case insensitive, partial matching supported)
#' routes(route_name = "Frankston")
#'
#' # All current disruptions
#' disruptions(disruption_status = "current")
#'
#' # Train stops near Flinders Street Station
#' stops_nearby(
#'   latitude = -37.8183,
#'   longitude = 144.9671,
#'   route_types = "Train"
#' )
#'
#' # Upcoming train departures from Flinders Street Station
#' departures(stop_id = 1071, route_type = "Train")
#' }
"_PACKAGE"
# nolint end
