#' Submit a GET request to the PTV API
#'
#' @section Obtaining API authentication details:
#'
#' You will need to obtain a user ID (also called a devid) and an API key from
#' Public Transport Victoria. These are obtained by email. Instructions are
#' available at
#' \url{https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/}.
#' You may pass these two pieces of information directly to the function, or you
#' can set the PTV_USER_ID and PTV_API_KEY environment variables.
#'
#' @param request A request or path for the API, eg. "routes".
#' @param user_id Integer or character. A user ID or devid provided by Public
#'   Transport Victoria. See section "Obtaining API authentication details".
#' @param api_key Character. An API key, with dashes, provided by Public
#'   Transport Victoria. See section "Obtaining API authentication details".
#' @param ... Additional arguments passed to `httr::GET`.
#'
#' @return A HTTP response. Content can be accessed with `httr::content`.
#' @export
#'
#' @examples \dontrun{
#' PTVGET(
#'   request = "routes",
#'   user_id = 123456,
#'   api_key = "4d840bc4-81e6-11ea-bc55-0242ac130003"
#' )
#' }
PTVGET <- function(request,
                   user_id = determine_user_id(),
                   api_key = determine_api_key(),
                   ...) {
  request_url <- generate_request_url(
    request = request,
    user_id = user_id,
    api_key = api_key
  )

  httr::GET(url = request_url, ...)
}

#' Submit a POST request to the PTV API
#'
#' @inheritSection PTVGET Obtaining API authentication details
#'
#' @inheritParams PTVGET
#' @param body Body of the POST request. Defaults to NULL, which submits an empty body. Refer to `httr::POST` for more details.
#' @param ... Additional arguments passed to `httr::POST`.
#'
#' @return A HTTP response. Content can be accessed with `httr::content`.
#' @export
#'
PTVPOST <- function(request,
                   user_id = determine_user_id(),
                   api_key = determine_api_key(),
                   body = NULL,
                   ...) {
  request_url <- generate_request_url(
    request = request,
    user_id = user_id,
    api_key = api_key
  )

  httr::POST(url = request_url, body = body, ...)
}
