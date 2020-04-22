#' Generate a URL with devid and signature
#'
#' @inheritParams PTVGET
#'
#' @return A complete URL character that can be queried with httr
#'
#' @keywords internal
#'
generate_request_url <- function(request,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {
  version <- 3

  # devid is a parameter, which means it goes on the end of the URL like so:
  # url?param1=value&param2=value&param3=value
  # If a URL already has a ? parameter, we join the devid with &
  # Otherwise, we start appending parameters to a URL with ?
  conjunction <- ifelse(grepl("\\?", request), "&", "?")
  request_with_devid <- glue::glue(
    "/v{version}/{request}{conjunction}devid={user_id}"
  )
  signature <- digest::hmac(
    key = api_key,
    object = request_with_devid,
    algo = "sha1"
  )
  signature <- toupper(signature)

  glue::glue(
    generate_request_url_without_auth(request),
    "{conjunction}devid={user_id}",
    "&signature={signature}"
  )
}

#' Generate a URL without devid and signature
#'
#' @inheritParams PTVGET
#'
#' @return A complete URL character that can be queried with httr
#'
#' @keywords internal
generate_request_url_without_auth <- function(request) {
  base_url <- "http://timetableapi.ptv.vic.gov.au"
  version <- 3 # This function is untested on other versions

  glue::glue("{base_url}/v{version}/{request}")
}
