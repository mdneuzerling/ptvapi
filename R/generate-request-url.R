#' Generate a URL with devid and signature
#'
#' @inheritParams PTVGET
#'
#' @return A complete URL character that can be queried with httr
#'
#' @keywords internal
generate_request_url <- function(request,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {
  version <- 3

  signature <- digest::hmac(
    key = api_key,
    object = glue::glue("/v{version}/{request}?devid={user_id}"),
    algo = "sha1"
  )

  glue::glue(
    generate_request_url_without_auth(request),
    "?devid={user_id}",
    "&signature={toupper(signature)}"
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
