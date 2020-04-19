#' Generate a URL with devid and signature
#'
#' @param request Character for request/path, eg. "routes"
#' @inheritParams PTVGET
#'
#' @return A complete URL character that can be queried with httr
#'
#' @keywords internal
generate_request_url <- function(request,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {

  base_url <- "http://timetableapi.ptv.vic.gov.au"
  version <- 3 # This function is untested on other versions

  signature <- digest::hmac(
    key = api_key,
    object = glue::glue("/v{version}/{request}?devid={user_id}"),
    algo = "sha1"
  )

  glue::glue(
    base_url,
    "/v{version}",
    "/{request}",
    "?devid={user_id}",
    "&signature={toupper(signature)}"
  )
}
