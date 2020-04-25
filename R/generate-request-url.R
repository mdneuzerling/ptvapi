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
  request_without_signature <- add_parameter(
    glue::glue("/v{version}/{request}"),
    "devid",
    user_id
  )
  signature <- digest::hmac(
    key = api_key,
    object = request_without_signature,
    algo = "sha1"
  )
  signature <- toupper(signature)
  request_without_url <- add_parameter(
    request_without_signature, "signature", signature
  )
  prefix_base_url(request_without_url)
}

add_parameter <- function(request, parameter_name, parameter_value) {
  # Parameters go on the end of the URL, first with ? then with &
  # If a URL already has a ? parameter, we join another one with &
  # Otherwise, we start appending parameters to a URL with ?
  if (is.null(parameter_value)) {
    return(request)
  }
  conjunction <- ifelse(grepl("\\?", request), "&", "?")
  glue::glue("{request}{conjunction}{parameter_name}={parameter_value}")
}

#' Prefix a request with the base URL and version
#'
#' @inheritParams PTVGET
#'
#' @return A complete URL character
#'
#' @keywords internal
prefix_base_url <- function(request, and_version = FALSE) {
  base_url <- "http://timetableapi.ptv.vic.gov.au"
  version <- 3 # This function is untested on other versions

  if (and_version) {
    glue::glue("{base_url}/v{version}/{request}")
  } else {
    glue::glue("{base_url}/{request}")
  }
}
