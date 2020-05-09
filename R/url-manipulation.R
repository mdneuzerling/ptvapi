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
  signature <- sign_request(
    request,
    user_id = user_id,
    api_key = api_key
  )
  unsigned_request <- prefix_base_url_and_version(request)
  unsigned_request <- add_parameter(unsigned_request, "devid", user_id)
  add_parameter(
    unsigned_request, "signature", signature
  )
}

sign_request <- function(request,
                         user_id = determine_user_id(),
                         api_key = determine_api_key()) {
  # The object to be signed is the request URL, including the version, but
  # not the base URL. So if the request is
  # http://timetableapi.ptv.vic.gov.au/v3/path?param=value&devid=1234567
  # then the signature is calculated on /v3/path?param=value&devid=1234567
  # This function will prefix the version and suffix the devid, so in this
  # scenario with would accept an input of simply "path?param=value

  input_error <- paste(
    "This function signs a request without a domain, version number, URL,",
    "or devid/user_id. So if the request URL is",
    "'http://timetableapi.ptv.vic.gov.au/v3/path?param=1&devid=1234567'",
    "then this function will take as input 'path?param=1'."
  )

  # It's very easy to make a mistake when signing a request. Extensive
  # assertions are a big help.
  assertthat::assert_that(
    !grepl("http", request),
    !grepl("vic.gov.au", request),
    !grepl("devid", request),
    !grepl("v3", request),
    msg = input_error
  )
  assertthat::assert_that(
    !grepl("signature", request),
    msg = "This request already has a signature"
  )

  request_without_signature <- add_parameters(
    prefix_version(request),
    devid = user_id
  )
  signature <- digest::hmac(
    key = api_key,
    object = request_without_signature,
    algo = "sha1"
  )
  toupper(signature)
}

#' Prefix a string with the API base URL
#'
#' A trailing `/` will be added to the base URL if the input string does not
#' begin with a `/`.
#'
#' @inheritParams PTVGET
#'
#' @return character of form `{base_url}/{string}`
#'
#' @keywords internal
#'
prefix_base_url <- function(string) {
  base_url <- "http://timetableapi.ptv.vic.gov.au"
  if (substr(string, 1, 1) != "/") base_url <- paste0(base_url, "/")
  glue::glue("{base_url}{string}")
}

#' Prefix a string with the API version
#'
#' A trailing `/` will be added to the version if the input string does not
#' begin with a `/`.
#'
#' @inheritParams PTVGET
#'
#' @return character of form `/{version}/{string}`
#'
#' @keywords internal
#'
prefix_version <- function(string) {
  version <- 3 # This function is untested on other versions
  if (substr(string, 1, 1) != "/") version <- paste0(version, "/")
  glue::glue("/v{version}{string}")
}

#' Prefix a string with the API base URL and version
#'
#' A trailing `/` will be added to the base URL and version if the input string
#' does not begin with a `/`.
#'
#' @inheritParams PTVGET
#'
#' @return character of form `{base_url}/{version}/{string}`
#'
#' @keywords internal
#'
prefix_base_url_and_version <- function(string) {
  prefix_base_url(prefix_version(string))
}

add_parameter <- function(request, parameter_name, parameter_value) {
  # Parameters go on the end of the URL, first with ? then with &
  # If a URL already has a ? parameter, we join another one with &
  # Otherwise, we start appending parameters to a URL with ?
  if (is.null(parameter_value)) {
    return(request)
  }
  if (length(parameter_value) != 1) {
    stop("Parameters must be singletons")
  }
  conjunction <- ifelse(grepl("\\?", request), "&", "?")
  glue::glue("{request}{conjunction}{parameter_name}={parameter_value}")
}

add_parameters <- function(request, ...) {
  dots <- list(...)
  dots_names <- names(dots)
  for (i in seq_along(dots)) {
    dot_name <- dots_names[i]
    dot_value <- dots[[i]]
    if (is.null(dot_name) || dot_name == "") stop("Parameters must be named")
    # add_parameter will error if dot_value if not a singletons, and will
    # return the request unaltered if dot_value is NULL
    request <- add_parameter(request, dot_name, dot_value)
  }
  request
}
