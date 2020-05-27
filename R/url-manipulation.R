#' Generate a URL with devid and signature
#'
#' @param request Character. A path without base URL or version, such as
#' "routes" or "stop/1071".
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
  add_parameters(unsigned_request, devid = user_id, signature = signature)
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
    "Cannot sign", request, ":",
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

prefix_base_url <- function(string) {
  base_url <- "http://timetableapi.ptv.vic.gov.au"
  if (substr(string, 1, 1) != "/") base_url <- paste0(base_url, "/")
  glue::glue("{base_url}{string}")
}

prefix_version <- function(string) {
  version <- 3 # This function is untested on other versions
  if (substr(string, 1, 1) != "/") version <- paste0(version, "/")
  glue::glue("/v{version}{string}")
}

prefix_base_url_and_version <- function(string) {
  prefix_base_url(prefix_version(string))
}

#' Convert an input to a form that can be used in a URL.
#'
#' Before a character can be used as part of a html, spaces must be converted to
#' "%20".
#'
#' @param input Character.
#'
#' @return Character.
#'
#' @keywords internal
#'
make_url_friendly <- function(input) {
  gsub(" ", "%20", input)
}

#' Suffix a parameter to a HTML request
#'
#' Parameters are suffixed to a URL, like so:
#' "request?para1=value1&para2=value2". The first parameter is suffixed with "?"
#' and all others after that "&". This function will determine the correct
#' suffix based on the presence of "&" in the request.
#'
#' @details There is no standardised way to combine multiple values for a
#' parameter. You should see how your API expects multiple values to be provided
#' to the same parameter. This function allows for the following strategies. If
#' any other value is provided, then the values will be concatenated and
#' separated with the provided value.
#' \itemize{
#'   \item "repeat_name" (default). The values of the parameter are repeated
#'     with the parameter name. For example, `("request", "para", c(1, 2))` will
#'     return "request?para=1&para=2".
#'   \item "with_commas". The values of the parameter are concatenated and
#'     separated with commas. For example, `("request", "para", c(1, 2))` will
#'     return "request?para=1,2".
#'   \item "with_commas". The values of the parameter are concatenated and
#'     separated with the ASCII keycode in hexadecimal for a comma ("%2C"). For
#'     example, `("request", "para", c(1, 2))` will return "request?para=1%2C2".
#' }
#'
#' @param request Character. The base URL or request which will be suffixed with
#' the parameter.
#' @param parameter_name Character. Name of parameter to suffix.
#' @param parameter_value Character, or a value that can be coerced to a
#' character. The value of the parameter to suffix.
#' @param .combine How to combine parameters with multiple values. One
#'   of "repeat_name", "with_commas", "with_hex_commas". See Details.
#'
#' @return Character. The request with suffixed parameters.
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' ptvapi:::add_parameter("www.example.com", "animal", "crocodile")
#' ptvapi:::add_parameter(
#'   "www.example.com",
#'   "numbers",
#'   c(1, 2, 3),
#'   .combine = "repeat_names"
#' )}
#'
add_parameter <- function(request,
                          parameter_name,
                          parameter_value,
                          .combine = "repeat_name") {
  if (is.null(parameter_value) || length(parameter_value) == 0) {
    return(request)
  }

  if (length(parameter_value) == 1) {
    conjunction <- ifelse(grepl("\\?", request), "&", "?")
    if (is.logical(parameter_value)) {
      parameter_value <- ifelse(parameter_value, "true", "false")
    }
    glue::glue("{request}{conjunction}{parameter_name}={parameter_value}")
  } else if (.combine == "repeat_name") {
    for (value in parameter_value) {
      request <- add_parameter(
        request,
        parameter_name,
        value,
        .combine = .combine
      )
    }
    request
  } else if (.combine == "with_commas") {
    add_parameter(request, parameter_name, parameter_value, .combine = ",")
  } else if (.combine == "with_hex_commas") {
    add_parameter(request, parameter_name, parameter_value, .combine = "%2C")
  } else {
    combined_value <- paste(parameter_value, collapse = .combine)
    add_parameter(request, parameter_name, combined_value)
  }
}


#' Suffix one or many parameters to a HTML request
#'
#' @inherit add_parameter description
#' @inherit add_parameter details
#'
#' @inheritParams add_parameter
#' @param ... The parameters to be suffixed, with name/value pairs provided as
#'   arguments.
#'
#' @inherit add_parameter return
#'
#' @keywords internal
#'
#' @examples \dontrun{
#' ptvapi:::add_parameters("www.example.com", animal = crocodile)
#' ptvapi:::add_parameters(
#'   "www.example.com",
#'   animal = crocodile,
#'   food = "cherries"
#' )
#' ptvapi:::add_parameters(
#'   "www.example.com",
#'   animal = crocodile,
#'   numbers = c(1, 2, 3),
#'   .combine = "repeat_names"
#' )}
#'
add_parameters <- function(request, ..., .combine = "repeat_name") {
  dots <- list(...)
  dots_names <- names(dots)
  for (i in seq_along(dots)) {
    dot_name <- dots_names[i]
    dot_value <- dots[[i]]
    if (is.null(dot_name) || dot_name == "") stop("Parameters must be named")
    # add_parameter will error if dot_value if not a singletons, and will
    # return the request unaltered if dot_value is NULL
    request <- add_parameter(request, dot_name, dot_value, .combine = .combine)
  }
  request
}
