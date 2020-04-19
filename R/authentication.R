#' Return the user ID from the PTV_USER_ID environment variable
#'
#' Currently this package only supports obtaining the user ID either directly
#' as an argument to functions, or from the PTV_USER_ID environment variable.
#' This function will error if the PTV_USER_ID environment variable is not set.
#' A message will also be displayed to the user, explaining how to obtain
#' authentication details from PTV. This message will be displayed only once
#' per session.
#'
#' @return Character. The value of the PTV_USER_ID environment variable.
#'
#' @keywords internal
#'
determine_user_id <- function() {
  user_id <- Sys.getenv("PTV_USER_ID")
  if (identical(user_id, "")) {
    if (!getOption("ptv_api_message_displayed", FALSE)) {
      message(need_api_authentication_details())
    }
    stop("Could not find PTV_USER_ID environment variable.")
  }
  user_id
}

#' Return the user ID from the PTV_API_KEY environment variable
#'
#' Currently this package only supports obtaining the API key either directly
#' as an argument to functions, or from the PTV_API_KEY environment variable.
#' This function will error if the PTV_API_KEY environment variable is not set.
#' A message will also be displayed to the user, explaining how to obtain
#' authentication details from PTV. This message will be displayed only once
#' per session.
#'
#' @return Character. The value of the PTV_API_KEY environment variable.
#'
#' @keywords internal
#'
determine_api_key <- function() {
  api_key <- Sys.getenv("PTV_API_KEY")
  if (identical(api_key, "")) {
    if (!getOption("ptv_api_message_displayed", FALSE)) {
      message(need_api_authentication_details())
    }
    stop("Could not find PTV_API_KEY environment variable.")
  }
  api_key
}

#' Message to display when API details are missing
#'
#' This function outputs a string that guides the user in obtaining a user ID
#' and API key so that they can access the PTV API. It sets an option so that
#' other functions can check if this message has already been displayed, to
#' avoid repetition.
#'
#' @return Character. A message explaining how to obtain and use authentication
#' details for the PTV API.
#'
#' @keywords internal
need_api_authentication_details <- function() {
  options("ptv_api_message_displayed" = TRUE)
  ptv_url <- "https://www.ptv.vic.gov.au/footer/data-and-reporting/datasets/ptv-timetable-api/"
  paste(
    "You need to provide authentication details in order to access the PTV",
    "API. You can pass your user ID and API key directly to the functions in",
    "this package, or you can set environment variables: your user ID as",
    "PTV_USER_ID, and your API key as PTV_API_KEY. You can obtain a user ID",
    "and an API key by contacting PTV. Details are available at", ptv_url
  )
}
