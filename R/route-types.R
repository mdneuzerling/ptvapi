#' Retrieve a translation from route type number to name
#'
#' Route types (tram, train, etc.) are provided to the PTV API by number. This
#' function retrieves a named vector in which the values are the route type
#' descriptions, and the names of the vector are the route type numbers. Note
#' that "Night Bus" is a separate route type.
#'
#' @inheritParams PTVGET
#'
#' @return A named vector in which the values are the route type descriptions,
#' and the names of the vector are the route type numbers.
#' @export
#'
#' @examples \dontrun{route_types()}
route_types <- function(user_id = determine_user_id(),
                        api_key = determine_api_key()) {
  response <- PTVGET(
    request = "route_types",
    user_id = user_id,
    api_key = api_key
  )
  content_route_types <- response$content$route_types

  route_type_names <- purrr::map_chr(
    content_route_types,
    function(x) x$route_type_name
  )
  route_type_numbers <- purrr::map_int(
    content_route_types,
    function(x) x$route_type
  )
  names(route_type_names) <- route_type_numbers
  route_type_names
}

#' Retrieve a translation from route type number to name, cached if possible
#'
#' This function is equivalent to `route_types()`, but will attempt to use a
#' cached copy of the results stored in options if it is available. If not
#' available, `route_types()` will be called and the results cached. Since
#' route types are commonly used in other API calls, this caching reduces the
#' number of times the API is called.
#'
#' @inheritParams PTVGET
#'
#' @return A named vector in which the values are the route type descriptions,
#' and the names of the vector are the route type numbers.
#'
#' @keywords internal
#'
route_types_cached <- function(user_id = determine_user_id(),
                               api_key = determine_api_key()) {
  if (is.null(getOption("route_types"))) {
    options("route_types" = route_types(user_id = user_id, api_key = api_key))
  }
  getOption("route_types")
}

#' Translate a route type input into a numerical route type
#'
#' Many API calls require a route type (eg. "Tram" or "Train"). These must be
#' provided as integers, which are translated to route type descriptions with
#'
#' @param route_type A route type which can be provided either as a non-negative
#'   integer code, or as a character: "Tram", "Train", "Bus", "Vline" or "Night
#'   Bus". Character inputs are not case-sensitive. Use the `route_types`
#'   function to extract a vector of all route types.
#'
#' @return An integer route type code
#'
#' @keywords internal
#'
translate_route_type <- function(route_type) {

  route_type_vector <- route_types_cached()

  if (is.numeric(route_type)) {
    if (as.character(route_type) %in% names(route_type_vector)) {
      translation <- route_type
    } else {
      stop(
        route_type,
        " is not a valid route type code. Valid codes are ",
        paste(names(route_type_vector), collapse = ", ")
      )
    }
  } else if (is.character(route_type)) {
    if (toupper(route_type) %in% toupper(route_type_vector)) {
      translation <- names(
        route_type_vector[
          which(toupper(route_type_vector) == toupper(route_type))]
      )
    } else {
      stop(
        route_type,
        " is not a valid route type description. Valid descriptions are ",
        paste(route_type_vector, collapse = ", ")
      )
    }
  } else {
    stop(
      "Couldn't determine route type code. Route types can be provided either ",
      "as a non-negative integer code, or as a character: \"Tram\", ",
      "\"Train\" \"Bus\", \"Vline\" or \"Night Bus\". Character inputs are ",
      "not case-sensitive."
    )
  }

  as.integer(translation)
}
