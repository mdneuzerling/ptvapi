runs_on_route <- function(route_id,
                          route_type = NULL,
                          user_id = determine_user_id(),
                          api_key = determine_api_key()) {
  request <- glue::glue("runs/route/{route_id}")
  if (!is.null(route_type)) {
    route_type <- translate_route_type(route_type)
    request <- glue::glue("{request}/route_type/route_type")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  content$runs
}

#' Convert a single run to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `runs_on_route` and `run_information` functions.
#'
#' @param route A run, as a list, returned by the `runs` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item TODO
#' }
#'
#' @keywords internal
#'
run_to_tibble <- function(run) {
  invisible(run)
}
