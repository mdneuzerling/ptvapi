#' Stop information (metropolitan and V/Line stations only).
#'
#' This function can be used when integer disruption ID is already known. This
#' can be searched for with either the `disruptions_on_route` or
#' `disruptions_at_stop` functions.
#'
#' @section Swagger documentation:
#'   \url{http://timetableapi.ptv.vic.gov.au/swagger/ui/index#/Stops}
#'
#' @param disruption_id Integer disruption ID.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
disruption_information <- function(disruption_id,
                                   user_id = determine_user_id(),
                                   api_key = determine_api_key()) {
  disruption_id <- to_integer(disruption_id)
  request <- glue::glue("disruptions/{disruption_id}")
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruption", "status")
  )

  disruption_to_tibble(content$disruption)
}

#' Disruptions on a given route
#'
#' @inheritParams route_directions
#' @param stop_id Integer. Optionally filter results to a specific stop ID.
#'   These can be searched for with the `stops_on_route()` and `stops_nearby()`
#'   functions.
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
#'
disruptions_on_route <- function(route_id,
                                 stop_id = NULL,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {
  route_id <- to_integer(route_id)
  request <- glue::glue("disruptions/route/{route_id}")
  if (!is.null(stop_id)) {
    stop_id <- to_integer(stop_id)
    request <- glue::glue("{request}/stop/{stop_id}")
  }
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  all_disruptions_to_tibble(content$disruptions)
}

#' Disruptions at a given stop
#'
#' @inheritParams stop_information
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
#'
disruptions_at_stop <- function(stop_id,
                                user_id = determine_user_id(),
                                api_key = determine_api_key()) {
  stop_id <- to_integer(stop_id)
  request <- glue::glue("disruptions/stop/{stop_id}")
  response <- PTVGET(request = request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  all_disruptions_to_tibble(content$disruptions)
}

#' Retrieve a translation from description mode number to description mode name.
#'
#' Disruption mode types (eg. "metro_train", "metro_tram", "school_bus", "taxi")
#' have corresponding integer IDs. This function retrieves a named vector in
#' which the values are the disruption mode descriptions, and the names of the
#' vector are the description mode numbers. Note that disruption mode names are
#' in snake case, that is, all lower case with underscores between words.
#'
#' @inheritParams PTVGET
#'
#' @return A named vector in which the values are the disruption mode
#'   descriptions, and the names of the vector are the description mode numbers.
#' @export
#'
#' @examples \dontrun{disruption_modes()}
disruption_modes <- function(user_id = determine_user_id(),
                             api_key = determine_api_key()) {
  request <- "disruptions/modes"
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruption_modes", "status")
  )

  purrr::reduce(
    purrr::map(
      content$disruption_modes,
      function(x) {
        dismode = x$disruption_mode_name
        names(dismode) = x$disruption_mode
        dismode
      }
    ),
    c
  )
}

#' Convert the contents of a disruptions API call to a single tibble.
#'
#' Disruptions API responses contain an element for every service type, eg.
#' metro train, taxis, Skybus. Normally we would map-reduce the content of an
#' API call with a function analogous to `disruption_to_tibble`. But because of
#' the extra layer of nesting in the response, we have to map-reduce the service
#' types first.
#'
#' Note that we return an empty tibble if there are no disruptions, so that
#' this situation is omitted.
#'
#' @param disruptions_content The raw disruptions content returned by the
#'   `disruptions` API call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item `service_type`
#' \item `disruption_id`
#' \item `title`
#' \item `url`
#' \item `description`
#' \item `disruption_status`
#' \item `disruption_type`
#' \item `published_on`
#' \item `last_updated`
#' \item `from_date`
#' \item `to_date`
#' \item `routes`
#' \item `stops`
#' \item `colour`
#' \item `display_on_board`
#' \item `display_status`
#' }
#'
#' @keywords internal
#'
all_disruptions_to_tibble <- function(disruptions_content) {
  dis <- purrr::reduce(
    purrr::map(seq_along(disruptions_content), function(x) {
      service_type <- names(disruptions_content)[x]
      dis <- disruptions_content[[x]]
      if (length(dis) == 0) {
        tibble::tibble()
      } else {
        dis_tibble <- map_and_rbind(dis, disruption_to_tibble)
        dis_tibble$service_type <- service_type
        dis_tibble
      }
    }),
    rbind
  )

  # Base R method of moving service column to the front
  dis[,c("service_type", colnames(dis)[colnames(dis) != "service_type"])]
}

#' Convert a single disruptions to a tibble
#'
#' This function is designed to parse the content returned by the interior
#' steps of the `disruptions_on_route` and `disruptions_at_stop` functions.
#'
#' @param disruption A disruption, as a list, returned by the `disruptions` API
#'   call.
#'
#' @return A tibble with the following columns: \itemize{
#' \item `disruption_id`
#' \item `title`
#' \item `url`
#' \item `description`
#' \item `disruption_status`
#' \item `disruption_type`
#' \item `published_on`
#' \item `last_updated`
#' \item `from_date`
#' \item `to_date`
#' \item `routes`
#' \item `stops`
#' \item `colour`
#' \item `display_on_board`
#' \item `display_status`
#' }
#' @export
#'
disruption_to_tibble <- function(disruption) {
  tibble::tibble(
    disruption_id = disruption$disruption_id,
    title = disruption$title,
    url = disruption$url,
    description = disruption$description,
    disruption_status = disruption$disruption_status,
    disruption_type = disruption$disruption_type,
    published_on = convert_to_melbourne_time(disruption$published_on),
    last_updated = convert_to_melbourne_time(disruption$last_updated),
    from_date = convert_to_melbourne_time(disruption$from_date),
    to_date = convert_to_melbourne_time(disruption$to_date),
    routes = map_and_rbind(disruption$routes, route_to_tibble),
    stops = list(disruption$stops),
    colour = disruption$colour,
    display_on_board = disruption$display_on_board,
    display_status = disruption$display_status
  )
}
