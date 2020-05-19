#' Retrieve information on a particular disruption.
#'
#' This function can be used when the integer disruption ID is already known.
#' This can be searched for with either `disruptions()`,
#' `disruptions_on_route()`, or `disruptions_at_stop()` functions.
#'
#' @param disruption_id Integer.
#' @inheritParams translate_route_type
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
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

  parsed <- disruption_to_tibble(content$disruption)
  new_ptvapi_tibble(response, parsed)
}


#' Retrieve information for all disruptions.
#'
#' @param route_types Integer or character vector. Optionally filter by a vector
#'   of route types. A route type can be provided either as a non-negative
#'   integer code, or as a character: "Tram", "Train", "Bus", "Vline" or "Night
#'   Bus". Character inputs are not case-sensitive. Use the `route_types`
#'   function to extract a vector of all route types. The filter is applied to
#'   the disruption mode, rather than the routes that are affected by the
#'   disruption. For example, filtering by the "train" route type will restrict
#'   the disruptions returned to those with a mode corresponding to
#'   "metro_train".
#' @param disruption_modes Integer vector. Optionally filter by disruption
#'   modes. For a full list of modes and their corresponding descriptions, use
#'   the `description_modes()` function.
#' @param disruption_status Character. Can be used to filter to either "current"
#'   or "planned" disruptions. Defaults to NULL, in which case no filter is
#'   applied.
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
#'
disruptions <- function(route_types = NULL,
                        disruption_modes = NULL,
                        disruption_status = NULL,
                        user_id = determine_user_id(),
                        api_key = determine_api_key()) {

  if (!is.null(route_types)) {
    route_types <- purrr::map_int(route_types, translate_route_type)
  }

  if (!is.null(disruption_modes)) {
    disruption_modes <- purrr::map_int(disruption_modes, to_integer)
  }

  if (!is.null(disruption_status)) {
    assertthat::assert_that(
      disruption_status %in% c("current", "planned"),
      msg = paste("disruption_status, if provided, must be either",
                  "\"current\" or \"planned\"")
    )
  }

  request <- add_parameters(
    "disruptions",
    route_types = route_types,
    disruption_status = disruption_status,
    disruption_modes = disruption_modes
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  parsed <- all_disruptions_to_tibble(content$disruptions)
  new_ptvapi_tibble(response, parsed)
}

#' Disruptions on a given route
#'
#' @inheritParams directions_on_route
#' @param stop_id Integer. Optionally filter results to a specific stop ID.
#'   These can be searched for with the `stops_on_route()` and `stops_nearby()`
#'   functions.
#' @inheritParams disruptions
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
#'
disruptions_on_route <- function(route_id,
                                 stop_id = NULL,
                                 disruption_status = NULL,
                                 user_id = determine_user_id(),
                                 api_key = determine_api_key()) {
  # Covers the following API calls:
  # get /v3/disruptions/route/{route_id}
  # get /v3/disruptions/route/{route_id}/stop/{stop_id}
  route_id <- to_integer(route_id)
  request <- glue::glue("disruptions/route/{route_id}")
  if (!is.null(stop_id)) {
    stop_id <- to_integer(stop_id)
    request <- glue::glue("{request}/stop/{stop_id}")
  }
  if (!is.null(disruption_status)) {
    assertthat::assert_that(
      disruption_status %in% c("current", "planned"),
      msg = paste("disruption_status, if provided, must be either",
                  "\"current\" or \"planned\"")
    )
  }
  request <- add_parameters(
    request,
    disruption_status = disruption_status
  )
  response <- PTVGET(request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  parsed <- all_disruptions_to_tibble(content$disruptions)
  new_ptvapi_tibble(response, parsed)
}

#' Disruptions at a given stop
#'
#' @inheritParams stop_information
#' @inheritParams disruptions
#' @inheritParams PTVGET
#'
#' @inherit all_disruptions_to_tibble return
#'
#' @export
#'
disruptions_at_stop <- function(stop_id,
                                disruption_status = NULL,
                                user_id = determine_user_id(),
                                api_key = determine_api_key()) {
  stop_id <- to_integer(stop_id)
  if (!is.null(disruption_status)) {
    assertthat::assert_that(
      disruption_status %in% c("current", "planned"),
      msg = paste("disruption_status, if provided, must be either",
                  "\"current\" or \"planned\"")
    )
  }
  request <- add_parameters(
    glue::glue("disruptions/stop/{stop_id}"),
    disruption_status = disruption_status
  )
  response <- PTVGET(request = request, user_id = user_id, api_key = api_key)
  content <- response$content
  assert_correct_attributes(
    names(content),
    c("disruptions", "status")
  )

  parsed <- all_disruptions_to_tibble(content$disruptions)
  new_ptvapi_tibble(response, parsed)
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
        dismode <- x$disruption_mode_name
        names(dismode) <- x$disruption_mode
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
#' \item `disruption_mode`
#' \item `disruption_mode_description`
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
  dis_modes <- disruption_modes()
  dis <- purrr::reduce(
    purrr::map(seq_along(disruptions_content), function(x) {
      disruption_mode_description <- names(disruptions_content)[x]
      disruption_mode <- names(
        dis_modes[dis_modes == disruption_mode_description]
      )
      dis <- disruptions_content[[x]]
      if (length(dis) == 0) {
        tibble::tibble()
      } else {
        dis_tibble <- map_and_rbind(dis, disruption_to_tibble)
        dis_tibble$disruption_mode <- disruption_mode
        dis_tibble$disruption_mode_description <- disruption_mode_description
        dis_tibble
      }
    }),
    rbind
  )

  if (nrow(dis) == 0) {
    return(dis)
  }

  # Base R method of moving service column to the front
  dis <- dis[, c("disruption_mode_description",
                 colnames(dis)[colnames(dis) != "disruption_mode_description"])]
  dis <- dis[, c("disruption_mode",
                 colnames(dis)[colnames(dis) != "disruption_mode"])]
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
#'
#' @keywords internal
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
