# ---------------------------------------------------------------------------- #
# ---- Define values if they haven't already been defined by another test ---- #
# ---------------------------------------------------------------------------- #
if (!exists("all_routes")) {
  all_routes <- routes()
}

if (!exists("frankston_route_id")) {
  frankston_route_id <- all_routes %>%
    dplyr::filter(route_name == "Frankston") %>%
    pull(route_id)
}

if (!exists("stops_near_flinders_street")) {
  stops_near_flinders_street <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671
  )
}

if (!exists("flinders_street_stop_id")) {
  flinders_street_stop_id <- stops_near_flinders_street %>%
    filter(stop_name == "Flinders Street Railway Station") %>%
    pull(stop_id)
}
# ---------------------------------------------------------------------------- #

flinders_departures <- departures(
  stop_id = flinders_street_stop_id,
  route_type = "Train"
)

test_that("route_directions result has class \"ptvapi\"", {
  expect_s3_class(flinders_departures, "ptvapi")
})

flinders_street_stop_id <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671
  ) %>%
  filter(stop_name == "Flinders Street Railway Station") %>%
  pull(stop_id)

test_that("Frankston train departs from Flinders Street Station",
  expect_true(frankston_route_id %in% flinders_departures$route_id)
)

test_that("Departures filtered by platform_number", {
  platform_5_6_departures <- departures(
    stop_id = flinders_street_stop_id,
    route_type = "Train",
    platform_numbers = c("5", "6"), # despite the name, they are characters
  )
  expect_gt(nrow(platform_5_6_departures), 0) # must have some results
  expect_equal(
    platform_5_6_departures %>% pull(platform_number) %>% unique %>% sort,
    c("5", "6")
  )
})

test_that("Departures filtered by datetime", {
  flinders_morning_departures <- departures(
    stop_id = flinders_street_stop_id,
    route_type = "Train",
    datetime = morning_test_time
  )
  expect_gt(nrow(flinders_morning_departures), 0) # must have some results
  expect_gt(
    min(flinders_morning_departures$scheduled_departure),
    as.POSIXct(morning_test_time, tz = "Australia/Melbourne")
  )
})

test_that("Departures filtered by datetime and max_results", {
  flinders_afternoon_departures <- departures(
    stop_id = flinders_street_stop_id,
    route_type = "Train",
    datetime = afternoon_test_time,
    max_results = 3
  )
  expect_gt(nrow(flinders_afternoon_departures), 0) # must have some results
  departures_per_route_id <- count(flinders_afternoon_departures, route_id)
  expect_true(all(departures_per_route_id$n == 3))
})
