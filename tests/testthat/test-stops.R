if (identical(Sys.getenv("NOT_CRAN"), "true")) {
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

stops_on_frankston_line <- stops_on_route(
  route_id = frankston_route_id,
  route_type = "Train"
)

test_that("stops_on_route result has class \"ptvapi\"", {
  expect_s3_class(stops_on_frankston_line, "ptvapi")
})

test_that("Stop names along a route are unique", {
  expect_equal(anyDuplicated(stops_on_frankston_line$stop_name), 0)
})

test_that("Expected stops on Frankston train line", {
  expect_true("South Yarra Station" %in% stops_on_frankston_line$stop_name)
  expect_true("Cheltenham Station" %in% stops_on_frankston_line$stop_name)
})

# Stops with a direction_id parameters
frankston_directions_on_route <- directions_on_route(frankston_route_id)

test_that("Stop sequence when travelling City to Frankston", {
  frankston_direction_id <- frankston_directions_on_route %>%
    filter(direction_name == "Frankston") %>%
    pull(direction_id)
  city_to_frankston <- stops_on_route(
    route_id = frankston_route_id,
    route_type = "Train",
    direction = frankston_direction_id
  )
  city_to_frankston_ends <- city_to_frankston %>%
    filter(stop_sequence == max(stop_sequence))
  expect_equal(nrow(city_to_frankston_ends), 1)
  expect_equal(city_to_frankston_ends$stop_suburb, "Frankston")
})

test_that("Stop sequence when travelling Frankston to City", {
  city_direction_id <- frankston_directions_on_route %>%
    filter(grepl("City", direction_name)) %>%
    pull(direction_id)
  frankston_to_city <- stops_on_route(
    route_id = frankston_route_id,
    route_type = "Train",
    direction_id = city_direction_id
  )
  frankston_to_city_ends <- frankston_to_city %>%
    filter(stop_sequence == max(stop_sequence))
  expect_equal(nrow(frankston_to_city_ends), 1)
  expect_equal(frankston_to_city_ends$stop_suburb, "Melbourne City")
})

flinders_info <- stop_information(
  flinders_street_stop_id,
  route_type = "Train"
)

test_that("stop_information result has class \"ptvapi\"", {
  expect_s3_class(flinders_info, "ptvapi")
})

test_that("Flinders Street stop_information is complete", {
  expect_lt(mean(is.na(flinders_info)), 0.25) # max prop of NA columns
})

# stops_near_flinders_street defined at top of file
test_that("stops_nearby result has class \"ptvapi\"", {
  expect_s3_class(stops_near_flinders_street, "ptvapi")
})

# flinders_street_stop_id defined at top of file
test_that("Can find Flinders Street Station stop with latitude and longitude", {
  expect_true(length(flinders_street_stop_id) == 1)
})

test_that("stops_nearby filtered by max_distance", {
  max_100 <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    max_distance = 100
  )
  max_1000 <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    max_distance = 1000
  )
  expect_true("Collins St/Queen St" %in% max_1000$stop_name)
  expect_false("Collins St/Queen St" %in% max_100$stop_name)
})

test_that("stops_nearby() can be filtered with multiple route types", {
  # We use Southern Cross because it has a variety of route types
  expect_equal(
    stops_nearby(
      latitude = -37.818229,
      longitude = 144.952404,
      route_types = c(0, 1)
    ) %>%
      pull(route_type) %>%
      unique %>%
      sort,
    c(0, 1)
  )
  expect_equal(
    stops_nearby(
      latitude = -37.818229,
      longitude = 144.952404,
      route_types = c(0, 3)
    ) %>%
      pull(route_type) %>%
      unique %>%
      sort,
    c(0, 3)
  )
})
}
