stops_on_frankston_line <- stops_on_route(
  route_id = frankston_route_id,
  route_type = train_route_type
)

test_that("Stop names along a route are unique", {
  expect_equal(anyDuplicated(stops_on_frankston_line$stop_name), 0)
})

test_that("Expected stops on Frankston train line", {
  expect_true("South Yarra Station" %in% stops_on_frankston_line$stop_name)
  expect_true("Cheltenham Station" %in% stops_on_frankston_line$stop_name)
})

# Stops with a direction_id parameters
frankston_route_directions <- route_directions(frankston_route_id)

test_that("Stop sequence when travelling City to Frankston", {
  frankston_direction_id <- frankston_route_directions %>%
    filter(direction_name == "Frankston") %>%
    pull(direction_id)
  city_to_frankston <- stops_on_route(
    route_id = frankston_route_id,
    route_type = train_route_type,
    direction = frankston_direction_id
  )
  city_to_frankston_ends <- city_to_frankston %>%
    filter(stop_sequence == max(stop_sequence))
  expect_equal(nrow(city_to_frankston_ends), 1)
  expect_equal(city_to_frankston_ends$stop_suburb, "Frankston")
})

test_that("Stop sequence when travelling Frankston to City", {
  city_direction_id <- frankston_route_directions %>%
    filter(grepl("City", direction_name)) %>%
    pull(direction_id)
  frankston_to_city <- stops_on_route(
    route_id = frankston_route_id,
    route_type = train_route_type,
    direction = city_direction_id
  )
  frankston_to_city_ends <- frankston_to_city %>%
    filter(stop_sequence == max(stop_sequence))
  expect_equal(nrow(frankston_to_city_ends), 1)
  expect_equal(frankston_to_city_ends$stop_suburb, "Melbourne City")
})

cheltenham_stop_id <- stops_on_frankston_line %>%
  filter(stop_name == "Cheltenham Station") %>%
  pull(stop_id)

test_that("Cheltenham Station can be identified with stop_information", {
  cheltenham_station_information <- stop_information(
    stop_id = cheltenham_stop_id,
    route_type = train_route_type
  )
  # The API seems to cut the Station off when retrieving an individual stop
  expect_true(grepl("Cheltenham", cheltenham_station_information$stop_name))
})

# We get whitespace after stop names
test_that("Can find Flinders Street with latitude and longitude", {
  stops_near_flinders_street <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671
  ) %>%
    pull(stop_name) %>%
    trimws()
  expect_true("Flinders Street Railway Station" %in% stops_near_flinders_street)
})
