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

test_that("Frankston train stops can be given a direction", {
  frankston_route_directions <- route_directions(frankston_route_id)
  frankston_direction_id <- frankston_route_directions %>%
    filter(direction_name == "Frankston") %>%
    pull(direction_id)
  frankston_stops_with_directions <- stops_on_route(
    route_id = frankston_route_id,
    route_type = train_route_type,
    direction = frankston_direction_id
  )
  frankston_train_ends <- frankston_stops_with_directions %>%
    filter(stop_sequence == max(stop_sequence))
  expect_equal(nrow(frankston_train_ends), 1)
  expect_equal(frankston_train_ends$stop_suburb, "Frankston")
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

