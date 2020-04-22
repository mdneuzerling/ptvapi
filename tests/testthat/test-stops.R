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

