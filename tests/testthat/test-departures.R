test_that("Frankston train departs from Flinders Street Station", {
  flinders_street_station <- stops_nearby(
    latitude = -37.8183,
    longitude = 144.9671
  ) %>%
    filter(trimws(stop_name) == "Flinders Street Railway Station") %>%
    pull(stop_id)
  flinders_departures <- departures(
      stop_id = flinders_street_station,
      route_type = train_route_type
  )
  expect_true(frankston_route_id %in% flinders_departures$route_id)
})
