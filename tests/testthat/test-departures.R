flinders_departures <- departures(
  stop_id = flinders_street_stop_id,
  route_type = train_route_type
)

test_that("route_directions result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(flinders_departures))
})

test_that("Frankston train departs from Flinders Street Station",
  expect_true(frankston_route_id %in% flinders_departures$route_id)
)
