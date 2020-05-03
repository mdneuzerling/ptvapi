# These functions set up some common values which will be used in multiple
# tests. Even though this is a helper file, we perform some tests here because
# it makes sense to do so immediately after the results are retrieved.

library(dplyr, quietly = TRUE)

all_routes <- routes()
test_that("routes() result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(all_routes))
})

frankston_route_id <- dplyr::filter(
  all_routes,
  route_name == "Frankston"
)$route_id
expect_true(length(frankston_route_id) == 1)

train_route_type <- translate_route_type("Train")

stops_near_flinders_street <-stops_nearby(
  latitude = -37.8183,
  longitude = 144.9671
)
test_that("stops_nearby result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(stops_near_flinders_street))
})

flinders_street_stop_id <- stops_near_flinders_street %>%
  filter(stop_name == "Flinders Street Railway Station") %>%
  pull(stop_id)
test_that("Can find Flinders Street Station stop with latitude and longitude", {
  expect_true(length(flinders_street_stop_id) == 1)
})

run_one <- run_information(run_id = 1)
test_that("run_information() result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(run_one))
})
test_that("Run 1 exists, and is unique up to route type", {
  expect_gte(nrow(run_one), 1)
  expect_equal(anyDuplicated(run_one$route_type), 0)
})
