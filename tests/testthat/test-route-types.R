
route_types_results <- route_types()

test_that("All 5 route types present", {
  expect_true("Train" %in% route_types_results)
  expect_true("Tram" %in% route_types_results)
  expect_true("Bus" %in% route_types_results)
  expect_true("Vline" %in% route_types_results)
  expect_true("Night Bus" %in% route_types_results)
})

test_that("route_types_cached is caching", {
  route_types_cached()
  expect_false(is.null(getOption("route_types")))
  expect_identical(route_types_cached(), route_types())
})
