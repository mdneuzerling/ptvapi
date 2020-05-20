if (identical(Sys.getenv("NOT_CRAN"), "true")) {

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

test_that("Route translation are working", {
  expect_equal(translate_route_type(0), 0)
  expect_error(translate_route_type(99999))

  expect_error(translate_route_type("notaroutetype"))
  train_route_code <- translate_route_type("Train")
  expect_type(train_route_code, "integer")
  expect_equal(translate_route_type("TRAIN"), train_route_code)
  expect_equal(translate_route_type("train"), train_route_code)
})

}
