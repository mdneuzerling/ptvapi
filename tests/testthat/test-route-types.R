if (identical(Sys.getenv("NOT_CRAN"), "true")) {

teardown(pkg.env$route_types <- NULL)

route_types_results <- route_types()

test_that("All 5 route types present", {
  expect_true("Train" %in% route_types_results)
  expect_true("Tram" %in% route_types_results)
  expect_true("Bus" %in% route_types_results)
  expect_true("Vline" %in% route_types_results)
  expect_true("Night Bus" %in% route_types_results)
})

test_that("Route type translation is working", {
  expect_equal(translate_route_type(0), 0)
  expect_error(translate_route_type(99999))

  expect_error(translate_route_type("notaroutetype"))
  train_route_code <- translate_route_type("Train")
  expect_type(train_route_code, "integer")
  expect_equal(translate_route_type("TRAIN"), train_route_code)
  expect_equal(translate_route_type("train"), train_route_code)
})

test_that("Route type description is working", {
  expect_equal(describe_route_type(0), "Train")
  expect_equal(describe_route_type(1), "Tram")
  expect_equal(describe_route_type(2), "Bus")
  expect_equal(describe_route_type(3), "Vline")
  expect_equal(describe_route_type(4), "Night Bus")

  # This test has an alternative use --- if a new route type is added, the tests
  # will start failing, and I can be notified by a CICD pipeline
  expect_error(describe_route_type(5), "Route type 5 doesn't exist")
})

test_that("route types are being cached", {
  cached_route_types()
  pkg_env$route_types <- c("0" = "fish")
  expect_equal(describe_route_type(0), "fish")
})

}
