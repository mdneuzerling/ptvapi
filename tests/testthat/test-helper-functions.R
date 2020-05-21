test_that("Can convert only integer-like things to integers", {
  expect_equal(to_integer(3), 3)
  expect_equal(to_integer(0), 0)
  expect_equal(to_integer(-1), -1)
  expect_equal(to_integer("3"), 3)
  expect_error(
    to_integer(),
    'Error in to_integer() : argument "x" is missing, with no default',
    fixed = TRUE
  )
  expect_error(to_integer("three"), "Cannot convert three to an integer")
  expect_error(to_integer(3.5), "Cannot convert 3.5 to an integer")
  expect_error(to_integer(data.frame()), "Cannot convert list to an integer")
})

test_that("Can convert API-provided timestamp to Melbourne time", {
  raw_time <- "2020-05-09T06:38:47.3194196+00:00" # this is what the API gives
  expect_equal(
    convert_to_melbourne_time(raw_time),
    as.POSIXct("2020-05-09 16:38:47.3194196 AEST", tz = "Australia/Melbourne")
  )
})

test_that("converting NULL datetime to NA", {
  na_datetime <- as.POSIXct(NA)
  attr(na_datetime, "tzone") <- "Australia/Melbourne"
  expect_identical(convert_to_melbourne_time(NULL), na_datetime)
})
