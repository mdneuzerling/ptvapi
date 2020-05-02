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
