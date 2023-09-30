if (identical(Sys.getenv("NOT_CRAN"), "true")) {

all_outlets <- outlets()
test_that("outlets result has class \"ptvapi\"", {
  expect_s3_class(all_outlets, "ptvapi")
})

test_that("We can find a 7-Eleven", {
  expect_true(
    any(grepl("7-Eleven", all_outlets$outlet_business))
  )
})

outlets_near_flinders <- outlets_nearby(
  latitude = -37.8183,
  longitude = 144.9671
)

test_that("outlets_nearby result has class \"ptvapi\"", {
  expect_s3_class(outlets_near_flinders, "ptvapi")
})

test_that("There's a 7-Eleven near Flinders Street", {
  expect_true(
    any(grepl("7-Eleven", outlets_near_flinders$outlet_business))
  )
})

test_that("outlets_nearby filtered by max_results", {
  one_outlet <- outlets_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    max_results = 1
  )
  expect_equal(nrow(one_outlet), 1)
})

test_that("outlets_nearby filtered by max_distance", {
  max_100 <- outlets_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    max_distance = 100
  )
  max_1000 <- outlets_nearby(
    latitude = -37.8183,
    longitude = 144.9671,
    max_distance = 1000
  )
  expect_gt(
    nrow(max_1000),
    nrow(max_100)
  )
})
}
