all_outlets <- outlets()
test_that("outlets result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(all_outlets))
})

test_that("We can find the Southern Cross Station outlet", {
  southern_cross_outlet <- all_outlets %>%
    filter(outlet_business == "Southern Cross Station")
  expect_equal(nrow(southern_cross_outlet), 1)
})

outlets_near_flinders <- outlets_nearby(
  latitude = -37.8183,
  longitude = 144.9671
)
test_that("outlets_nearby result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(outlets_near_flinders))
})
test_that("There's a 7-Eleven near Flinders Street", {
  expect_true(
    any(grepl("7-Eleven", outlets_near_flinders$outlet_business))
  )
})
