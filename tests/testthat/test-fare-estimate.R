zone12 <- fare_estimate(1, 2)

test_that("fare_estimate result has class \"ptvapi\"", {
  expect_s3_class(zone12, "ptvapi")
})

test_that("basic fare estimate looks right", {
  expect_equal(nrow(zone12), 3)
  expect_equal(nrow(distinct(zone12, passenger_type)), 3)
})

test_that("fare_estimate picks up on the weekday early bird free travel", {
  diff <- 2 - wday(today())
  if (diff < 0) diff <- diff + 7
  monday <- today() + diff # Next Monday, or today, if today is Monday
  early_touch_on <- paste(as.character(monday), "06:00:00")
  early_touch_off <- paste(as.character(monday), "06:00:00")
  early_bird <- fare_estimate(
    min_zone = 1,
    max_zone = 1,
    journey_touch_on = early_touch_on,
    journey_touch_off = early_touch_off
  )
  expect_true(all(early_bird$early_bird))
  expect_false(any(early_bird$weekend_journey))
})

test_that("fare_estimate accepts free tram zone", {
  ftz <- fare_estimate(1, 1, journey_in_free_tram_zone = TRUE)
  expect_true(all(ftz$free_tram_zone))
})
