# Note that in all of these test cases we order the departures by departure
# time: by estimated_departure (if defined), otherwise by scheduled departure.
#
# In the event of there being a tie, ie. the next 3 departures for a route are
# all at the same time and we have max_results = 2, then the tie is arbitrarily
# broken. This shouldn't be an issue because it would be highly unlikely that
# there are two services of the same route departing at the same time from the
# same stop. Maybe if they have different direction IDs?

test_departures <- tibble::tribble(
  ~route_id, ~scheduled_departure, ~estimated_departure,
  1, "2020-06-01 12:04:00", NA,
  1, "2020-06-01 12:03:00", NA,
  1, "2020-06-01 12:02:00", NA,
  1, "2020-06-01 12:01:00", NA,
  2, "2020-06-01 12:04:00", NA,
  2, "2020-06-01 12:03:00", NA,
  2, "2020-06-01 12:02:00", "2020-06-01 12:05:00",
  2, "2020-06-01 12:01:00", NA,
  3, "2020-06-01 12:01:00", NA,
  3, "2020-06-01 12:01:00", NA,
  3, "2020-06-01 12:01:00", NA,
  3, "2020-06-01 12:02:00", NA
) %>%
  mutate_at(
    vars(contains("departure")),
    ~as.POSIXct(.x, tz = "Australia/Melbourne")
  )

test_that("Can filter departures with max_results", {
  expect_equal(
    filter_departures(test_departures, max_results = 2),
    tibble::tribble(
      ~route_id, ~scheduled_departure, ~estimated_departure,
      1, "2020-06-01 12:01:00", NA,
      2, "2020-06-01 12:01:00", NA,
      3, "2020-06-01 12:01:00", NA,
      3, "2020-06-01 12:01:00", NA,
      1, "2020-06-01 12:02:00", NA,
      2, "2020-06-01 12:03:00", NA
    ) %>%
      mutate_at(
        vars(contains("departure")),
        ~as.POSIXct(.x, tz = "Australia/Melbourne")
      )
  )
})

test_that("Can filter departures with datetime", {
  expect_equal(
    filter_departures(
      test_departures,
      datetime = as.POSIXct("2020-06-01 12:03:00", tz = "Australia/Melbourne")
    ),
    tibble::tribble(
      ~route_id, ~scheduled_departure, ~estimated_departure,
      1, "2020-06-01 12:03:00", NA,
      2, "2020-06-01 12:03:00", NA,
      1, "2020-06-01 12:04:00", NA,
      2, "2020-06-01 12:04:00", NA,
      2, "2020-06-01 12:02:00", "2020-06-01 12:05:00"
    ) %>%
      mutate_at(
        vars(contains("departure")),
        ~as.POSIXct(.x, tz = "Australia/Melbourne")
      )
  )
})

test_that("Can filter departures with route_id", {
  expect_equal(
    filter_departures(
      test_departures,
      route_id = 2
    ),
    tibble::tribble(
      ~route_id, ~scheduled_departure, ~estimated_departure,
      2, "2020-06-01 12:01:00", NA,
      2, "2020-06-01 12:03:00", NA,
      2, "2020-06-01 12:04:00", NA,
      2, "2020-06-01 12:02:00", "2020-06-01 12:05:00"
    ) %>%
      mutate_at(
        vars(contains("departure")),
        ~as.POSIXct(.x, tz = "Australia/Melbourne")
      )
  )
})

test_that("Can filter departures with datetime and max_results", {
  expect_equal(
    filter_departures(
      test_departures,
      datetime = as.POSIXct("2020-06-01 12:03:00", tz = "Australia/Melbourne"),
      max_results = 2
    ),
    tibble::tribble(
      ~route_id, ~scheduled_departure, ~estimated_departure,
      1, "2020-06-01 12:03:00", NA,
      2, "2020-06-01 12:03:00", NA,
      1, "2020-06-01 12:04:00", NA,
      2, "2020-06-01 12:04:00", NA,
    ) %>%
      mutate_at(
        vars(contains("departure")),
        ~as.POSIXct(.x, tz = "Australia/Melbourne")
      )
  )
})
