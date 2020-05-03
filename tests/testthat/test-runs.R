# We need to be careful how we design this test. If there are planned works and
# the trains aren't running, then we can expect that there will be no runs on
# the Frankston line. In this case, the test should trivially pass. The
# assertions within the `runs_on_route()` function will still be run, and these
# act as tests of sorts.

runs_on_frankston_route <- runs_on_route(
  route_id = frankston_route_id,
  route_type = train_route_type
)

test_that("runs_on_route result has class \"ptvapi\"", {
  expect_true("ptvapi" %in% class(runs_on_frankston_route))
})

test_that("Runs on the Frankston route end at stops on the Frankston route", {
  frankston_runs_destinations <- runs_on_frankston_route %>%
    pull(destination_name) %>%
    unique
  frankston_route_train_stops <- stops_on_route(
    route_id = frankston_route_id,
    route_type = train_route_type
  ) %>%
    pull(stop_name)
  for (suburb in frankston_runs_destinations) {
    expect_true(any(grepl(!!suburb, frankston_route_train_stops)))
  }
})


# run_one is defined and tested in the helper.R test file
