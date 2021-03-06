if (identical(Sys.getenv("NOT_CRAN"), "true")) {
# ---------------------------------------------------------------------------- #
# ---- Define values if they haven't already been defined by another test ---- #
# ---------------------------------------------------------------------------- #
if (!exists("all_routes")) {
  all_routes <- routes()
}

if (!exists("frankston_route_id")) {
  frankston_route_id <- all_routes %>%
    dplyr::filter(route_name == "Frankston") %>%
    pull(route_id)
}
# ---------------------------------------------------------------------------- #

# We need to be careful how we design this test. If there are planned works and
# the trains aren't running, then we can expect that there will be no runs on
# the Frankston line. In this case, the test should trivially pass. The
# assertions within the `runs_on_route` function will still be run, and these
# act as tests of sorts.

runs_on_frankston_route <- runs_on_route(
  route_id = frankston_route_id,
  route_type = "Train"
)

test_that("runs_on_route result has class \"ptvapi\"", {
  expect_s3_class(runs_on_frankston_route, "ptvapi")
})

test_that("Runs on the Frankston route end at stops on the Frankston route", {
  frankston_runs_destinations <- runs_on_frankston_route %>%
    pull(destination_name) %>%
    unique
  frankston_route_train_stops <- stops_on_route(
    route_id = frankston_route_id,
    route_type = "Train"
  ) %>%
    pull(stop_name)
  for (suburb in frankston_runs_destinations) {
    expect_true(any(grepl(!!suburb, frankston_route_train_stops)))
  }
})

run_one <- run_information(run_id = 1)

test_that("run_information() result has class \"ptvapi\"", {
  expect_s3_class(run_one, "ptvapi")
})

test_that("Run 1 exists, and is unique up to route type", {
  expect_gte(nrow(run_one), 1)
  expect_equal(anyDuplicated(run_one$route_type), 0)
})
}
