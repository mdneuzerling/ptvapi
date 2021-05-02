if (identical(Sys.getenv("NOT_CRAN"), "true")) {

pats <- patterns(
  run_ref = "1",
  route_type = "Train",
  departs = morning_test_time
)

test_that("patterns returns list with right names", {
  expect_true(is.list(pats))
  expect_equal(
    names(pats),
    c("departures", "stops", "routes", "runs", "directions", "disruptions")
  )
})
}
