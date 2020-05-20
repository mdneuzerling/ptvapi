if (identical(Sys.getenv("NOT_CRAN"), "true")) {

pats <- patterns(
  run_id = 1,
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

# test_that("patterns departures filtered by datetime", {
#   expect_gt(nrow(pats$departures), 0) # must have some results
#   expect_gt(
#     min(pats$departures$scheduled_departure),
#     as.POSIXct(morning_test_time, tz = "Australia/Melbourne")
#   )
# })
}
