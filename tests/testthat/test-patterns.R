pats <- patterns(
  run_id = 1,
  route_type = "Train",
  datetime = "2020-03-01T16:41:50"
)

test_that("patterns returns list with right names", {
  expect_true(is.list(pats))
  expect_equal(
    names(pats),
    c("departures", "stops", "routes", "runs", "directions", "disruptions")
  )
})
