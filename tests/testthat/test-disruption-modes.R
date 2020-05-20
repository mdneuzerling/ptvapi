if (identical(Sys.getenv("NOT_CRAN"), "true")) {
test_that("Disruption modes contain expected disruptions", {
  retrieved_disruption_modes <- disruption_modes()
  expect_true("metro_train" %in% retrieved_disruption_modes)
  expect_true("metro_tram" %in% retrieved_disruption_modes)
  expect_true("ferry" %in% retrieved_disruption_modes)
  expect_true("taxi" %in% retrieved_disruption_modes)
  expect_true("general" %in% retrieved_disruption_modes)
})
}
