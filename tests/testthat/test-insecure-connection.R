if (identical(Sys.getenv("NOT_CRAN"), "true")) {

current_option <- getOption("use_insecure_ptv_connection")
teardown(options(use_insecure_ptv_connection = current_option))

test_that("secure connection uses https", {
  secure_request <- run_information(1)
  expect_true(grepl("https", attr(secure_request, "request")))
})

test_that("insecure connection doesn't use https", {
  options(use_insecure_ptv_connection = TRUE)
  insecure_request <- run_information(1)
  expect_false(grepl("https", attr(insecure_request, "request")))
})

}
