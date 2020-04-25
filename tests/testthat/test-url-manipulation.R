fake_user_id <- 1234567
fake_api_key <- "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"

test_that("Prefixing API version", {
  expect_equal(
    prefix_version("routes"),
    "/v3/routes"
  )
  expect_equal(
    prefix_version("/routes"),
    "/v3/routes"
  )
})

test_that("Prefixing base URL", {
  expect_equal(
    prefix_base_url("v3/routes"),
    "http://timetableapi.ptv.vic.gov.au/v3/routes"
  )
  expect_equal(
    prefix_base_url("/v3/routes"),
    "http://timetableapi.ptv.vic.gov.au/v3/routes"
  )
})

test_that("Can correctly suffix a parameter", {
  expect_equal(
    add_parameter("routes", "devid", fake_user_id),
    "routes?devid=1234567"
  )
  expect_equal(
    add_parameter("routes?route_type=0", "devid", fake_user_id),
    "routes?route_type=0&devid=1234567"
  )
})

test_that("Prefixing base URL and API version", {
  expect_equal(
    prefix_base_url_and_version("routes"),
    "http://timetableapi.ptv.vic.gov.au/v3/routes"
  )
  expect_equal(
    prefix_base_url_and_version("/routes"),
    "http://timetableapi.ptv.vic.gov.au/v3/routes"
  )
})

test_that("Signature is calculated correctly", {
  # The signature below was calculated user the tools at
  # http://timetableapi.ptv.vic.gov.au/swagger/ui/index#!/Routes/
  # While the user_id and api_key are fake, we should still be able to work out
  # the signature.
  expected_signature <- "E6C7BA1C2C083866BEDD09A85AE01DC31E34B2B5"
  request <- "routes"
  generated_signature <- sign_request(
    request = request,
    user_id = fake_user_id,
    api_key = fake_api_key
  )
  expect_equal(generated_signature, expected_signature)
})
