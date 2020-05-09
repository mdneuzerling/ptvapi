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

# The signature below was calculated user the tools at
# http://timetableapi.ptv.vic.gov.au/swagger/ui/index#!/Routes/
# While the user_id and api_key are fake, we should still be able to work out
# the signature.
fake_user_id <- 1234567
fake_api_key <- "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
request = "routes"
expected_signature <- "E6C7BA1C2C083866BEDD09A85AE01DC31E34B2B5"
input_error <- paste0( # note the regex escaping here
  "This function signs a request without a domain, version number, URL, or ",
  "devid\\/user_id\\. So if the request URL is ",
  "'http\\:\\/\\/timetableapi\\.ptv\\.vic\\.gov\\.au\\/v3\\/path\\?param",
  "\\=1&devid\\=1234567' ",
  "then this function will take as input 'path\\?param\\=1'\\."
)

test_that("Signature is calculated correctly on good input", {
  expect_equal(
    sign_request(
      request = request,
      user_id = fake_user_id,
      api_key = fake_api_key
    ),
    expected_signature
  )
})

test_that("Signing function rejects bad input", {
  for (bad_input in c(
    "http://www.example.com/path",
    "www.example.vic.gov.au/path",
    "path?devid=1234567",
    "v3/path"
  )) {
    expect_error(
      sign_request(!!bad_input, user_id = fake_user_id, api_key = fake_api_key),
      input_error
    )
  }
})

test_that("Signing function rejects requests that already have a signature", {
  expect_error(
    sign_request(
      "path?signature=ABCABCABC123123123",
      user_id = fake_user_id,
      api_key = fake_api_key
    ),
    "This request already has a signature"
  )
})

test_that("Can correctly suffix an individual parameter", {
  expect_equal(
    add_parameter("www.example.com", "animal1", "echidna"),
    "www.example.com?animal1=echidna"
  )
  expect_equal(
    add_parameter("www.example.com?animal1=echidna", "animal2", "ostrich"),
    "www.example.com?animal1=echidna&animal2=ostrich"
  )
})

# The rest of the tests will use the add_parameters function, which calls on the
# add_parameter function. All parameters should be suffixed with add_parameters.

test_that("Can add parameters either directly or with variables", {
  fat_goose <- "penguin"
  expect_equal(
    add_parameters("www.example.com", animal = fat_goose, food = "peanut"),
    "www.example.com?animal=penguin&food=peanut"
  )
})

test_that("Adding a NULL parameter will return request unaltered", {
  expect_equal(
    add_parameters("www.example.com", parameter = NULL),
    "www.example.com"
  )
  expect_equal(
    add_parameters("www.example.com", parameter = NULL, animal = "crocodile"),
    "www.example.com?animal=crocodile"
  )
  expect_equal(
    add_parameters("www.example.com", animal = "crocodile", parameter = NULL),
    "www.example.com?animal=crocodile"
  )
})

test_that("Adding a non-singleton parameter will error", {
  expect_error(
    add_parameters("www.example.com", parameter = c("fish", "giraffe")),
    "Parameters must be singletons"
  )
  expect_error(
    add_parameters("www.example.com", parameter = list(42, "giraffe")),
    "Parameters must be singletons"
  )
})

test_that("Adding an unnamed parameter will error", {
  expect_error(
    add_parameters("www.example.com", "giraffe"),
    "Parameters must be named"
  )
  expect_error(
    add_parameters("www.example.com", food = "peanut", "giraffe"),
    "Parameters must be named"
  )
})

test_that("Adding no parameters leaves request unchanges", {
  expect_equal(
    add_parameters("www.example.com"),
    "www.example.com"
  )
})

test_that("Can suffix multiple parameters at once", {
  expect_equal(
    add_parameters("example.com", para1 = "fish", para2 = "cow"),
    "example.com?para1=fish&para2=cow"
  )
})
