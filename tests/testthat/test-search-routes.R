test_that("can find route number 11 with search_routes", {
  route_11 <- search_routes(11)
  expect_gte(
    nrow(dplyr::filter(route_11, route_number == 11)),
    1
  )
})

test_that("Can find Pakenham route with search_routes", {
  pakenham <- search_routes("pakenham")
  expect_gte(
    nrow(dplyr::filter(pakenham, route_name == "Pakenham Line")),
    1
  )
})

test_that("Errors when providing incomplete location arguments", {
  expect_error(
    search_routes("Bundoora", latitude = -37.818229),
    "If searching near a location, both latitude and longitude must be provided"
  )
  expect_error(
    search_routes("Bundoora", longitude = 144.952404),
    "If searching near a location, both latitude and longitude must be provided"
  )
  expect_error(
    search_routes("Bundoora", max_distance = 100),
    paste0(
      "Trying to limit search results with a maximum distance, but a latitude ",
      "and longitude wasn't provided"
    )
  )
})
