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
