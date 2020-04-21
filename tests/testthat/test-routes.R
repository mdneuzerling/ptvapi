all_routes <- routes()

test_that("Frankston train route can be found", {
  # I can't think of any non-train routes, but just in case
  frankston_train_routes <- all_routes %>%
    dplyr::filter(
      route_name == "Frankston",
      route_type == translate_route_type("Train")
    )
  expect_equal(nrow(frankston_train_routes), 1)
})

test_that("86 tram route can be found", {
  eighty_six_tram_routes <- all_routes %>%
    dplyr::filter(
      route_number == 86,
      route_type == translate_route_type("Tram")
    )
  expect_equal(nrow(eighty_six_tram_routes), 1)
})

test_that("Can identify individual route", {
  craigieburn_train_route_id <- all_routes %>%
    filter(
      grepl("Craigieburn", route_name),
      route_type == translate_route_type("Train")
    ) %>%
    pull(route_id)
  craigieburn_route <- routes(route_id = craigieburn_train_route_id)
  expect_equal(nrow(craigieburn_route), 1)
})
