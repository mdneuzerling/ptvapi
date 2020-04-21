all_routes <- routes()

test_that("Frankston train route can be found", {
  # I can't think of any non-train routes, but just in case
  frankston_train_routes <- all_routes %>%
    dplyr::filter(
      route_name == "Frankston",
      route_type == train_route_type
    )
  expect_equal(nrow(frankston_train_routes), 1)
})
