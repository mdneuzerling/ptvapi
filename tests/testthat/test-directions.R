
# Simply running this functions asserts that column names, etc. are as expected
frankston_route_directions <- directions(frankston_route)

city_directions <- frankston_route_directions %>%
  filter(grepl("City", direction_name, ignore.case = TRUE))

test_that("Frankston train goes to the city", {
  expect_gte(nrow(city_directions), 1)
})

frankston_directions <- frankston_route_directions %>%
  filter(grepl("Frankston", direction_name, ignore.case = TRUE))

test_that("Frankston train goes to the city", {
  expect_gte(nrow(frankston_directions), 1)
})
