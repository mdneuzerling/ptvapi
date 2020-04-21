
# Testing route_directions
# Simply running this functions asserts that column names, etc. are as expected
frankston_route_directions <- route_directions(route_id = frankston_route_id)

city_directions <- frankston_route_directions %>%
  filter(grepl("City", direction_name, ignore.case = TRUE))

test_that("Frankston train goes to the city", {
  expect_equal(nrow(city_directions), 1)
})

frankston_directions <- frankston_route_directions %>%
  filter(grepl("Frankston", direction_name, ignore.case = TRUE))

test_that("Frankston train goes to the city", {
  expect_equal(nrow(frankston_directions), 1)
})

# Testing directions
# We'll run the Frankston -> City direction from above through the directions
# function, and see if we can recover the Frankston route ID.
recovering_frankston_direction <- city_directions$direction_id %>%
  directions(direction_id = .) %>%
  filter(
    grepl("Frankston", route_direction_description, ignore.case = TRUE),
    route_id == frankston_route_id
  )

test_that("Frankston route can be recovered with directions call", {
  expect_equal(nrow(recovering_frankston_direction), 1)
  expect_equal(recovering_frankston_direction$route_id, frankston_route_id)
})


# Testing directions with route type
test_that("Frankstain train directions are all train routes", {
  city_train_directions <- city_directions$direction_id %>%
    directions(direction_id = ., route_type = train_route_type)
  expect_true(
    all(
      grepl(
        "train",
        city_train_directions$route_direction_description,
        ignore.case = TRUE
      )
    )
  )
})
