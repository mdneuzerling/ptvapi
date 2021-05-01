if (identical(Sys.getenv("NOT_CRAN"), "true")) {
# ---------------------------------------------------------------------------- #
# ---- Define values if they haven't already been defined by another test ---- #
# ---------------------------------------------------------------------------- #
if (!exists("all_routes")) {
  all_routes <- routes()
}

if (!exists("frankston_route_id")) {
  frankston_route_id <- all_routes %>%
    dplyr::filter(route_name == "Frankston") %>%
    pull(route_id)
}
# ---------------------------------------------------------------------------- #

single_route <- route_information(route_id = frankston_route_id)

test_that("route_information() result has class \"ptvapi\"", {
  expect_s3_class(single_route, "ptvapi")
})

test_that("we can query for a single route", {
  expect_equal(nrow(single_route), 1)
})

test_that("routes() result has class \"ptvapi\"", {
  expect_s3_class(all_routes, "ptvapi")
})

test_that("we can find the Frankston train route", {
  expect_true(length(frankston_route_id) == 1)
})

test_that("we can filter route results by name", {
  frankston_named_routes <- routes(route_name = "frank")
  expect_gt(nrow(frankston_named_routes), 0)
  expect_equal(
    nrow(dplyr::filter(
      frankston_named_routes,
      !grepl("frank", route_name, ignore.case = TRUE)
    )),
    0
  )
})

test_that("86 tram route can be found", {
  eighty_six_tram_routes <- all_routes %>%
    dplyr::filter(
      route_number == 86,
      route_type == translate_route_type("Tram")
    )
  expect_equal(nrow(eighty_six_tram_routes), 1)
})

test_that("we can filter routes by multiple route types", {
  expect_equal(
    routes(route_types = c(0, 1)) %>% pull(route_type) %>% unique %>% sort,
    c(0, 1)
  )
})

test_that("we can download geopath data with specific route", {
  frankston_with_geo <- route_information(
    frankston_route_id,
    include_geopath = TRUE,
    geopath_utc =
  )
  geopaths <- frankston_with_geo$geopath[[1]]
  # One path for each of the two directions
  expect_equal(nrow(geopaths), 2)
  expect_identical(
    names(geopaths),
    c("direction_id", "valid_from", "valid_to", "paths")
  )
})

}
