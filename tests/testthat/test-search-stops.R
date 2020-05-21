if (identical(Sys.getenv("NOT_CRAN"), "true")) {

ascot_vale <- search_stops("Ascot Vale")

test_that("search_stops result has class \"ptvapi\"", {
  expect_s3_class(ascot_vale, "ptvapi")
})

test_that("results in search_stops can relate to stop name alone", {
  expect_gte(
    nrow(
      dplyr::filter(
        ascot_vale,
        grepl("Ascot Vale", stop_name, ignore.case = TRUE),
        !grepl("Ascot Vale", stop_suburb, ignore.case = TRUE)
      )
    ),
    1
  )
})

test_that("results in search_stops can relate to stop suburb alone", {
  expect_gte(
    nrow(
      dplyr::filter(
        ascot_vale,
        !grepl("Ascot Vale", stop_name, ignore.case = TRUE),
        grepl("Ascot Vale", stop_suburb, ignore.case = TRUE)
      )
    ),
    1
  )
})

test_that("all results in search_stops relate to search term somehow", {
  expect_equal(
    nrow(
      dplyr::filter(
        ascot_vale,
        !grepl("Ascot Vale", stop_name
               , ignore.case = TRUE),
        !grepl("Ascot Vale", stop_suburb, ignore.case = TRUE)
      )
    ),
    0
  )
})

test_that("search_stops can be filtered with multiple route types", {
  expect_equal(
    search_stops("South Yarra", route_types = c(0, 1)) %>%
      pull(route_type) %>%
      unique %>%
      sort,
    c(0, 1)
  )
  expect_equal(
    search_stops("South Yarra", route_types = c(0, 2)) %>%
      pull(route_type) %>%
      unique %>%
      sort,
    c(0, 2)
  )
})
}
