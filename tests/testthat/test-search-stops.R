ascot_vale <- search_stops("Ascot Vale")

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
        !grepl("Ascot Vale", stop_name, ignore.case = TRUE),
        !grepl("Ascot Vale", stop_suburb, ignore.case = TRUE)
      )
    ),
    0
  )
})
