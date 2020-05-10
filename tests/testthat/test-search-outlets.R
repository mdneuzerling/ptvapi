st_kilda <- search_outlets("st kilda")

test_that("results in search_outlets can relate to outlet name alone", {
  expect_gte(
    nrow(
      dplyr::filter(
        st_kilda,
        grepl("st kilda", outlet_name, ignore.case = TRUE),
        !grepl("st kilda", outlet_business, ignore.case = TRUE),
        !grepl("st kilda", outlet_suburb, ignore.case = TRUE)
      )
    ),
    1
  )
})

test_that("results in search_outlets can relate to outlet business alone", {
  expect_gte(
    nrow(
      dplyr::filter(
        st_kilda,
        !grepl("st kilda", outlet_name, ignore.case = TRUE),
        grepl("st kilda", outlet_business, ignore.case = TRUE),
        !grepl("st kilda", outlet_suburb, ignore.case = TRUE)
      )
    ),
    1
  )
})

test_that("results in search_outlets can relate to outlet suburb alone", {
  expect_gte(
    nrow(
      dplyr::filter(
        st_kilda,
        !grepl("st kilda", outlet_name, ignore.case = TRUE),
        !grepl("st kilda", outlet_business, ignore.case = TRUE),
        grepl("st kilda", outlet_suburb, ignore.case = TRUE)
      )
    ),
    1
  )
})

test_that("all results in search_outlets relate to search term somehow", {
  expect_equal(
    nrow(
      dplyr::filter(
        st_kilda,
        !grepl("st kilda", outlet_name, ignore.case = TRUE),
        !grepl("st kilda", outlet_business, ignore.case = TRUE),
        !grepl("st kilda", outlet_suburb, ignore.case = TRUE)
      )
    ),
    0
  )
})

test_that("search_outlets filtered by max_distance", {
  # We'll try to find a 7-Eleven between 500 and 1000 metres from Southern Cross
  max_500 <- search_outlets(
    "7-Eleven",
    latitude = -37.818229,
    longitude = 144.952404,
    max_distance = 500
  )
  max_1000 <- search_outlets(
    "7-Eleven",
    latitude = -37.818229,
    longitude = 144.952404,
    max_distance = 1000
  )
  difference <- max_1000 %>% anti_join(max_500, by = "outlet_name")
  expect_gt(nrow(difference), 0)
})
