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
