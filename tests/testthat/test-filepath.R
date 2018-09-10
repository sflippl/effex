context("filepath")

test_that("Setting filepaths works", {
  flp <- new_filepath("tectr.json",
                      c("description", "notes"),
                      reader = function(file, ...)
                        jsonlite::read_json(file, simplifyDataFrame = TRUE) %>%
                        dplyr::as_tibble(.)
  )
  expect_true(is_filepath(flp))
  expect_equal(fields(flp), c("description", "notes"))
  expect_equal(reader(flp),
               function(file, ...)
                 jsonlite::read_json(file, simplifyDataFrame = TRUE) %>%
                 dplyr::as_tibble(.))
  tectr_df <- dplyr::tibble(name = "tectr",
    description = "A package.", notes = "Some notes")
  expect_equal(tectr_df, read_filepath(flp))
  expect_equal(as_filepath(flp), flp)
  flp2 <- flp
  class(flp2) <- "character"
  expect_equal(as_filepath(flp2), flp)
})

test_that("Subsetting filepaths works", {
  flp <- new_filepath("tectr.json",
                      c("description", "notes"),
                      reader = function(file, ...)
                        jsonlite::read_json(file, simplifyDataFrame = TRUE) %>%
                        dplyr::as_tibble(.)
  )
  expect_true(is_filepath(flp[1]))
})
