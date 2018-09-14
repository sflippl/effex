context("Test metaframe.R")

test_that("Building metaframes works", {
  mf <- new_metaframe(name = "column_name", label = "Column Name")
  expect_identical(mf, structure(
    data.frame(name = "column_name", label = "Column Name",
               stringsAsFactors = FALSE),
    class = c("metaframe", "data.frame")
  ))
  expect_true(is_metaframe(mf))
  expect_identical(as_metaframe(mf), mf)
  expect_error(metaframe(var_name = "color"))
})

test_that("Getting and setting metaframes works", {
  mf <- new_metaframe(name = "column_name", label = "Column Name")
  df <- data.frame(column_name = 1:10)
  expect_false(has_metaframe(df))
  df2 <- df
  attr(df2, "metaframe") <- mf
  metaframe(df) <- mf
  expect_true(has_metaframe(df))
  expect_identical(df, df2)
  expect_identical(metaframe(df), mf)
  expect_error(metaframe(1))
})

test_that("mutate_mf works", {
  df <- data.frame(column_name = 1:10)
  ex <-
    df %>%
    mutate_mf(tst = "a")
  expect_equal(metaframe(ex),
               data.frame(name = "column_name", tst = "a",
                          stringsAsFactors = FALSE) %>%
                 as_metaframe())
})
