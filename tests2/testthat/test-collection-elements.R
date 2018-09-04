context("Collection elements")

test_that("Keys are recognized", {
  library(dplyr)
  df <- data.frame(a = 1:10, b = 2 * (1:10)) %>%
    collection_df()
  c_el <- change_key(df, key = "a")
  expect_identical(key(c_el), new_key("a"))
  expect_null(key(df)[[1]])
  f <- function(a, c) 2 * a
  expect_identical(key(f), new_key(c("a", "c")))
  key(f) <- "a"
  expect_identical(key(f), new_key("a"))
  expect_error(key(f) <- "b")
  expect_error(key(df) <- "c")
  key(c_el) <- "b"
  expect_true(names(c_el)[1] == "b")
  c_el <- mutate(c_el, c = 1)
  expect_error(key(df) <- "c")
  key(c_el) <- c("a", "c")
  expect_true("c" %in% key(c_el)[[1]])
  expect_true(is_collection_df(df))
  expect_message(key(1))
  a <- 1
  expect_error(key(a) <- "a")
})
